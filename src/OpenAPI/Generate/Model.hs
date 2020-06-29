{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Defines functionality for the generation of models from OpenAPI schemas
module OpenAPI.Generate.Model
  ( getSchemaType,
    resolveSchemaReferenceWithoutWarning,
    getConstraintDescriptionsOfSchema,
    defineModelForSchemaNamed,
    defineModelForSchema,
    TypeWithDeclaration,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Calendar
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified OpenAPI.Common as OC
import OpenAPI.Generate.Doc (appendDoc, emptyDoc)
import qualified OpenAPI.Generate.Doc as Doc
import qualified OpenAPI.Generate.Flags as OAF
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS
import Prelude hiding (maximum, minimum, not)

-- | The type of a model and the declarations needed for defining it
type TypeWithDeclaration = (Q Type, Dep.ModelContentWithDependencies)

type BangTypesSelfDefined = (Q [VarBangType], Q Doc, Dep.Models)

data TypeAliasStrategy = CreateTypeAlias | DontCreateTypeAlias
  deriving (Show, Eq, Ord)

addDependencies :: Dep.Models -> OAM.Generator TypeWithDeclaration -> OAM.Generator TypeWithDeclaration
addDependencies dependenciesToAdd typeDef = do
  (type', (content, dependencies)) <- typeDef
  pure (type', (content, Set.union dependencies dependenciesToAdd))

-- | default derive clause for the objects
objectDeriveClause :: [Q DerivClause]
objectDeriveClause =
  [ derivClause
      Nothing
      [ conT ''Show,
        conT ''Eq
      ]
  ]

-- | Defines all the models for a schema
defineModelForSchema :: Text -> OAS.Schema -> OAM.Generator Dep.ModelWithDependencies
defineModelForSchema schemaName schema = do
  namedSchema <- defineModelForSchemaNamedWithTypeAliasStrategy CreateTypeAlias schemaName schema
  pure (transformToModuleName schemaName, snd namedSchema)

-- | Defines all the models for a schema and returns the declarations with the type of the root model
defineModelForSchemaNamed :: Text -> OAS.Schema -> OAM.Generator TypeWithDeclaration
defineModelForSchemaNamed = defineModelForSchemaNamedWithTypeAliasStrategy DontCreateTypeAlias

-- | defines the definitions for a schema and returns a type to the "entrypoint" of the schema
defineModelForSchemaNamedWithTypeAliasStrategy :: TypeAliasStrategy -> Text -> OAS.Schema -> OAM.Generator TypeWithDeclaration
defineModelForSchemaNamedWithTypeAliasStrategy strategy schemaName schema = OAM.nested schemaName $
  case schema of
    OAT.Concrete concrete -> defineModelForSchemaConcrete strategy schemaName concrete
    OAT.Reference reference -> do
      refName <- haskellifyNameM True $ getSchemaNameFromReference reference
      OAM.logInfo $ "Reference " <> reference <> " to " <> T.pack (nameBase refName)
      pure (varT refName, (emptyDoc, transformReferenceToDependency reference))

getSchemaNameFromReference :: Text -> Text
getSchemaNameFromReference = T.replace "#/components/schemas/" ""

transformReferenceToDependency :: Text -> Set.Set Text
transformReferenceToDependency = Set.singleton . transformToModuleName . getSchemaNameFromReference

-- | Transforms a 'OAS.Schema' (either a reference or a concrete object) to @'Maybe' 'OAS.SchemaObject'@
-- If a reference is found it is resolved. If it is not found, no log message is generated.
resolveSchemaReferenceWithoutWarning :: OAS.Schema -> OAM.Generator (Maybe OAS.SchemaObject)
resolveSchemaReferenceWithoutWarning schema =
  case schema of
    OAT.Concrete concrete -> pure $ Just concrete
    OAT.Reference ref -> OAM.getSchemaReferenceM ref

resolveSchemaReference :: Text -> OAS.Schema -> OAM.Generator (Maybe (OAS.SchemaObject, Dep.Models))
resolveSchemaReference schemaName schema =
  OAM.nested schemaName $
    case schema of
      OAT.Concrete concrete -> pure $ Just (concrete, Set.empty)
      OAT.Reference ref -> do
        p <- OAM.getSchemaReferenceM ref
        when (Maybe.isNothing p) $ OAM.logWarning $
          "Reference " <> ref <> " to SchemaObject from "
            <> schemaName
            <> " could not be found and therefore will be skipped."
        pure $ (,transformReferenceToDependency ref) <$> p

-- | creates an alias depending on the strategy
createAlias :: Text -> Text -> TypeAliasStrategy -> OAM.Generator TypeWithDeclaration -> OAM.Generator TypeWithDeclaration
createAlias schemaName description strategy res = do
  schemaName' <- haskellifyNameM True schemaName
  (type', (content, dependencies)) <- res
  pure $ case strategy of
    CreateTypeAlias ->
      ( type',
        ( content
            `appendDoc` ( ( Doc.generateHaddockComment
                              [ "Defines an alias for the schema " <> Doc.escapeText schemaName,
                                "",
                                description
                              ]
                              $$
                          )
                            . ppr <$> tySynD schemaName' [] type'
                        ),
          dependencies
        )
      )
    DontCreateTypeAlias -> (type', (content, dependencies))

-- | returns the type of a schema. Second return value is a 'Q' Monad, for the types that have to be created
defineModelForSchemaConcrete :: TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator TypeWithDeclaration
defineModelForSchemaConcrete strategy schemaName schema =
  let enumValues = OAS.enum schema
   in if null enumValues
        then defineModelForSchemaConcreteIgnoreEnum strategy schemaName schema
        else defineEnumModel strategy schemaName schema enumValues

-- | Creates a Model, ignores enum values
defineModelForSchemaConcreteIgnoreEnum :: TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator TypeWithDeclaration
defineModelForSchemaConcreteIgnoreEnum strategy schemaName schema = do
  flags <- OAM.getFlags
  let schemaDescription = getDescriptionOfSchema schema
      typeAliasing = createAlias schemaName schemaDescription strategy
  case schema of
    OAS.SchemaObject {type' = OAS.SchemaTypeArray, ..} -> defineArrayModelForSchema strategy schemaName schema
    OAS.SchemaObject {type' = OAS.SchemaTypeObject, ..} ->
      let allOfNull = Set.null $ OAS.allOf schema
          oneOfNull = Set.null $ OAS.oneOf schema
          anyOfNull = Set.null $ OAS.anyOf schema
       in case (allOfNull, oneOfNull, anyOfNull) of
            (False, _, _) -> defineAllOfSchema schemaName schemaDescription $ Set.toList $ OAS.allOf schema
            (_, False, _) -> typeAliasing $ defineOneOfSchema schemaName schemaDescription $ Set.toList $ OAS.oneOf schema
            (_, _, False) -> defineAnyOfSchema strategy schemaName schemaDescription $ Set.toList $ OAS.anyOf schema
            _ -> defineObjectModelForSchema strategy schemaName schema
    _ ->
      typeAliasing $ pure (varT $ getSchemaType flags schema, (emptyDoc, Set.empty))

defineEnumModel :: TypeAliasStrategy -> Text -> OAS.SchemaObject -> Set.Set Aeson.Value -> OAM.Generator TypeWithDeclaration
defineEnumModel strategy schemaName schema enumValuesSet = do
  OAM.logInfo (T.pack "Generate Enum " <> schemaName)
  let enumValues = Set.toList enumValuesSet
      getConstructor (a, _, _) = a
      showValue (Aeson.Number a) = case Scientific.toBoundedInteger a :: Maybe Int of
        Just num -> show num
        Nothing -> show a
      showValue a = show a
      getValueInfo value = do
        cname <- haskellifyNameM True (schemaName <> T.pack "Enum" <> T.replace "\"" "" (T.pack (showValue value)))
        pure (normalC cname [], cname, value)
  name <- haskellifyNameM True schemaName
  (typ, (content, dependencies)) <- defineModelForSchemaConcreteIgnoreEnum strategy (schemaName <> "EnumValue") schema
  constructorsInfo <- mapM getValueInfo enumValues
  otherName <- haskellifyNameM True $ schemaName <> "EnumOther"
  typedName <- haskellifyNameM True $ schemaName <> "EnumTyped"
  let nameValuePairs = fmap (\(_, a, b) -> (a, b)) constructorsInfo
      toBangType t = do
        ban <- bang noSourceUnpackedness noSourceStrictness
        banT <- t
        pure (ban, banT)
      otherC = normalC otherName [toBangType (varT ''Aeson.Value)]
      typedC = normalC typedName [toBangType typ]
      jsonImplementation = defineJsonImplementationForEnum name otherName [otherName, typedName] nameValuePairs
      newType =
        ( Doc.generateHaddockComment
            [ "Defines the enum schema " <> Doc.escapeText schemaName,
              "",
              getDescriptionOfSchema schema
            ]
            $$
        )
          . ppr
          <$> dataD
            (pure [])
            name
            []
            Nothing
            (otherC : typedC : (getConstructor <$> constructorsInfo))
            objectDeriveClause
  pure (varT name, (content `appendDoc` newType `appendDoc` jsonImplementation, dependencies))

defineJsonImplementationForEnum :: Name -> Name -> [Name] -> [(Name, Aeson.Value)] -> Q Doc
defineJsonImplementationForEnum name fallbackName specialCons nameValues =
  -- without this function, a N long string takes up N lines, as every
  -- new character starts on a new line
  let nicifyValue (Aeson.String a) = [|$(litE $ stringL $ T.unpack a)|]
      nicifyValue a = [|a|]
      fnArgName = mkName "val"
      fromJsonCases =
        multiIfE $
          fmap
            ( \(name', value) -> normalGE [|$(varE fnArgName) == $(nicifyValue value)|] (varE name')
            )
            nameValues
            <> [normalGE [|otherwise|] [|$(varE fallbackName) $(varE fnArgName)|]]
      fromJsonFn =
        funD
          (mkName "parseJSON")
          [clause [varP fnArgName] (normalB [|pure $fromJsonCases|]) []]
      fromJson = instanceD (cxt []) [t|Aeson.FromJSON $(varT name)|] [fromJsonFn]
      toJsonClause (name', value) =
        let jsonValue = Aeson.toJSON value
         in clause [conP name' []] (normalB $ nicifyValue jsonValue) []
      toSpecialCons name' =
        clause
          [conP name' [varP $ mkName "patternName"]]
          (normalB [|Aeson.toJSON $(varE (mkName "patternName"))|])
          []
      toJsonFn =
        funD
          (mkName "toJSON")
          ((toSpecialCons <$> specialCons) <> (toJsonClause <$> nameValues))
      toJson = instanceD (cxt []) [t|Aeson.ToJSON $(varT name)|] [toJsonFn]
   in fmap ppr toJson `appendDoc` fmap ppr fromJson

-- | defines anyOf types
--
-- If the subschemas consist only of objects an allOf type without any required field can be generated
-- If there are differen subschema types, per schematype a oneOf is generated
defineAnyOfSchema :: TypeAliasStrategy -> Text -> Text -> [OAS.Schema] -> OAM.Generator TypeWithDeclaration
defineAnyOfSchema strategy schemaName description schemas = do
  OAM.logInfo $ T.pack "defineAnyOfSchema " <> schemaName
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      schemasWithoutRequired = fmap (\o -> o {OAS.required = Set.empty}) concreteSchemas
      notObjectSchemas = filter (\o -> OAS.type' o /= OAS.SchemaTypeObject) concreteSchemas
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  if null notObjectSchemas
    then addDependencies newDependencies $ defineAllOfSchema schemaName description (fmap OAT.Concrete schemasWithoutRequired)
    else createAlias schemaName description strategy $ defineOneOfSchema schemaName description schemas

--    this would be the correct implementation
--    but it generates endless loop because some implementations use anyOf as a oneOf
--    where the schema reference itself
--      let objectSchemas = filter (\o -> OAS.type' o == OAS.SchemaTypeObject) concreteSchemas
--      (propertiesCombined, _) <- fuseSchemasAllOf schemaName (fmap OAT.Concrete objectSchemas)
--      if null propertiesCombined then
--        createAlias schemaName strategy $ defineOneOfSchema schemaName schemas
--        else
--          let schemaPrototype = head objectSchemas
--              newSchema = schemaPrototype {OAS.properties = propertiesCombined, OAS.required = Set.empty}
--          in
--            createAlias schemaName strategy $ defineOneOfSchema schemaName (fmap OAT.Concrete (newSchema : notObjectSchemas))

-- | defines a OneOf Schema
--
-- creates types for all the subschemas and then creates an adt with constructors for the different
-- subschemas. Constructors are numbered
defineOneOfSchema :: Text -> Text -> [OAS.Schema] -> OAM.Generator TypeWithDeclaration
defineOneOfSchema schemaName description schemas = do
  if null schemas
    then OAM.logWarning "schemas are empty, can not create OneOfSchemas"
    else OAM.logInfo $ "define oneOf Model " <> schemaName
  flags <- OAM.getFlags
  let indexedSchemas = zip schemas ([1 ..] :: [Integer])
      defineIndexed schema index = defineModelForSchemaNamed (schemaName <> "OneOf" <> T.pack (show index)) schema
  variants <- mapM (uncurry defineIndexed) indexedSchemas
  let variantDefinitions = vcat <$> mapM (fst . snd) variants
      dependencies = Set.unions $ fmap (snd . snd) variants
      types = fmap fst variants
      indexedTypes = zip types ([1 ..] :: [Integer])
      getConstructorName (typ, n) = do
        t <- typ
        let suffix = if OAF.optUseNumberedVariantConstructors flags then "Variant" <> T.pack (show n) else typeToSuffix t
        pure $ haskellifyName (OAF.optConvertToCamelCase flags) True $ schemaName <> suffix
      constructorNames = fmap getConstructorName indexedTypes
      createTypeConstruct (typ, n) = do
        t <- typ
        bang' <- bang noSourceUnpackedness noSourceStrictness
        haskellifiedName <- getConstructorName (typ, n)
        normalC haskellifiedName [pure (bang', t)]
      emptyCtx = pure []
      name = haskellifyName (OAF.optConvertToCamelCase flags) True $ schemaName <> "Variants"
      patternName = mkName "a"
      p = varP patternName
      e = varE patternName
      fromJsonFn =
        let paramName = mkName "val"
            body = do
              constructorNames' <- sequence constructorNames
              let resultExpr =
                    foldr
                      ( \constructorName expr ->
                          [|($(varE constructorName) <$> Aeson.fromJSON $(varE paramName)) <|> $expr|]
                      )
                      [|Aeson.Error "No variant matched"|]
                      constructorNames'
              [|
                case $resultExpr of
                  Aeson.Success $p -> pure $e
                  Aeson.Error $p -> fail $e
                |]
         in funD
              (mkName "parseJSON")
              [ clause
                  [varP paramName]
                  (normalB body)
                  []
              ]
      toJsonFn =
        funD
          (mkName "toJSON")
          ( fmap
              ( \constructorName -> do
                  n <- constructorName
                  clause
                    [conP n [p]]
                    (normalB [|Aeson.toJSON $e|])
                    []
              )
              constructorNames
          )
      dataDefinition =
        ( Doc.generateHaddockComment
            [ "Define the one-of schema " <> Doc.escapeText schemaName,
              "",
              description
            ]
            $$
        )
          . ppr
          <$> dataD
            emptyCtx
            name
            []
            Nothing
            (createTypeConstruct <$> indexedTypes)
            [ derivClause
                Nothing
                [ conT ''Show,
                  conT ''Eq
                ]
            ]
      toJson = ppr <$> instanceD emptyCtx [t|Aeson.ToJSON $(varT name)|] [toJsonFn]
      fromJson = ppr <$> instanceD emptyCtx [t|Aeson.FromJSON $(varT name)|] [fromJsonFn]
      innerRes = (varT name, (variantDefinitions `appendDoc` dataDefinition `appendDoc` toJson `appendDoc` fromJson, dependencies))
  pure innerRes

typeToSuffix :: Type -> Text
typeToSuffix (ConT name') = T.pack $ nameBase name'
typeToSuffix (VarT name') =
  let x = T.pack $ nameBase name'
   in if x == "[]" then "List" else x
typeToSuffix (AppT type1 type2) = typeToSuffix type1 <> typeToSuffix type2
typeToSuffix x = T.pack $ show x

-- | combines schemas so that it is usefull for a allOf fusion
fuseSchemasAllOf :: Text -> [OAS.Schema] -> OAM.Generator (Map.Map Text OAS.Schema, Set.Set Text)
fuseSchemasAllOf schemaName schemas = do
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
  subSchemaInformation <- mapM (getPropertiesForAllOf schemaName) concreteSchemas
  let propertiesCombined = foldl (Map.unionWith const) Map.empty (fmap fst subSchemaInformation)
  let requiredCombined = foldl Set.union Set.empty (fmap snd subSchemaInformation)
  pure (propertiesCombined, requiredCombined)

-- | gets properties for an allOf merge
-- looks if subschemas define further subschemas
getPropertiesForAllOf :: Text -> OAS.SchemaObject -> OAM.Generator (Map.Map Text OAS.Schema, Set.Set Text)
getPropertiesForAllOf schemaName schema =
  let allOf = OAS.allOf schema
      anyOf = OAS.anyOf schema
      relevantSubschemas = Set.union allOf anyOf
   in if null relevantSubschemas
        then pure (OAS.properties schema, OAS.required schema)
        else do
          (allOfProps, allOfRequired) <- fuseSchemasAllOf schemaName $ Set.toList allOf
          (anyOfProps, _) <- fuseSchemasAllOf schemaName $ Set.toList anyOf
          pure (Map.unionWith const allOfProps anyOfProps, allOfRequired)

-- | defines a allOf subschema
-- Fuses the subschemas together
defineAllOfSchema :: Text -> Text -> [OAS.Schema] -> OAM.Generator TypeWithDeclaration
defineAllOfSchema schemaName description schemas = do
  newDefs <- defineNewSchemaForAllOf schemaName description schemas
  case newDefs of
    Just (newSchema, newDependencies) ->
      addDependencies newDependencies $ defineModelForSchemaConcrete DontCreateTypeAlias schemaName newSchema
    Nothing -> pure (varT ''Text, (emptyDoc, Set.empty))

-- | defines a new Schema, which properties are fused
defineNewSchemaForAllOf :: Text -> Text -> [OAS.Schema] -> OAM.Generator (Maybe (OAS.SchemaObject, Dep.Models))
defineNewSchemaForAllOf schemaName description schemas = do
  OAM.logInfo $ "define allOf Model " <> schemaName
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  (propertiesCombined, requiredCombined) <- fuseSchemasAllOf schemaName schemas
  if Map.null propertiesCombined
    then do
      OAM.logWarning "allOf schemas is empty"
      pure Nothing
    else
      let schemaPrototype = head concreteSchemas
          newSchema = schemaPrototype {OAS.properties = propertiesCombined, OAS.required = requiredCombined, OAS.description = Just description}
       in pure $ Just (newSchema, newDependencies)

-- | defines an array
defineArrayModelForSchema :: TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator TypeWithDeclaration
defineArrayModelForSchema strategy schemaName schema = do
  (type', (content, dependencies)) <-
    case OAS.items schema of
      Just itemSchema -> defineModelForSchemaNamed schemaName itemSchema
      -- not allowed by the spec
      Nothing -> do
        OAM.logWarning $ T.pack "items is empty for an array (assume string) " <> schemaName
        pure (varT ''Text, (emptyDoc, Set.empty))
  let arrayType = appT listT type'
  schemaName' <- haskellifyNameM True schemaName
  pure
    ( arrayType,
      ( content `appendDoc` case strategy of
          CreateTypeAlias ->
            ( Doc.generateHaddockComment
                [ "Defines an alias for the schema " <> Doc.escapeText schemaName,
                  "",
                  getDescriptionOfSchema schema
                ]
                $$
            )
              . ppr
              <$> tySynD schemaName' [] arrayType
          DontCreateTypeAlias -> emptyDoc,
        dependencies
      )
    )

-- | Defines a Record
defineObjectModelForSchema :: TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator TypeWithDeclaration
defineObjectModelForSchema strategy schemaName schema =
  if OAS.isSchemaEmpty schema
    then createAlias schemaName (getDescriptionOfSchema schema) strategy $ pure ([t|Aeson.Object|], (Doc.emptyDoc, Set.empty))
    else do
      flags <- OAM.getFlags
      let convertToCamelCase = OAF.optConvertToCamelCase flags
          name = haskellifyName convertToCamelCase True schemaName
          props = Map.toList $ OAS.properties schema
          propsWithNames = zip (fmap fst props) $ fmap (haskellifyName convertToCamelCase False . (schemaName <>) . uppercaseFirstText . fst) props
          emptyCtx = pure []
          required = OAS.required schema
      OAM.logInfo $ "define object model " <> T.pack (nameBase name)
      (bangTypes, propertyContent, propertyDependencies) <- propertiesToBangTypes schemaName props required
      propertyDescriptions <- getDescriptionOfProperties props
      let dataDefinition :: Q Doc
          dataDefinition = do
            bangs <- bangTypes
            let record = recC name (pure <$> bangs)
            flip Doc.zipCodeAndComments propertyDescriptions
              . T.lines
              . T.pack
              . show
              . Doc.breakOnTokensWithReplacement
                ( \case
                    "{" -> "{\n  "
                    token -> "\n  " <> token
                )
                [",", "{", "}"]
              . ppr <$> dataD emptyCtx name [] Nothing [record] objectDeriveClause
          toJsonInstance = createToJSONImplementation name propsWithNames
          fromJsonInstance = createFromJSONImplementation name propsWithNames required
      pure
        ( varT name,
          ( pure
              ( Doc.generateHaddockComment
                  [ "Defines the data type for the schema " <> Doc.escapeText schemaName,
                    "",
                    getDescriptionOfSchema schema
                  ]
              )
              `appendDoc` dataDefinition
              `appendDoc` toJsonInstance
              `appendDoc` fromJsonInstance
              `appendDoc` propertyContent,
            propertyDependencies
          )
        )

-- | create toJSON implementation for an object
createToJSONImplementation :: Name -> [(Text, Name)] -> Q Doc
createToJSONImplementation objectName recordNames =
  let emptyDefs = pure []
      fnArgName = mkName "obj"
      toAssertion (jsonName, hsName) =
        [|
          $(varE $ mkName "Data.Aeson..=")
            $(litE $ stringL $ T.unpack jsonName)
            ($(varE hsName) $(varE fnArgName))
          |]
      toExprList :: [Q Exp] -> Q Exp
      toExprList [] = [|[]|]
      toExprList (x : xs) = uInfixE x (varE $ mkName ":") (toExprList xs)
      toExprCombination :: [Q Exp] -> Q Exp
      toExprCombination [] = [|[]|]
      toExprCombination [x] = x
      toExprCombination (x : xs) = [|$(x) <> $(toExprCombination xs)|]
      defaultJsonImplementation :: [DecQ]
      defaultJsonImplementation =
        if null recordNames
          then
            [ funD
                (mkName "toJSON")
                [ clause
                    [varP fnArgName]
                    ( normalB
                        [|
                          $(varE $ mkName "Data.Aeson.object") []
                          |]
                    )
                    []
                ],
              funD
                (mkName "toEncoding")
                [ clause
                    [varP fnArgName]
                    ( normalB
                        [|
                          $(varE $ mkName "Data.Aeson.pairs")
                            ($(varE $ mkName "Data.Aeson..=") "string" ("string" :: String))
                          |]
                    )
                    []
                ]
            ]
          else
            [ funD
                (mkName "toJSON")
                [ clause
                    [varP fnArgName]
                    ( normalB
                        [|
                          $(varE $ mkName "Data.Aeson.object")
                            $(toExprList $ toAssertion <$> recordNames)
                          |]
                    )
                    []
                ],
              funD
                (mkName "toEncoding")
                [ clause
                    [varP fnArgName]
                    ( normalB
                        [|
                          $(varE $ mkName "Data.Aeson.pairs")
                            $(toExprCombination $ toAssertion <$> recordNames)
                          |]
                    )
                    []
                ]
            ]
   in ppr <$> instanceD emptyDefs [t|Aeson.ToJSON $(varT objectName)|] defaultJsonImplementation

-- | create FromJSON implementation for an object
createFromJSONImplementation :: Name -> [(Text, Name)] -> Set.Set Text -> Q Doc
createFromJSONImplementation objectName recordNames required =
  let fnArgName = mkName "obj"
      withObjectLamda =
        foldl
          ( \prev (propName, _) ->
              let propName' = litE $ stringL $ T.unpack propName
                  arg = varE fnArgName
                  readPropE =
                    if propName `elem` required
                      then [|$arg Aeson..: $propName'|]
                      else [|$arg Aeson..:? $propName'|]
               in [|$prev <*> $readPropE|]
          )
          [|pure $(varE objectName)|]
          recordNames
   in ppr
        <$> instanceD
          (cxt [])
          [t|Aeson.FromJSON $(varT objectName)|]
          [ funD
              (mkName "parseJSON")
              [ clause
                  []
                  ( normalB
                      [|Aeson.withObject $(litE $ stringL $ show objectName) $(lam1E (varP fnArgName) withObjectLamda)|]
                  )
                  []
              ]
          ]

-- | create "bangs" record fields for properties
propertiesToBangTypes :: Text -> [(Text, OAS.Schema)] -> Set.Set Text -> OAM.Generator BangTypesSelfDefined
propertiesToBangTypes _ [] _ = pure (pure [], emptyDoc, Set.empty)
propertiesToBangTypes schemaName props required = do
  flags <- OAM.getFlags
  let propertySuffix = T.pack $ OAF.optPropertyTypeSuffix flags
  let createBang :: Text -> Text -> Q Type -> OAM.Generator (Q VarBangType)
      createBang recordName propName myType =
        let qVar :: Q VarBangType
            qVar = do
              bang' <- bang noSourceUnpackedness noSourceStrictness
              type' <-
                if recordName `elem` required
                  then myType
                  else appT (varT ''Maybe) myType
              pure (haskellifyName (OAF.optConvertToCamelCase flags) False propName, bang', type')
         in pure qVar
      propToBangType :: (Text, OAS.Schema) -> OAM.Generator (Q VarBangType, Q Doc, Dep.Models)
      propToBangType (recordName, schema) = do
        let propName = schemaName <> uppercaseFirstText recordName
        (myType, (content, depenencies)) <- defineModelForSchemaNamed (propName <> propertySuffix) schema
        myBang <- createBang recordName propName myType
        pure (myBang, content, depenencies)
      foldFn :: OAM.Generator BangTypesSelfDefined -> (Text, OAS.Schema) -> OAM.Generator BangTypesSelfDefined
      foldFn accHolder next = do
        (varBang, content, dependencies) <- accHolder
        (nextVarBang, nextContent, nextDependencies) <- propToBangType next
        pure
          ( varBang `liftedAppend` fmap pure nextVarBang,
            content `appendDoc` nextContent,
            Set.union dependencies nextDependencies
          )
  foldl foldFn (pure (pure [], emptyDoc, Set.empty)) props

getDescriptionOfSchema :: OAS.SchemaObject -> Text
getDescriptionOfSchema schema = Doc.escapeText $ Maybe.fromMaybe "" $ OAS.description schema

getDescriptionOfProperties :: [(Text, OAS.Schema)] -> OAM.Generator [Text]
getDescriptionOfProperties =
  mapM
    ( \(name, schema) -> do
        schema' <- resolveSchemaReferenceWithoutWarning schema
        let description = maybe "" (": " <>) $ schema' >>= OAS.description
            constraints = T.unlines $ ("* " <>) <$> getConstraintDescriptionsOfSchema schema'
        pure $ Doc.escapeText $ name <> description <> (if T.null constraints then "" else "\n\nConstraints:\n\n" <> constraints)
    )

-- | Extracts the constraints of a 'OAS.SchemaObject' as human readable text
getConstraintDescriptionsOfSchema :: Maybe OAS.SchemaObject -> [Text]
getConstraintDescriptionsOfSchema schema =
  let showConstraint desc = showConstraintSurrounding desc ""
      showConstraintSurrounding prev after = fmap $ (prev <>) . (<> after) . T.pack . show
      exclusiveMaximum = maybe False OAS.exclusiveMaximum schema
      exclusiveMinimum = maybe False OAS.exclusiveMinimum schema
   in Maybe.catMaybes
        [ showConstraint "Must be a multiple of " $ schema >>= OAS.multipleOf,
          showConstraint ("Maxium " <> if exclusiveMaximum then " (exclusive)" else "" <> " of ") $ schema >>= OAS.maximum,
          showConstraint ("Minimum " <> if exclusiveMinimum then " (exclusive)" else "" <> " of ") $ schema >>= OAS.minimum,
          showConstraint "Maximum length of " $ schema >>= OAS.maxLength,
          showConstraint "Minimum length of " $ schema >>= OAS.minLength,
          ("Must match pattern '" <>) . (<> "'") <$> (schema >>= OAS.pattern'),
          showConstraintSurrounding "Must have a maximum of " " items" $ schema >>= OAS.maxItems,
          showConstraintSurrounding "Must have a minimum of " " items" $ schema >>= OAS.minItems,
          schema
            >>= ( \case
                    True -> Just "Must have unique items"
                    False -> Nothing
                )
            . OAS.uniqueItems,
          showConstraintSurrounding "Must have a maximum of " " properties" $ schema >>= OAS.maxProperties,
          showConstraintSurrounding "Must have a minimum of " " properties" $ schema >>= OAS.minProperties
        ]

-- | Extracts the 'Name' of a 'OAS.SchemaObject' which should be used for primitive types
getSchemaType :: OAF.Flags -> OAS.SchemaObject -> Name
getSchemaType OAF.Flags {optUseIntWithArbitraryPrecision = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeInteger, ..} = ''Integer
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, format = Just "int32", ..} = ''Int.Int32
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, format = Just "int64", ..} = ''Int.Int64
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeInteger, ..} = ''Int
getSchemaType OAF.Flags {optUseFloatWithArbitraryPrecision = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeNumber, ..} = ''Scientific.Scientific
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, format = Just "float", ..} = ''Float
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, format = Just "double", ..} = ''Double
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeNumber, ..} = ''Double
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "byte", ..} = ''OC.JsonByteString
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "binary", ..} = ''OC.JsonByteString
getSchemaType OAF.Flags {optUseDateTypesAsString = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "date", ..} = ''Day
getSchemaType OAF.Flags {optUseDateTypesAsString = True, ..} OAS.SchemaObject {type' = OAS.SchemaTypeString, format = Just "date-time", ..} = ''OC.JsonDateTime
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeString, ..} = ''Text
getSchemaType _ OAS.SchemaObject {type' = OAS.SchemaTypeBool, ..} = ''Bool
getSchemaType _ OAS.SchemaObject {..} = ''Text
