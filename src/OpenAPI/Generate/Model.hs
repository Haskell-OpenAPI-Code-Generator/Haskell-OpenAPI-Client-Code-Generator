{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module OpenAPI.Generate.Model where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import OpenAPI.Common as OC
import OpenAPI.Generate.Doc (appendDoc, emptyDoc)
import qualified OpenAPI.Generate.Flags as OAF
import OpenAPI.Generate.Internal.Model
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Monad as OAM
import OpenAPI.Generate.Types.Referencable
import qualified OpenAPI.Generate.Types.Schema as OAS
import qualified OpenAPI.Generate.Util as Util

data TypeAliasStrategy = CreateTypeAlias | DontCreateTypeAlias
  deriving (Show, Eq, Ord)

type NamedTypeDef = (Q Type, ModelContentWithDependencies)

type BangTypesSelfDefined = (Q [VarBangType], Q Doc, Models)

addDependencies :: Models -> OAM.Generator NamedTypeDef -> OAM.Generator NamedTypeDef
addDependencies dependenciesToAdd typeDef = do
  (type', (content, dependencies)) <- typeDef
  pure (type', (content, Set.union dependencies dependenciesToAdd))

-- | defines all the models for a schema
defineModelForSchema :: T.Text -> OAS.Schema -> OAM.Generator ModelWithDependencies
defineModelForSchema schemaName schema = do
  namedSchema <- defineModelForSchemaNamedWithTypeAliasStrategy CreateTypeAlias schemaName schema
  pure (transformToModuleName schemaName, snd namedSchema)

defineModelForSchemaNamed :: T.Text -> OAS.Schema -> OAM.Generator NamedTypeDef
defineModelForSchemaNamed = defineModelForSchemaNamedWithTypeAliasStrategy DontCreateTypeAlias

-- | defines the definitions for a schema and returns a type to the "entrypoint" of the schema
defineModelForSchemaNamedWithTypeAliasStrategy :: TypeAliasStrategy -> T.Text -> OAS.Schema -> OAM.Generator NamedTypeDef
defineModelForSchemaNamedWithTypeAliasStrategy strategy schemaName schema = OAM.nested schemaName $
  case schema of
    Concrete concrete -> defineModelForSchemaConcrete strategy schemaName concrete
    Reference reference -> do
      let originalName = T.replace "#/components/schemas/" "" reference
      refName <- haskellifyNameM True originalName
      OAM.logInfo $ "Reference " <> reference <> " to " <> T.pack (nameBase refName)
      pure (varT refName, (emptyDoc, Set.singleton $ transformToModuleName originalName))

resolveSchemaReference :: T.Text -> OAS.Schema -> OAM.Generator (Maybe (OAS.SchemaObject, Models))
resolveSchemaReference schemaName schema =
  OAM.nested schemaName $
    case schema of
      Concrete concrete -> pure $ Just (concrete, Set.empty)
      Reference ref -> do
        p <- OAM.getSchemaReferenceM ref
        when (Maybe.isNothing p) $ OAM.logWarning $
          "Reference " <> ref <> " to SchemaObject from "
            <> schemaName
            <> " could not be found and therefore will be skipped."
        pure $ (,Set.singleton $ transformToModuleName ref) <$> p

-- | creates an alias depending on the strategy
createAlias :: T.Text -> TypeAliasStrategy -> OAM.Generator NamedTypeDef -> OAM.Generator NamedTypeDef
createAlias schemaName strategy res = do
  schemaName' <- haskellifyNameM True schemaName
  (type', (content, dependencies)) <- res
  pure $ case strategy of
    CreateTypeAlias ->
      (type', (content `appendDoc` (ppr <$> tySynD schemaName' [] type'), dependencies))
    DontCreateTypeAlias -> (type', (content, dependencies))

-- | returns the type of a schema. Second return value is a 'Q' Monad, for the types that have to be created
defineModelForSchemaConcrete :: TypeAliasStrategy -> T.Text -> OAS.SchemaObject -> OAM.Generator NamedTypeDef
defineModelForSchemaConcrete strategy schemaName schema =
  let enumValues = OAS.enum schema
   in if null enumValues
        then defineModelForSchemaConcreteIgnoreEnum strategy schemaName schema
        else defineEnumModel strategy schemaName schema enumValues

-- | Creates a Model, ignores enum values
defineModelForSchemaConcreteIgnoreEnum :: TypeAliasStrategy -> T.Text -> OAS.SchemaObject -> OAM.Generator NamedTypeDef
defineModelForSchemaConcreteIgnoreEnum strategy schemaName schema = do
  flags <- OAM.getFlags
  let typeAliasing = createAlias schemaName strategy
  case schema of
    OAS.SchemaObject {type' = OAS.SchemaTypeArray, ..} -> defineArrayModelForSchema strategy schemaName schema
    OAS.SchemaObject {type' = OAS.SchemaTypeObject, ..} ->
      let allOfNull = Set.null $ OAS.allOf schema
          oneOfNull = Set.null $ OAS.oneOf schema
          anyOfNull = Set.null $ OAS.anyOf schema
       in case (allOfNull, oneOfNull, anyOfNull) of
            (False, _, _) -> defineAllOfSchema schemaName $ Set.toList $ OAS.allOf schema
            (_, False, _) -> typeAliasing $ defineOneOfSchema schemaName $ Set.toList $ OAS.oneOf schema
            (_, _, False) -> defineAnyOfSchema strategy schemaName $ Set.toList $ OAS.anyOf schema
            _ -> defineObjectModelForSchema schemaName schema
    _ ->
      typeAliasing $ pure (varT $ getSchemaType flags schema, (emptyDoc, Set.empty))

defineEnumModel :: TypeAliasStrategy -> T.Text -> OAS.SchemaObject -> Set.Set Aeson.Value -> OAM.Generator NamedTypeDef
defineEnumModel strategy schemaName schema enumValuesSet = do
  OAM.logInfo (T.pack "Generate Enum " <> schemaName)
  let enumValues = Set.toList enumValuesSet
  let getConstructor (a, _, _) = a
  let getValueInfo value = do
        cname <- haskellifyNameM True (schemaName <> T.pack "Enum" <> T.replace "\"" "" (T.pack (show value)))
        pure (normalC cname [], cname, value)
  name <- haskellifyNameM True schemaName
  (typ, (content, dependencies)) <- defineModelForSchemaConcreteIgnoreEnum strategy (schemaName <> T.pack "EnumValue") schema
  constructorsInfo <- mapM getValueInfo enumValues
  otherName <- haskellifyNameM True (schemaName <> T.pack "EnumOther")
  typedName <- haskellifyNameM True (schemaName <> T.pack "EnumTyped")
  let nameValuePairs = fmap (\(_, a, b) -> (a, b)) constructorsInfo
  let toBangType t = do
        ban <- bang noSourceUnpackedness noSourceStrictness
        banT <- t
        pure (ban, banT)
  let otherC = normalC otherName [toBangType (varT ''Aeson.Value)]
  let typedC = normalC typedName [toBangType typ]
  let jsonImplementation = defineJsonImplementationForEnum name otherName [otherName, typedName] nameValuePairs
  let newType =
        ppr
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
  let nicifyValue (Aeson.String a) = [|Aeson.String $ T.pack $(litE $ stringL $ T.unpack a)|]
      nicifyValue a = [|a|]
      fnArgName = mkName "val"
      getName = fst
      getValue = snd
      fromJsonCns (x : xs) =
        let vl = getValue x
            name = getName x
         in [|if $(varE fnArgName) == $(nicifyValue vl) then $(varE name) else $(fromJsonCns xs)|]
      fromJsonCns [] = appE (varE fallbackName) (varE fnArgName)
      fromJsonFn =
        funD
          (mkName "parseJSON")
          [clause [varP fnArgName] (normalB [|pure $(fromJsonCns nameValues)|]) []]
      fromJson = instanceD (pure []) (appT (varT $ mkName "Data.Aeson.FromJSON") $ varT name) [fromJsonFn]
      toJsonClause (name, value) =
        let jsonValue = Aeson.toJSON value
         in clause [conP name []] (normalB $ nicifyValue jsonValue) []
      toSpecialCons name =
        clause
          [conP name [varP $ mkName "patternName"]]
          (normalB [|Aeson.toJSON $(varE (mkName "patternName"))|])
          []
      toJsonFn =
        funD
          (mkName "toJSON")
          ((toSpecialCons <$> specialCons) <> (toJsonClause <$> nameValues))
      toJson = instanceD (pure []) (appT (varT $ mkName "Data.Aeson.ToJSON") $ varT name) [toJsonFn]
   in fmap ppr toJson `appendDoc` fmap ppr fromJson

-- | defines anyOf types
--
-- If the subschemas consist only of objects an allOf type without any required field can be generated
-- If there are differen subschema types, per schematype a oneOf is generated
defineAnyOfSchema :: TypeAliasStrategy -> T.Text -> [OAS.Schema] -> OAM.Generator NamedTypeDef
defineAnyOfSchema strategy schemaName schemas = do
  OAM.logInfo $ T.pack "defineAnyOfSchema " <> schemaName
  schemasWithDependencies <- Util.mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      schemasWithoutRequired = fmap (\o -> o {OAS.required = Set.empty}) concreteSchemas
      notObjectSchemas = filter (\o -> OAS.type' o /= OAS.SchemaTypeObject) concreteSchemas
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  if null notObjectSchemas
    then addDependencies newDependencies $ defineAllOfSchema schemaName (fmap Concrete schemasWithoutRequired)
    else createAlias schemaName strategy $ defineOneOfSchema schemaName schemas

--    this would be the correct implementation
--    but it generates endless loop because some implementations use anyOf as a oneOf
--    where the schema reference itself
--      let objectSchemas = filter (\o -> OAS.type' o == OAS.SchemaTypeObject) concreteSchemas
--      (propertiesCombined, _) <- fuseSchemasAllOf schemaName (fmap Concrete objectSchemas)
--      if null propertiesCombined then
--        createAlias schemaName strategy $ defineOneOfSchema schemaName schemas
--        else
--          let schemaPrototype = head objectSchemas
--              newSchema = schemaPrototype {OAS.properties = propertiesCombined, OAS.required = Set.empty}
--          in
--            createAlias schemaName strategy $ defineOneOfSchema schemaName (fmap Concrete (newSchema : notObjectSchemas))

-- | defines a OneOf Schema
--
-- creates types for all the subschemas and then creates an adt with constructors for the different
-- subschemas. Constructors are numbered
defineOneOfSchema :: T.Text -> [OAS.Schema] -> OAM.Generator NamedTypeDef
defineOneOfSchema schemaName schemas = do
  if null schemas
    then OAM.logWarning "schemas are empty, can not create OneOfSchemas"
    else OAM.logInfo $ "define oneOf Model " <> schemaName
  flags <- OAM.getFlags
  let indexedSchemas = zip schemas [1 ..]
      defineIndexed schema index = defineModelForSchemaNamed (schemaName <> "OneOf" <> T.pack (show index)) schema
  variants <- mapM (uncurry defineIndexed) indexedSchemas
  let variantDefinitions = vcat <$> mapM (fst . snd) variants
      dependencies = Set.unions $ fmap (snd . snd) variants
      types = fmap fst variants
      indexedTypes = zip types [1 ..]
      createTypeConstruct (typ, n) = do
        t <- typ
        bang' <- bang noSourceUnpackedness noSourceStrictness
        let haskellifiedName = haskellifyName (OAF.optConvertToCamelCase flags) True $ schemaName <> "Variant" <> T.pack (show n)
        normalC haskellifiedName [pure (bang', t)]
      emptyCtx = pure []
      name = haskellifyName (OAF.optConvertToCamelCase flags) True $ schemaName <> "Variants"
      fromJsonFn =
        funD
          (mkName "parseJSON")
          [ clause
              []
              ( normalB
                  [|
                    Aeson.genericParseJSON Aeson.defaultOptions {Aeson.sumEncoding = Aeson.UntaggedValue}
                    |]
              )
              []
          ]
      toJsonFn =
        funD
          (mkName "toJSON")
          [ clause
              []
              ( normalB
                  [|
                    Aeson.genericToJSON Aeson.defaultOptions {Aeson.sumEncoding = Aeson.UntaggedValue}
                    |]
              )
              []
          ]
      dataDefinition =
        ppr
          <$> dataD
            emptyCtx
            name
            []
            Nothing
            (createTypeConstruct <$> indexedTypes)
            [ derivClause
                Nothing
                [ conT ''Show,
                  conT ''Eq,
                  -- makes the programm slow, but oneOf is not used that often
                  conT $ mkName "GHC.Generics.Generic"
                ]
            ]
      toJson = ppr <$> instanceD emptyCtx (appT (varT $ mkName "Data.Aeson.ToJSON") $ varT name) [toJsonFn]
      fromJson = ppr <$> instanceD emptyCtx (appT (varT $ mkName "Data.Aeson.FromJSON") $ varT name) [fromJsonFn]
      innerRes = (varT name, (variantDefinitions `appendDoc` dataDefinition `appendDoc` toJson `appendDoc` fromJson, dependencies))
  pure innerRes

-- | combines schemas so that it is usefull for a allOf fusion
fuseSchemasAllOf :: T.Text -> [OAS.Schema] -> OAM.Generator (Map.Map T.Text OAS.Schema, Set.Set T.Text)
fuseSchemasAllOf schemaName schemas = do
  schemasWithDependencies <- Util.mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      setOfProperties = fmap OAS.properties concreteSchemas
      setOfRequired = fmap OAS.required concreteSchemas
      propertiesCombined = foldl (Map.unionWith const) Map.empty setOfProperties
      requiredCombined = foldl Set.union Set.empty setOfRequired
  pure (propertiesCombined, requiredCombined)

-- | defines a allOf subschema
-- Fuses the subschemas together
defineAllOfSchema :: T.Text -> [OAS.Schema] -> OAM.Generator NamedTypeDef
defineAllOfSchema schemaName schemas = do
  OAM.logInfo $ "define allOf Model " <> schemaName
  schemasWithDependencies <- Util.mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  (propertiesCombined, requiredCombined) <- fuseSchemasAllOf schemaName schemas
  if Map.null propertiesCombined
    then do
      OAM.logWarning "allOf schemas is empty"
      pure (varT ''String, (emptyDoc, Set.empty))
    else
      let schemaPrototype = head concreteSchemas
          newSchema = schemaPrototype {OAS.properties = propertiesCombined, OAS.required = requiredCombined}
       in addDependencies newDependencies $ defineModelForSchemaConcrete DontCreateTypeAlias schemaName newSchema

-- | defines an array
defineArrayModelForSchema :: TypeAliasStrategy -> T.Text -> OAS.SchemaObject -> OAM.Generator NamedTypeDef
defineArrayModelForSchema strategy schemaName schema = do
  (type', (content, dependencies)) <-
    case OAS.items schema of
      Just itemSchema -> defineModelForSchemaNamed schemaName itemSchema
      -- not allowed by the spec
      Nothing -> do
        OAM.logWarning $ T.pack "items is empty for an array (assume string) " <> schemaName
        pure (varT ''String, (emptyDoc, Set.empty))
  let arrayType = appT (varT $ mkName "[]") type'
  schemaName' <- haskellifyNameM True schemaName
  pure
    ( arrayType,
      ( content `appendDoc` case strategy of
          CreateTypeAlias -> ppr <$> tySynD schemaName' [] arrayType
          DontCreateTypeAlias -> emptyDoc,
        dependencies
      )
    )

-- | default derive clause for the objects
objectDeriveClause =
  [ derivClause
      Nothing
      [ conT ''Show,
        conT ''Eq
        -- makes the programm compilation unusable slow
        -- conT $ mkName "GHC.Generics.Generic",
      ]
  ]

-- | Defines a Record
defineObjectModelForSchema :: T.Text -> OAS.SchemaObject -> OAM.Generator NamedTypeDef
defineObjectModelForSchema schemaName schema = do
  flags <- OAM.getFlags
  let convertToCamelCase = OAF.optConvertToCamelCase flags
      name = haskellifyName convertToCamelCase True schemaName
      stringName = nameBase name
      dropLength = length stringName
      props = Map.toList $ OAS.properties schema
      propsWithNames = zip (fmap fst props) $ fmap (haskellifyName convertToCamelCase False . (schemaName <>) . T.toTitle . fst) props
      emptyCtx = pure []
      required = OAS.required schema
  OAM.logInfo $ "define object model " <> T.pack (nameBase name)
  (bangTypes, propertyContent, propertyDependencies) <- propertiesToBangTypes schemaName props required
  let qs :: Q Doc
      qs = do
        bangs <- bangTypes
        let record = recC name (pure <$> bangs)
        ppr <$> dataD emptyCtx name [] Nothing [record] objectDeriveClause
      toJsonInstance = createToJSONImplementation name propsWithNames
      fromJsonInstance = createFromJSONImplementation name propsWithNames required
  pure (varT name, (qs `appendDoc` propertyContent `appendDoc` toJsonInstance `appendDoc` fromJsonInstance, propertyDependencies))

-- | create toJSON implementation for an object
createToJSONImplementation :: Name -> [(T.Text, Name)] -> Q Doc
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
   in ppr <$> instanceD emptyDefs (appT (varT $ mkName "Data.Aeson.ToJSON") $ varT objectName) defaultJsonImplementation

-- | create FromJSON implementation for an object
createFromJSONImplementation :: Name -> [(T.Text, Name)] -> Set.Set T.Text -> Q Doc
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
propertiesToBangTypes :: T.Text -> [(T.Text, OAS.Schema)] -> Set.Set T.Text -> OAM.Generator BangTypesSelfDefined
propertiesToBangTypes _ [] _ = pure (pure [], emptyDoc, Set.empty)
propertiesToBangTypes schemaName props required = do
  flags <- OAM.getFlags
  let propertySuffix = T.pack $ OAF.optPropertyTypeSuffix flags
  let bang' = bang noSourceUnpackedness noSourceStrictness
      createBang :: T.Text -> T.Text -> Q Type -> OAM.Generator (Q VarBangType)
      createBang recordName propName myType =
        let qVar :: Q VarBangType
            qVar = do
              myBang <- bang'
              type' <-
                if recordName `elem` required
                  then myType
                  else appT (varT ''Maybe) myType
              pure (haskellifyName (OAF.optConvertToCamelCase flags) False propName, myBang, type')
         in pure qVar
      propToBangType :: (T.Text, OAS.Schema) -> OAM.Generator (Q VarBangType, Q Doc, Models)
      propToBangType (recordName, schema) = do
        let propName = schemaName <> T.toTitle recordName
        (myType, (content, depenencies)) <- defineModelForSchemaNamed (propName <> propertySuffix) schema
        myBang <- createBang recordName propName myType
        pure (myBang, content, depenencies)
      foldFn :: OAM.Generator BangTypesSelfDefined -> (T.Text, OAS.Schema) -> OAM.Generator BangTypesSelfDefined
      foldFn accHolder next = do
        (bang, content, dependencies) <- accHolder
        (nextBang, nextContent, nextDependencies) <- propToBangType next
        pure
          ( bang `Util.liftedAppend` fmap pure nextBang,
            content `appendDoc` nextContent,
            Set.union dependencies nextDependencies
          )
  foldl foldFn (pure (pure [], emptyDoc, Set.empty)) props
