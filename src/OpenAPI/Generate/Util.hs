-- | Utility functions for the OpenAPI code generator
module OpenAPI.Generate.Util where

import Control.Applicative

-- | Split a List on on a given element
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x =
  foldr
    ( \element (currentAcc : acc) ->
        if element == x
          then [] : currentAcc : acc
          else (element : currentAcc) : acc
    )
    [[]]

-- | A version of 'mapMaybe' that works with a monadic predicate.
-- from https://hackage.haskell.org/package/extra-1.7.1/docs/src/Control.Monad.Extra.html#mapMaybeM copied
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM op = foldr f (pure [])
  where
    f x xs = do x <- op x; case x of { Nothing -> xs; Just x -> do { xs <- xs; pure $ x : xs } }

liftedAppend :: (Applicative f, Semigroup a) => f a -> f a -> f a
liftedAppend = liftA2 (<>)
