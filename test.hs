{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative       (liftA2)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Foldable             (asum)
import           Data.List                 (intersect)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromJust)
import           Data.Monoid
import           System.Directory

data SituationGraph c e k x = SG
  { situations  :: Map k (e (Maybe x))
  , transitions :: Map k [(c k, (c x) -> e x)]
  }

isDirEmpty = ((fmap $ (== 0) . length) . listDirectory)

exampleSituations =
  M.fromList
    [ ("clean", (guard . not) <$> doesDirectoryExist "/tmp/foo")
    , ( "foo"
      , (fmap guard) $
        liftA2
          (\f -> \b -> f && (not b))
          (doesDirectoryExist "/tmp/foo")
          (doesDirectoryExist "/tmp/foo/bar"))
    , ("bar", guard <$> doesDirectoryExist "/tmp/foo/bar")
    ]

exampleTransitions =
  M.fromList
    [ ( "foo"
      , [ ( ["bar"]
          , const $
            removeDirectory "/tmp/foo/bar" >> putStrLn "deleting /tmp/foo/bar")
        , ( ["clean"]
          , const $ createDirectory "/tmp/foo" >> putStrLn "creating /tmp/foo")
        ])
    , ( "clean"
      , [ ( ["foo"]
          , const $ removeDirectory "/tmp/foo" >> putStrLn "deleting /tmp/foo")
        ])
    , ( "bar"
      , [ ( ["foo"]
          , const $
            createDirectory "/tmp/foo/bar" >> putStrLn "creating /tmp/foo/bar")
        ])
    ]

exampleSG = SG exampleSituations exampleTransitions

lkp k = fromJust . M.lookup k

logW x = tell [x]

run avoid sg k = do
  m <- lift sitch
  logW $ "Trying to get to " <> show k
  maybe travel arrive m
  where
    sitch = lkp k (situations sg)
    routes = lkp k (transitions sg)
    arrive v = do
      logW "Already there"
      pure $ Just v
    travel = do
      logW "Not there; trying to reach dependencies..."
      runMaybeT .
        asum .
        (fmap attemptRoute) .
        (filter $ \(deps, _) -> (== 0) . length $ intersect avoid deps) $
        routes
    attemptRoute (deps, e) =
      (lift . lift) . e =<< (traverse (MaybeT . run (avoid <> [k]) sg) deps)
