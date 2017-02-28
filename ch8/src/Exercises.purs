module Exercises where

import Prelude
import Data.Maybe
import Data.List
import Data.String
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

foldM :: forall m a b. Monad m
  => (a -> b -> m a)
  -> a
  -> List b
  -> m a
foldM _ a Nil = pure a
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

--type Person a = { firstName :: String, lastName :: String, age :: Int | a }
type Person = { firstName :: String, lastName :: String, age :: Int, country :: String }

formatName :: forall a. {firstName :: String, lastName :: String | a} -> String
formatName {firstName, lastName} = firstName <> " " <> lastName

billMurray :: Person
billMurray =
  { firstName: "Bill"
  , lastName: "Murray"
  , age: 100
  , country: "Heaven"
  }

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  log "Hello sailor!"
  log $ formatName billMurray
  n <- random
  logShow n
