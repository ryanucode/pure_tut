module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (null, filter, range, (..))
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product)
import Control.MonadZero (guard)
import Math (sqrt)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
     then 0
     else 1 + length (unsafePartial tail arr)

even :: Int -> Boolean
even 0 = true
even 1 = false
even n = even (n - 2)

squareArr :: Array Int -> Array Int
squareArr = map (\n -> n * n)

onlyPositive :: Array Int -> Array Int
onlyPositive = filter ((<=) 0)

onlyPositiveInfix :: Array Int -> Array Int
onlyPositiveInfix = (<$?>) ((<=) 0)

infix 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

pythTriple n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard (a * a + b * b == c * c && c <= n)
  pure [a,b,c]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow $ fib 5
  logShow $ fact 10
  logShow $ even 5
  logShow $ even 42
  logShow $ squareArr [1,2,3,4,5,6,7,8,9]
  logShow $ onlyPositive [-2, -1, 0, 1, 2]
  logShow $ ((<=) 0) <$?> [-2, -1, 0, 1, 2]
  logShow $ factors 10
  logShow $ pythTriple 40
