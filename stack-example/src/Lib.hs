module Lib
  ( someFunc,
    factorial,
  )
where

import Control.Monad (when)

factorial n = product [1 .. n]

someFunc :: IO ()
someFunc =
  when True (putStrLn "Hola mundo")
