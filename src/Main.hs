{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Main where

import System.Environment
import Test.QuickSpec
import Test.QuickSpec.Measure.Large

main = do [n] <- getArgs
          quickSpec (variables ++ take (read n) functions)
