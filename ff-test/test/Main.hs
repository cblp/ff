{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main
  ( main
    )
where

import Config (configTests)
import Database (dataTests)
import Readme (readmeTest)
import Regression (mkRegressionTest)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  regressionTest <- mkRegressionTest
  withSystemTempDirectory "ff-test" $ \tmp ->
    defaultMain
      $ testGroup "" [configTests, dataTests, readmeTest, regressionTest tmp]
