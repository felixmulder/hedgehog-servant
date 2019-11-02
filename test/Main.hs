module Main (main) where

import           Hedgehog.Main (defaultMain)

import qualified Test.Hedgehog.Servant as Hedgehog.Servant

main :: IO ()
main = defaultMain
  [ Hedgehog.Servant.tests
  ]
