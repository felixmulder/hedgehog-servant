{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Servant
  ( tests
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Functor ((<&>))
import           GHC.Generics (Generic)
import           Servant.API (ReqBody, JSON, Post)
import           Servant.API ((:>))
import           Servant.Client (BaseUrl(..), Scheme(..))
import           Network.HTTP.Client (Request(..), RequestBody(..))
import           Network.HTTP.Types (methodPost)

import           Hedgehog (Gen, Property)
import           Hedgehog (annotateShow, checkParallel, discover, forAll)
import           Hedgehog (failure, property, success)
import           Hedgehog ((===))
import qualified Hedgehog.Gen as Gen

import           Hedgehog.Servant (GenRequest(..))

-- A typical cat:
data Cat = Cat
  { name :: Text
  , color :: Text
  }
  deriving stock (Generic)

-- With ability to go back and forth to JSON:
instance ToJSON Cat
instance FromJSON Cat

-- And a simple generator to get random values of Cat:
genCat :: Gen Cat
genCat = Cat
  <$> Gen.element [ "Kitty", "Misty" ]
  <*> Gen.element [ "Red", "Green", "Blue" ]

-- Here's a simple API that allows posting a Cat in JSON to
--
-- POST /cats
type SomeAPI =
  "my" :> "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] ()

-- Generate a request to the Cat API from a base URL
catRequestGen :: BaseUrl -> Gen Request
catRequestGen baseUrl =
  genRequest (Proxy @SomeAPI) genCat <&>
    \makeReq -> makeReq baseUrl

prop_check_cat_request :: Property
prop_check_cat_request = property $ do
  req <- forAll . catRequestGen $ BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "localhost"
    , baseUrlPort = 8080
    , baseUrlPath = ""
    }

  method req === methodPost
  secure req === True
  host req === "localhost"
  port req === 8080
  path req === "/my/cats"

  case requestBody req of
    RequestBodyLBS (eitherDecode @Cat -> Right _) ->
      success
    RequestBodyLBS (eitherDecode @Cat -> Left err) ->
      annotateShow err >> failure
    _ -> failure

tests :: IO Bool
tests = checkParallel $$discover
