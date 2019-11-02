{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Servant
  ( tests
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.String (IsString)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Functor ((<&>))
import           Data.Foldable (find)
import           GHC.Generics (Generic)
import           Servant.API (Capture, ReqBody, JSON, Post)
import           Servant.API ((:>), (:<|>))
import           Servant.Client (BaseUrl(..), Scheme(..))
import           Network.HTTP.Client (Request(..), RequestBody(..))
import           Network.HTTP.Types (methodPost)

import           Hedgehog (Gen, Property, PropertyT)
import           Hedgehog (annotate, annotateShow, checkParallel, discover, forAll)
import           Hedgehog (failure, property, success)
import           Hedgehog ((===))
import qualified Hedgehog.Gen as Gen

import           Hedgehog.Servant (GenRequest(..), HList(..))

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
type SimplestApi =
  "my" :> "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] ()

-- Generate a request to the Cat API from a base URL
catRequestGen :: BaseUrl -> Gen Request
catRequestGen baseUrl =
  genRequest (Proxy @SimplestApi) (genCat :*: HNil) <&>
    \makeReq -> makeReq baseUrl

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "localhost"
  , baseUrlPort = 8080
  , baseUrlPath = ""
  }

prop_check_cat_request :: Property
prop_check_cat_request = property $ do
  forAll (catRequestGen defaultBaseUrl) >>= propCat

type CaptureApi =
  "cats" :> Capture "pathParam" Text :> Capture "path2" Text :> Post '[JSON] ()

captureGen :: Gen Text -> BaseUrl -> Gen Request
captureGen txt baseUrl =
  genRequest (Proxy @CaptureApi) (txt :*: HNil) <&>
    \makeReq -> makeReq baseUrl

prop_check_capture_req :: Property
prop_check_capture_req =
  let
    pathParam :: IsString a => a
    pathParam = "myPathParam"
  in
    property $ do
      req <- forAll $ captureGen (pure pathParam) defaultBaseUrl
      propBaseUrl req
      method req === methodPost
      path req === "/cats/" <> pathParam <> "/" <> pathParam

-- A typical dog:
data Dog = Dog
  { dogName :: Text
  , dogHappy :: Bool
  }
  deriving stock (Generic)

-- With ability to go back and forth to JSON:
instance ToJSON Dog
instance FromJSON Dog

-- And a simple generator to get random values of Cat:
genDog :: Gen Dog
genDog = Dog
  <$> Gen.element [ "Pluto", "Rowdy" ]
  <*> Gen.bool

type AltApi =
       "my" :> "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] ()
  :<|> "dogs" :> ReqBody '[JSON] Dog :> Post '[JSON] ()

-- Generate a request to the Cat API from a base URL
altRequestGen :: BaseUrl -> Gen Request
altRequestGen baseUrl =
  genRequest (Proxy @AltApi) (genCat :*: genDog :*: HNil) <&>
    \makeReq -> makeReq baseUrl

prop_check_alt_api :: Property
prop_check_alt_api = property $ do
  req <- forAll (altRequestGen defaultBaseUrl)
  case path req of
    "/my/cats" -> propCat req
    "/dogs" -> propDog req
    badPath -> annotate "Bad path" >> annotateShow badPath >> failure

propCat :: Request -> PropertyT IO ()
propCat req = do
  propBaseUrl req
  propContentTypeJson req
  propDecodeJson (Proxy @Cat) req
  method req === methodPost
  path req === "/my/cats"

propDog :: Request -> PropertyT IO ()
propDog req = do
  propBaseUrl req
  propContentTypeJson req
  propDecodeJson (Proxy @Dog) req
  method req === methodPost
  path req === "/dogs"


propBaseUrl :: Request -> PropertyT IO ()
propBaseUrl req = do
  method req === methodPost
  secure req === True
  host req === "localhost"
  port req === 8080

propContentTypeJson :: Request -> PropertyT IO ()
propContentTypeJson req =
  let
    headers = requestHeaders req
  in
    case find ((== "Content-Type"). fst) headers of
      Just _ -> success
      Nothing -> do
        annotate "Couldn't find \"Content-Type\" header"
        annotateShow headers
        failure

propDecodeJson :: forall a. FromJSON a => Proxy a -> Request -> PropertyT IO ()
propDecodeJson _ req =
  case requestBody req of
    RequestBodyLBS (eitherDecode @a -> Right _) ->
      success
    RequestBodyLBS (eitherDecode @a -> Left err) ->
      annotateShow err >> failure
    _ -> failure

tests :: IO Bool
tests = checkParallel $$discover
