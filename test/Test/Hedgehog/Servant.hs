{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Servant
  ( tests
  ) where

import           Control.Monad (forM_)
import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.String (IsString)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text, splitOn)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Functor ((<&>))
import           Data.Foldable (find)
import           GHC.Generics (Generic)
import           Servant.API (Capture, ReqBody, Header, JSON)
import           Servant.API (QueryFlag, QueryParam, QueryParams)
import           Servant.API (Post)
import           Servant.API ((:>), (:<|>))
import           Servant.Client (BaseUrl(..), Scheme(..))
import           Network.HTTP.Client (Request(..), RequestBody(..))
import           Network.HTTP.Types (methodPost)

import           Hedgehog (Gen, Property, PropertyT)
import           Hedgehog (annotate, annotateShow, checkParallel, discover, forAll)
import           Hedgehog (failure, property, success)
import           Hedgehog ((===))
import qualified Hedgehog.Gen as Gen

import           Hedgehog.Servant (GenRequest(..), GList(..))

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
  genRequest (Proxy @SimplestApi) (genCat :*: GNil) <&>
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
  genRequest (Proxy @CaptureApi) (txt :*: GNil) <&>
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
  genRequest (Proxy @AltApi) (genCat :*: genDog :*: GNil) <&>
    \makeReq -> makeReq baseUrl

prop_check_alt_api :: Property
prop_check_alt_api = property $ do
  req <- forAll (altRequestGen defaultBaseUrl)
  case path req of
    "/my/cats" -> propCat req
    "/dogs" -> propDog req
    badPath -> annotate "Bad path" >> annotateShow badPath >> failure

type HeaderApi = "cats" :> Header "Correlation-Id" Text :> Post '[JSON] ()

headerRequestGen :: BaseUrl -> Gen Request
headerRequestGen baseUrl =
  let
    textGen :: Gen Text
    textGen = pure "some-corr-id"
  in
    genRequest (Proxy @HeaderApi) (textGen :*: GNil) <&>
      \makeReq -> makeReq baseUrl

prop_has_correlation_id :: Property
prop_has_correlation_id = property $ do
  req <- forAll (headerRequestGen defaultBaseUrl)
  case find ((== "Correlation-Id") . fst) (requestHeaders req) of
    Just _ -> success
    Nothing -> failure

type QueryFlagApi =
  "my" :> "cats" :> QueryFlag "foo" :> QueryFlag "fi" :> Post '[JSON] ()

queryFlagGen :: BaseUrl -> Gen Request
queryFlagGen baseUrl =
  genRequest (Proxy @QueryFlagApi) GNil <&>
    \makeReq -> makeReq baseUrl

prop_has_query_Flag :: Property
prop_has_query_Flag = property $ do
  req <- forAll (queryFlagGen defaultBaseUrl)
  queryString req === "foo&fi"

type QueryParamApi =
  "my" :> "cats" :> QueryParam "foo" Text :> QueryParam "fi" Text :> Post '[JSON] ()

queryParamGen :: BaseUrl -> Gen Request
queryParamGen baseUrl =
  let
    paramGen :: Gen Text
    paramGen = pure "bar"
  in
    genRequest (Proxy @QueryParamApi) (paramGen :*: GNil) <&>
      \makeReq -> makeReq baseUrl

prop_has_query_param :: Property
prop_has_query_param = property $ do
  req <- forAll (queryParamGen defaultBaseUrl)
  queryString req === "foo=bar&fi=bar"

type QueryParamsApi =
  "my" :> "cats" :> QueryParams "foo" Text :> Post '[JSON] ()

queryParamsGen :: BaseUrl -> Gen Request
queryParamsGen baseUrl =
  let
    paramGen :: Gen Text
    paramGen = pure "bar"
  in
    genRequest (Proxy @QueryParamsApi) (paramGen :*: GNil) <&>
      \makeReq -> makeReq baseUrl

prop_has_query_params :: Property
prop_has_query_params = property $ do
  queryParams <-
    decodeUtf8 . queryString <$>
      forAll (queryParamsGen defaultBaseUrl)

  forM_ (splitOn "&" queryParams) (=== "foo[]=bar")

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
