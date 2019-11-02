module Hedgehog.Servant where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS (c2w)
import           Data.Proxy (Proxy(..))
import           Data.String.Conversions (ConvertibleStrings, cs)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Network.HTTP.Media (renderHeader)
import           Network.HTTP.Client (Request(..), RequestBody(..))
import           Network.HTTP.Client (defaultRequest)
import           Network.HTTP.Types (HeaderName)
import           Servant.API (ToHttpApiData(..))
import           Servant.API (Capture', Description, Summary)
import           Servant.API (ReqBody', Verb, ReflectMethod)
import           Servant.API ((:>), (:<|>))
import           Servant.API (reflectMethod)
import           Servant.API.ContentTypes (AllMimeRender(..))
import           Servant.Client (BaseUrl(..), Scheme(..))

-- | Simple getter from the HList
class HasGen gens g where
  getGen :: gens -> Gen g

-- | The id instance for HasGen
instance HasGen (Gen g) g where
  getGen = id

-- | Type class used to generate requests from `gens` for API `api`
class GenRequest api gens where
  genRequest :: Proxy api -> gens -> Gen (BaseUrl -> Request)

-- | Instance for composite APIs
instance
  ( GenRequest a reqs
  , GenRequest b reqs
  ) => GenRequest (a :<|> b) reqs where
  genRequest _ gens =
    Gen.choice
      [ genRequest (Proxy @a) gens
      , genRequest (Proxy @b) gens
      ]

-- | Instance for description
instance
  ( GenRequest api reqs
  ) => GenRequest (Description d :> api) reqs where
  genRequest _ = genRequest (Proxy @api)

-- | Instance for summary
instance
  ( GenRequest api reqs
  ) => GenRequest (Summary s :> api) reqs where
  genRequest _ = genRequest (Proxy @api)

-- | Instance for path part of API
instance
  ( KnownSymbol path
  , GenRequest api reqs
  ) => GenRequest (path :> api) reqs where
  genRequest _ gens = do
    makeRequest <- genRequest (Proxy @api) gens
    pure $ prependPath (symbolVal $ Proxy @path) . makeRequest

-- | Instance for path capture
instance
  ( ToHttpApiData a
  , HasGen gens a
  , GenRequest api gens
  ) => GenRequest (Capture' modifiers sym a :> api) gens where
    genRequest _ gens = do
      capture <- toUrlPiece <$> getGen @gens @a gens
      makeRequest <- genRequest (Proxy @api) gens
      pure $ prependPath capture . makeRequest

-- | Instance for request body
instance
  ( AllMimeRender contentTypes body
  , HasGen gens body
  , GenRequest api gens
  ) => GenRequest (ReqBody' mods contentTypes body :> api) gens where
    genRequest _ gens = do
      newBody <- getGen @gens @body gens

      (contentType, body) <-
        Gen.element $ allMimeRender (Proxy @contentTypes) newBody

      makeRequest <- genRequest (Proxy @api) gens

      pure $ setBody body
           . addHeader "Content-Type" (renderHeader contentType)
           . makeRequest

instance
  ( ReflectMethod method
  ) => GenRequest (Verb method status contentTypes body) gens where
    genRequest _ _ =
      pure $ \baseUrl -> defaultRequest
        { host = cs . baseUrlHost $ baseUrl
        , port = baseUrlPort baseUrl
        , secure = baseUrlScheme baseUrl == Https
        , method = reflectMethod (Proxy @method)
        }


setBody :: LBS.ByteString -> Request -> Request
setBody body oldReq = oldReq { requestBody = RequestBodyLBS body }

addHeader :: HeaderName -> BS.ByteString -> Request -> Request
addHeader name value oldReq =
  let
    headers = (name, value) : requestHeaders oldReq
  in
    oldReq { requestHeaders = headers }

-- | Helper function for prepending a new URL piece
prependPath :: ConvertibleStrings s BS.ByteString => s -> Request -> Request
prependPath new oldReq =
  let
    partialUrl = BS.dropWhile (== BS.c2w '/') . path $ oldReq
    urlPieces = filter (not . BS.null) [cs new, partialUrl]
  in
    oldReq { path = "/" <> BS.intercalate "/" urlPieces }
