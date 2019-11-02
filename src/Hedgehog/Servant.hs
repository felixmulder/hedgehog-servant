module Hedgehog.Servant where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w)
import           Data.Proxy (Proxy(..))
import           Data.String.Conversions (cs)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Network.HTTP.Media (renderHeader)
import           Network.HTTP.Client (Request(..), RequestBody(..))
import           Network.HTTP.Client (defaultRequest)
import           Servant.API (ReqBody', Verb, ReflectMethod)
import           Servant.API ((:>))
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

instance
  ( KnownSymbol path
  , GenRequest api reqs
  ) => GenRequest (path :> api) reqs where
  genRequest _ gens = do
    makeRequest <- genRequest (Proxy @api) gens
    pure $ \baseUrl ->
      let
        partialReq = makeRequest baseUrl
        partialUrl = BS.dropWhile (== BS.c2w '/') .  path $ partialReq
        urlPieces = filter (not . BS.null) [cs . symbolVal $ Proxy @path, partialUrl]
      in
        partialReq { path = "/" <> BS.intercalate "/" urlPieces }

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
      pure $ \baseUrl ->
        let
          baseRequest = makeRequest baseUrl
          headers = ("Content-Type", renderHeader contentType)
                  : requestHeaders baseRequest
        in
          baseRequest
            { requestBody = RequestBodyLBS body
            , requestHeaders = headers
            }

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
