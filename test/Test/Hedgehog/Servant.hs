module Test.Hedgehog.Servant where

import           Data.Aeson (ToJSON)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Functor ((<&>))
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Servant.API (ReqBody, JSON, Post)
import           Servant.API ((:>))
import           Servant.Client (BaseUrl)
import           Network.HTTP.Client (Request)

import           Hedgehog.Servant (GenRequest(..))

-- A typical cat:
data Cat = Cat
  { name :: Text
  , color :: Text
  }
  deriving (Generic)

-- With ability to go back and forth to JSON:
instance ToJSON Cat

-- And a simple generator to get random values of Cat:
genCat :: Gen Cat
genCat = Cat
  <$> Gen.element [ "Kitty", "Misty" ]
  <*> Gen.element [ "Red", "Green", "Blue" ]

-- Here's a simple API that allows posting a Cat in JSON to
--
-- POST /cats
type SomeAPI =
  "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] ()

-- Generate a request to the Cat API from a base URL
catRequestGen :: BaseUrl -> Gen Request
catRequestGen baseUrl =
  genRequest (Proxy @SomeAPI) genCat <&>
    \makeReq -> makeReq baseUrl
