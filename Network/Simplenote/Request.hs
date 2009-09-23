module Network.Simplenote.Request
  ( 
  -- * Authentication token
    Token
  , authenticate
  -- * HTTP helpers
  , post
  , get
  ) where

import Data.List         (intercalate)
import Data.Typeable     (Typeable(..))

import Network.Curl.Post     (multiformString, HttpPost(..))
import Network.Curl.Download (openURIWithOpts)
import Network.Curl          (CurlOption(..))
import Network.HTTP.Base     (urlEncode, urlEncodeVars)

import qualified Codec.Binary.UTF8.String as US
import qualified Data.ByteString.Char8    as Char8
import qualified Codec.Binary.Base64      as B64

data Token = Token
               String           -- ^ email address
               String           -- ^ simplenote token
             deriving (Show)

-- | Authenticate to simplenote with the given email &
-- password. Returns a 'Token' to use for subsequent requests.
authenticate :: String          -- ^ email address
             -> String          -- ^ password
             -> IO (Either String Token)
authenticate email password =
  let body = b64EncodeString 
           $ urlEncodeVars [("email", email), ("password", password)]
  in
    post_ "login" [] body >>= return . either Left (Right . Token email)

-- | POST a request with the given method, args & body to the
-- simplenote server.
post :: Token                   -- ^ authentication token
     -> String                  -- ^ method name
     -> [(String, String)]      -- ^ arg key-value pairs
     -> String                  -- ^ body
     -> IO (Either String String)
post token method args body = 
  post_ method (addAuthArgs token args) (b64EncodeString body)

-- | GET a request with the given method & args to the simplenote
-- | server.
get :: Token                    -- ^ authentication token
    -> String                   -- ^ method name
    -> [(String, String)]       -- ^ arg key-value pairs
    -> IO (Either String String)
get token method args = 
  get_  method (addAuthArgs token args)

-- Internal:

baseUrl = "https://simple-note.appspot.com/api/"

urlOfMethod method [] = baseUrl ++ method
urlOfMethod method args = urlOfMethod method [] ++ "?" ++ urlEncodeVars args

fetch url opts =
  openURIWithOpts opts url >>= \response ->
    case response of
      Left error   -> return $ Left error
      Right result -> return $ Right (Char8.unpack result)

post_ method args body = 
  fetch (urlOfMethod method args) ([CurlPost True, CurlPostFields [body]])
get_ method args = fetch (urlOfMethod method args) []

addAuthArgs (Token email token) args =
  args ++ [("email", email), ("auth", token)]

b64EncodeString :: String -> String
b64EncodeString = B64.encode . US.encode

