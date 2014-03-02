module Vogogo.Internal where

import Control.Applicative ((<$>))
import Control.Monad (when, (<=<))
import UnexceptionalIO (fromIO, runUnexceptionalIO)
import Control.Exception (fromException)
import Control.Error (EitherT(..), fmapLT, throwT, runEitherT, hush)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Char8 as BS8 -- eww

import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)
import Network.HTTP.Types.Status (Status)
import System.IO.Streams (OutputStream, InputStream, fromLazyByteString)
import System.IO.Streams.Attoparsec (parseFromStream, ParseException(..))
import Network.Http.Client (withConnection, establishConnection, sendRequest, buildRequest, http, setAccept, setContentType, Response, receiveResponse, RequestBuilder, inputStreamBody, getStatusCode, setAuthorizationBasic, setContentLength, getHeader)
import qualified Network.Http.Client as HttpStreams

data APIError = APIParamError | APIAuthError | APINotFoundError | APIParseError | APIRequestError Status | APIOtherError
	deriving (Show, Eq)

newtype UUID = UUID String deriving (Eq)

instance Show UUID where
	show (UUID s) = show s

data VogogoAuth = VogogoAuth String String String

basicAuth :: VogogoAuth -> RequestBuilder ()
basicAuth (VogogoAuth user key token) =
	setAuthorizationBasic (BS8.pack user) (BS8.pack $ key ++ ":" ++ token)

newtype VogogoResponseList a = VogogoResponseList [a]

getVogogoResponseList :: VogogoResponseList a -> [a]
getVogogoResponseList (VogogoResponseList xs) = xs

instance (Aeson.FromJSON a) => Aeson.FromJSON (VogogoResponseList a) where
	parseJSON (Aeson.Object o) = VogogoResponseList <$> o .: T.pack "objects"
	parseJSON _ = fail "Vogogo response is always an object"

create :: (Aeson.ToJSON a) => String -> VogogoAuth -> a -> IO (Either APIError URI)
create path auth x = runEitherT $ do
	resp <- EitherT $ post (apiCall path) (basicAuth auth) x
		(const . return . statusCodeHandler)
	when (getStatusCode resp /= 201) $ throwT APIParseError
	case getHeader resp (BS8.pack "Location") >>= decode of
		Just loc -> return loc
		Nothing  -> throwT APIParseError
	where
	decode = parseAbsoluteURI <=< fmap T.unpack . hush . T.decodeUtf8'

baseURI :: URI
baseURI = URI "https:" (Just $ URIAuth "" "api.vogogo.com" "") "/v1/" "" ""

apiCall :: String -> URI
apiCall ('/':path) = apiCall path
apiCall path = baseURI { uriPath = uriPath baseURI ++ path }

post :: (Aeson.ToJSON a) => URI -> RequestBuilder () -> a -> (Response -> InputStream ByteString -> IO b) -> IO b
post uri req payload handler = do
	let req' = do
		setAccept (BS8.pack "application/json")
		setContentType (BS8.pack "application/json")
		setContentLength (LZ.length body)
		req
	bodyStream <- fromLazyByteString body
	oneShotHTTP HttpStreams.POST uri req' (inputStreamBody bodyStream) handler
	where
	body = Aeson.encode payload

safeJSONresponse :: (Aeson.FromJSON a) => Response -> InputStream ByteString -> IO (Either APIError (Response, a))
safeJSONresponse resp i = runUnexceptionalIO $ runEitherT $ do
	v <- fmapLT (handle . fromException) $ fromIO $ parseFromStream Aeson.json' i
	case Aeson.fromJSON v of
		Aeson.Success a -> return (resp, a)
		Aeson.Error _ -> throwT APIParseError
	where
	handle (Just (ParseException _)) = APIParseError
	handle _ = APIOtherError

statusCodeHandler :: Response -> Either APIError Response
statusCodeHandler resp =
	case getStatusCode resp of
		code | code >= 200 && code < 300 -> Right resp
		400 -> Left APIParamError
		401 -> Left APIAuthError
		404 -> Left APINotFoundError
		code -> Left $ APIRequestError $ toEnum code

oneShotHTTP :: HttpStreams.Method -> URI -> RequestBuilder () -> (OutputStream Builder -> IO ()) -> (Response -> InputStream ByteString -> IO b) -> IO b
oneShotHTTP method uri req body handler = do
	req' <- buildRequest $ do
		http method (BS8.pack $ uriPath uri)
		req
	withConnection (establishConnection url) $ \conn -> do
		sendRequest conn req' body
		receiveResponse conn handler
	where
	url = BS8.pack $ show uri -- URI can only have ASCII, so should be safe
