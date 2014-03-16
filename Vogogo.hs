module Vogogo (Auth(..),APIError(..),UUID(..),uuid,delete) where

import Network.URI (URI(..))
import Network.Http.Client (setContentLength, emptyBody)
import qualified Network.Http.Client as HttpStreams

import Vogogo.Internal

uuid :: URI -> UUID
uuid (URI _ _ pth _ _) = UUID $ go $ reverse pth
	where
	go ('/':pth) = go pth
	go pth = reverse $ takeWhile (/='/') pth

delete :: Auth -> URI -> IO (Either APIError ())
delete auth uri = (fmap.fmap) (const ()) $ oneShotHTTP HttpStreams.DELETE uri
	(setContentLength 0 >> basicAuth auth) emptyBody
	(const . return . statusCodeHandler)
