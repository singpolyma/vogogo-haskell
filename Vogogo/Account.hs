module Vogogo.Account (Account(..), getWallet, getAccounts) where

import Data.List (find)
import Control.Applicative ((<$>))
import Control.Error (EitherT(..), runEitherT, hoistMaybe, noteT)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Network.Http.Client (setContentLength, emptyBody)
import qualified Network.Http.Client as HttpStreams

import Vogogo.Internal

data Account = Wallet {
		uuid :: UUID
	} |
	BankAccount {
		uuid :: UUID
	}
	deriving (Show, Eq)

instance Aeson.FromJSON Account where
	parseJSON (Aeson.Object o) = do
		typ <- o .: T.pack "account_type"
		case T.unpack typ of
			"WALLET" -> Wallet . UUID <$> o .: T.pack "uuid"
			"BANK"   -> BankAccount . UUID <$> o .: T.pack "uuid"
			_ -> fail "Unknown Vogogo account_type"
	parseJSON _ = fail "Vogogo Account is always represented by a JSON object"

getWallet :: Auth -> IO (Either APIError UUID)
getWallet auth = runEitherT $ do
	accounts <- EitherT $ getAccounts auth
	uuid <$> noteT APINotFoundError (hoistMaybe $ find isWallet accounts)
	where
	isWallet (Wallet {}) = True
	isWallet _           = False

getAccounts :: Auth -> IO (Either APIError [Account])
getAccounts auth = (fmap.fmap) (getVogogoResponseList . snd) $
	oneShotHTTP HttpStreams.GET (apiCall "account/")
	(setContentLength 0 >> basicAuth auth) emptyBody
	(\r i -> fmap (statusCodeHandler r >>) (safeJSONresponse r i))
