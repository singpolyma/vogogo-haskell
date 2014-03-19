module Vogogo.Transaction (sendEFT) where

import Data.Fixed (Centi)
import Control.Monad (when)
import Currency (ISO4217Currency)
import Data.Aeson ((.=))
import Control.Error (EitherT(..), throwT, runEitherT)
import Network.Http.Client (getStatusCode)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Vogogo.Internal

data EFT = EFT UUID UUID Centi ISO4217Currency

instance Aeson.ToJSON EFT where
	toJSON (EFT (UUID from) (UUID to) amount currency) = Aeson.object [
			T.pack "src_account" .= from,
			T.pack "dst_account" .= to,
			T.pack "amount"      .= amount,
			T.pack "currency"    .= show currency
		]

sendEFT ::
	Auth
	-> UUID  -- ^ From account
	-> UUID  -- ^ To account
	-> Centi -- ^ Amount
	-> ISO4217Currency
	-> IO (Either APIError ())
sendEFT auth from to amount currency = runEitherT $ do
	resp <- EitherT $ post (apiCall "credit/") (basicAuth auth)
		(EFT from to amount currency)
		(const . return . statusCodeHandler)
	when (getStatusCode resp /= 201) $ throwT APIParseError
