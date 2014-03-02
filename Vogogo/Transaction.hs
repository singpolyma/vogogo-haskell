module Vogogo.Transaction (sendEFT) where

import Currency (ISO4217Currency)
import Network.URI (URI(..))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Vogogo.Customer
import Vogogo.Internal

data EFT = EFT UUID UUID Double ISO4217Currency

instance Aeson.ToJSON EFT where
	toJSON (EFT (UUID from) (UUID to) amount currency) = Aeson.object [
			T.pack "src_account" .= from,
			T.pack "dst_account" .= to,
			T.pack "amount"      .= amount,
			T.pack "currency"    .= show currency
		]

sendEFT ::
	VogogoAuth
	-> UUID   -- ^ From account
	-> UUID   -- ^ To account
	-> Double -- ^ Amount
	-> ISO4217Currency
	-> IO (Either APIError URI)
sendEFT auth from to amount currency = create "customer_account/" auth $
	EFT from to amount currency
