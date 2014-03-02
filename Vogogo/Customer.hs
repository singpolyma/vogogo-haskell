module Vogogo.Customer (Account(..), createAccount) where

import Currency (ISO4217Currency)
import Data.Aeson ((.=))
import Network.URI (URI)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Vogogo.Internal

data Account = BankAccount {
		accountName       :: String,
		transitNumber     :: String,
		institutionNumber :: String,
		accountNumber     :: String,
		currency          :: ISO4217Currency
	} deriving (Show, Eq)

instance Aeson.ToJSON Account where
	toJSON (BankAccount name transit inst acct currency) = Aeson.object [
			T.pack "account_type"       .= "BANK_PA",
			T.pack "name"               .= name,
			T.pack "transit_number"     .= transit,
			T.pack "institution_number" .= inst,
			T.pack "account_number"     .= acct,
			T.pack "currency"           .= show currency
		]

createAccount :: VogogoAuth -> Account -> IO (Either APIError URI)
createAccount = create "customer_account/"
