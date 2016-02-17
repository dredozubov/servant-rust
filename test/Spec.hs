{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant
import Servant.Rust
import Test.Hspec

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username" <*> o .: "userid"
  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic, HasForeignType Rust)

instance FromJSON Package

type HackageAPI =
       "users"    :> Get '[JSON] [UserSummary]
  :<|> "user"     :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]

hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

main :: IO ()
main = hspec $ do
  describe "codegen" $ do
    specify "equality" $ do
      rustForAPI (Proxy :: Proxy HackageAPI) `shouldBe` ""
