{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Rust
  ( rustForAPI
  ) where

import Control.Lens
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text, pack, toLower, intercalate)
import Data.Text.Encoding (decodeUtf8)
import NeatInterpolation
import Network.HTTP.Types
import Servant.Foreign

data Rust

rust :: Proxy Rust
rust = Proxy

instance HasForeignType Rust Text Int where
  -- FIXME: should probably depend on arch
  typeFor _ _ _ = "i32"

instance HasForeignType Rust Text Bool where
  typeFor _ _ _ = "std::bool"

instance
  {-# OVERLAPPING #-}
  HasForeignType Rust Text [Char] where
    typeFor _ _ _ = "std::string"

instance HasForeignType Rust Text Text where
  typeFor _ _ _ = "std::string"

-- instances can overlap because of dreaded strings
instance
  {-# OVERLAPPABLE #-}
  ( ftype ~ Text
  , FromJSON t
  , ToJSON t
  , HasForeignType Rust ftype t)
    =>  HasForeignType Rust ftype [t] where
      typeFor rust ft layout = "Vec<" <> typeFor rust ft layout <> ">"

type BaseURL = Text

rustForAPI
  :: ( ftype ~ Text
     , HasForeign Rust ftype api
     , GenerateList ftype (Foreign ftype api))
  => Proxy api
  -> BaseURL
  -> Text
rustForAPI api = rustForAPI' api Nothing

rustForAPI'
  :: ( ftype ~ Text
     , HasForeign Rust ftype api
     , GenerateList ftype (Foreign ftype api))
  => Proxy api
  -> Maybe Text -- type introductions
  -> BaseURL
  -> Text
rustForAPI' api typeIntros baseUrl = [text|
  extern crate hyper;

  use std::io::Read;
  use hyper::Client;
  use hyper::header::Connection;

  ${clientFunctions reqs} |]
  where
    reqs = listFromAPI rust (Proxy :: Proxy Text) api

hyperMethod :: Method -> Text
hyperMethod = over _tail toLower . decodeUtf8

clientFunctions :: BaseURL -> [Req Text] -> Text
clientFunctions baseUrl = mconcat . map (clientFunction baseUrl)

arguments :: Req Text -> Text
arguments (view (reqUrl . queryStr) -> captures)
  = [text|client: Client${captureArgs captures}|]
  where
    captureArgs [] = ""
    captureArgs x
      = ", "
     <> (intercalate ", "
      . map (view ( queryArgName
                  . to (\a -> a ^. argPath <> ": " <> a ^. argType)))
      $ x)

clientFunction :: BaseURL -> Req Text -> Text
clientFunction baseUrl req = [text|
fn ${req ^. reqFuncName}(
        ${arguments req}
    ) -> result::Result<${req ^. reqReturnType}>, String> {
    match client
        .${req ^. reqMethod . to hyperMethod}("${baseUrl}${req ^. reqUrl}")
        .send() {
            Ok(mut res) => {
                let mut body = String::new();
                res.read_to_string(&mut body).unwrap();

                match json::decode(&body) {
                    Ok(decoded) => Ok(decoded),
                    Err(_) => Err("json error".to_string())
                }
            },
            Err(e) => {
                println!("http error: ");
                Err("http error".to_string())
            }
    }
}

|]
