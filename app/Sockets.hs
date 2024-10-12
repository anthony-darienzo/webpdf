{-# LANGUAGE OverloadedStrings #-}

{- This file exists so that I can move TemplateHaskell and Generics out of Main. -}
module Sockets 
  ( SocketEvent (..)
  , SocketMessage (..)
  , decode
  , encode
  ) where

import Data.Text (Text)
import Network.WebSockets (DataMessage (..))
import Data.Aeson
import Data.ByteString (ByteString)

import Control.Monad (mzero)

data SocketEvent = Initialize | Message | Reload

instance FromJSON SocketEvent where
  parseJSON (String "initialize") = pure Initialize
  parseJSON (String "message")    = pure Message
  parseJSON (String "reload")     = pure Reload
  parseJSON _ = mzero

instance ToJSON SocketEvent where
  toJSON Initialize = String "initialize"
  toJSON Message    = String "message"
  toJSON Reload     = String "reload"

data SocketMessage = SocketMessage 
  { event :: SocketEvent
  , message :: Text }

instance FromJSON SocketMessage where
  parseJSON = withObject "SocketMessage" $ \v -> SocketMessage
    <$> v .: "event"
    <*> v .: "message"

instance ToJSON SocketMessage where
    toJSON (SocketMessage e m) =
        object ["event" .= e, "message" .= m]

    toEncoding (SocketMessage e m) =
        pairs ("event" .= e <> "message" .= m)
