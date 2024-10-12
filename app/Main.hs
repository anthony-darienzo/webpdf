{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import qualified Network.Wai.Middleware.Static as NS

-- For WebSockets
import Network.Wai (Application)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

-- For watching files
--import qualified System.FSNotify as FSNotify

import Text.Read (readMaybe)
import System.Environment (getArgs)

import Viewer (viewer)
import Sockets ( SocketEvent (..), SocketMessage (..), decode, encode )

import Paths_webpdf (getDataDir)

defaultPort :: Int
defaultPort = 3000

grabStaticFiles :: IO NS.Policy
grabStaticFiles = fmap NS.addBase getDataDir

mainScottyLoop :: FilePath -> IO Application
mainScottyLoop pdf_path = do
  putStrLn $ "Serving PDF file " <> pdf_path
  static_file_policy <- grabStaticFiles
  scottyApp $ do
    middleware (NS.unsafeStaticPolicy static_file_policy)
    get "/pdf" $ do
      html viewer
    get "/currentPDF" $ do
      setHeader "Content-Type" "application/pdf"
      setHeader "Cache-Control" "no-cache"
      file pdf_path
    get "/" $ do
      redirect "/pdf?file=/currentPDF"

mainSocketsLoop :: WS.ServerApp
mainSocketsLoop pending = do
  putStrLn "WebSocket connection request."
  initial_conn <- WS.acceptRequest pending
  welcome <- WS.receiveData initial_conn
  case (decode welcome :: Maybe SocketMessage) of
    Just (SocketMessage Initialize greet) -> putStrLn $ "From client: " <> show greet
    _ -> putStrLn "Connection did not send greeting!"
  WS.withPingPong WS.defaultPingPongOptions initial_conn
    (\ _ -> return () ) -- Nothing to do besides ping/pong
  forever $ threadDelay 1000000

serverLoop :: Int -> FilePath -> IO ()
serverLoop port pdf_path = do
  let settings = Warp.setPort port Warp.defaultSettings
  httpLoop    <- mainScottyLoop pdf_path
  Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions mainSocketsLoop httpLoop

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
  [ "webpdf : Haskell PDF.js server."
  , ""
  , "Uses scotty and pdf.js to render a PDF supllied on the command line into"
  , " a static server."
  , "  - Use -p to specify a port (default 3000)."
  , ""
  , "Usage: wedpdf [-p PORT] SAMPLE.PDF"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                      -> printHelpDialog
    ["-h"]                  -> printHelpDialog
    [pdf_path]              -> serverLoop defaultPort pdf_path
    ["-p", portarg, pdf_path]  -> do
      let port = readMaybe portarg
      case port of
        Nothing -> ioError $ userError "Error : Invalid port."
        Just p  -> serverLoop p pdf_path
    _ -> printHelpDialog
