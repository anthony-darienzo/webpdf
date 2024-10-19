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
import Control.Concurrent (threadDelay, forkIO)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as Ch
import qualified System.FSNotify as FS
import System.FilePath (takeDirectory)

import Data.Text (Text)
import Text.Read (readMaybe)
import System.Environment (getArgs)

import Viewer (viewer)
import Sockets ( SocketEvent (..), SocketMessage (..), decode, encode )

import Paths_webpdf (getDataDir)

---
--- Setup file-watching logic
---
data Event = Reload | Alert Text | OSEvent FS.Event deriving Show

fileListenThread :: Ch.TChan Event -> FilePath -> IO ()
fileListenThread events dir = do
  putStrLn $ "fileListenThread observing " <> dir
  -- FS.withManager forks the thread, so we loop forever to keep thread alive.
  FS.withManager $ \mgr -> do
    _ <- FS.watchDir mgr dir -- FS.watchDir returns an IO StopListening, which we don't need.
      (const True) -- Accept all events
      (\e -> do
        STM.atomically $ do
          STM.writeTChan events $ OSEvent e)
    forever $ do
      threadDelay 1000000

processEvent :: Ch.TChan Event -> FilePath -> IO (Maybe SocketMessage)
processEvent events pdf_path = do
  maybe_event <- STM.atomically $ Ch.tryReadTChan events
  case maybe_event of
    Nothing -> return Nothing
    Just e  -> do
      putStrLn $ "processEvent thread: " <> show maybe_event
      return $ getMessageFromEvent e
  where
    getMessageFromEvent :: Event -> Maybe SocketMessage
    getMessageFromEvent Reload      = Just $ SocketMessage ClientReload mempty
    getMessageFromEvent (Alert t)   = Just $ SocketMessage Message t
    -- TODO : Add more detail to OSEvent message
    getMessageFromEvent (OSEvent e) = 
      if FS.eventPath e == pdf_path
        then Just $ SocketMessage ClientReload "/currentPDF"
        else Nothing
---
---
---

---
--- Set up the web servers (http and websocket)
---
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

mainSocketsLoop :: Ch.TChan Event -> FilePath -> WS.ServerApp
mainSocketsLoop events pdf_path pending = do
  putStrLn "WebSocket connection request."
  initial_conn <- WS.acceptRequest pending
  welcome <- WS.receiveData initial_conn
  case (decode welcome :: Maybe SocketMessage) of
    Just (SocketMessage Initialize greet) -> putStrLn $ "From client: " <> show greet
    _ -> putStrLn "Connection did not send greeting!"
  WS.withPingPong WS.defaultPingPongOptions initial_conn
    (\ _ -> return () ) -- Nothing to do besides ping/pong
  forever $ do
    msgToClient <- processEvent events pdf_path
    case msgToClient of
      Nothing -> return ()
      Just sm -> do 
        putStrLn $ "Sending to client: " <> show sm
        WS.sendTextData initial_conn $ encode sm
    threadDelay 1000000

serverLoop :: Int -> FilePath -> IO ()
serverLoop port pdf_path = do
  let settings = Warp.setPort port Warp.defaultSettings
  httpLoop    <- mainScottyLoop pdf_path
  events <- STM.newTChanIO
  let socketsLoop = mainSocketsLoop events pdf_path
  _ <- forkIO $ fileListenThread events (takeDirectory pdf_path) -- Dump ThreadID
  Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions socketsLoop httpLoop
---
---
---

--- Finally, provide the main logic for executing webpdf.
printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
  [ "webpdf : Haskell PDF.js server."
  , ""
  , "Uses scotty and pdf.js to render a PDF supllied on the command line into"
  , " a static server."
  , "  - Use -p to specify a port (default 3000)."
  , ""
  , "Usage: webpdf [-p PORT] SAMPLE.PDF"
  ]

printWelcomeDialog :: Int -> FilePath -> IO ()
printWelcomeDialog port pdf_file =
  putStrLn $ "--- webpdf: Starting web server on port " <> show port <> " for file " <> pdf_file

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                      -> printHelpDialog
    ["-h"]                  -> printHelpDialog
    [pdf_path]              -> do
      printWelcomeDialog defaultPort pdf_path
      serverLoop defaultPort pdf_path
    ["-p", portarg, pdf_path]  -> do
      let port = readMaybe portarg
      case port of
        Nothing -> ioError $ userError "Error : Invalid port."
        Just p  -> do
          printWelcomeDialog p pdf_path
          serverLoop p pdf_path
    _ -> printHelpDialog
