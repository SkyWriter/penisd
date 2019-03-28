{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Foldable
import Data.List.Split
import Network.Socket
import Network.Socket.ByteString as S (send)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C



penisFrames :: [ByteString]
penisFrames = map C.unlines $ chunksOf 16 $ C.lines $ $(embedFile "penis.txt")

slowlySend :: Socket -> ByteString -> IO ()
slowlySend sock bs = S.send sock bs >> threadDelay 50000

mainLoop :: MVar Int -> Socket -> IO ()
mainLoop clientCount sock = do
  currentClientCount <- readMVar clientCount
  conn <- accept sock
  forkIO $ if currentClientCount < 10
              then bracket_ (modifyMVar_ clientCount (return . (+) 1)) (modifyMVar_ clientCount (return . flip (-) 1)) (runConn conn)
              else dropConn conn
  mainLoop clientCount sock

almostForever :: IO a -> IO ()
almostForever action = sequenceA_ $ take 3 $ repeat action

handlePenisLover :: Socket -> IO ()
handlePenisLover sock =
  let deliver = traverse_ (slowlySend sock)
   in almostForever $ deliver penisFrames

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = putStrLn "Accepted. Sending love..." >> finally (handlePenisLover sock) (close sock)

dropConn :: (Socket, SockAddr) -> IO ()
dropConn (sock, _) = putStrLn "Rejected. Let him try again later." >> close sock

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    clientCount <- newMVar 0
    mainLoop clientCount sock
