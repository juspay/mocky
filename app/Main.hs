{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (unwords, length, map)
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 (fromStrict, toStrict)
import Data.ByteString.Builder (Builder, stringUtf8)
import Data.ByteString.Builder.HTTP.Chunked (chunkedTransferEncoding, chunkedTransferTerminator)
import Data.Aeson hiding (Encoding)
import Data.CaseInsensitive(mk)
import Data.Functor (($>))
import Data.Foldable (fold)
import Data.IORef(IORef, writeIORef, readIORef, newIORef)
import Data.Maybe (fromMaybe)
-- import Data.Char (toLower)
import Data.List (intersperse, find)
import Data.Text (Text, unwords, unpack, toLower, length, pack)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import Data.Map (Map, map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.IO.Unsafe(unsafePerformIO)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))


string :: String -> String
string = id

text :: Text -> Text
text = id

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond = do
  reqString <- getRequestBodyChunk request
  print "request received:"
  putStr "reqString " >> print reqString
  let req = getRequest reqString
  putStr "req " >> print req
  let path = pathInfo request
  print $ "path: " <> text "/" <> fold (intersperse "/" path)
  resp <- getResponse req path reqString
  print $ "response: "  <> show resp
  let
    enc = getEncoding resp
    builderbody = mkBuilder resp enc
  putStr "enc " >> print enc
  let res = responseBuilder (mkStatus (responseCode resp) "") (convertHeader <$> fromMaybe [] (responseHeader resp)) builderbody
  putStr "res headers " >> print (responseHeaders res)
  respond res


getEncoding :: MockResponse -> Encoding
getEncoding (MockResponse _ Nothing _) = Standard
getEncoding (MockResponse _ (Just []) _) = Standard
getEncoding (MockResponse _ (Just xs) _) = maybe (Standard) (\x -> if (isChunked $ snd $ x) then Chunked else Standard ) (find (isTransferEncoding . fst) xs)

addLength :: Map Text MockResponse -> Map Text MockResponse
addLength m = map adder m
  where
    adder :: MockResponse -> MockResponse
    adder resp = case getEncoding resp of
      Chunked -> resp
      Standard ->
        let header = responseHeader resp
            body = responseBody resp
            updatedHeader = case header of
              Nothing -> Just [("Content-Length", pack $ show $ length body)]
              Just hs -> case find (isContentLength . fst) hs of
                Nothing -> (("Content-Length", pack $ show $ length body):) <$> header
                Just cl -> header
        in resp{responseHeader = updatedHeader}

isContentLength :: Text -> Bool
isContentLength x = toLower x == "content-length"

isTransferEncoding :: Text -> Bool
isTransferEncoding x = toLower x == "transfer-encoding"

isChunked :: Text -> Bool
isChunked x = toLower x == "chunked"

mkBuilder :: MockResponse -> Encoding -> Builder
mkBuilder (MockResponse _ _ body) Standard = textBuilder body
mkBuilder (MockResponse _ _ body) Chunked = (chunkedTransferEncoding $ textBuilder body) <> chunkedTransferTerminator

textBuilder :: Text -> Builder
textBuilder = stringUtf8 . unpack

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  -- print $ "example json"
  -- print $ encode $ defaultMockResponse { responseEncoding = Chunked }
  -- print $ encode $ defaultMockResponse
  print "starting server"
  run 3000 application

convertHeader :: (Text,Text) -> (HeaderName,ByteString)
convertHeader (x,y) = (mk $ encodeUtf8 x, encodeUtf8 y)

getRequest request = eitherDecode . fromStrict $ request :: Either String (Map Text MockResponse)

data Encoding = Standard | Chunked
  deriving stock (Generic, Show, Eq)
  deriving anyclass(ToJSON, FromJSON)

data MockResponse = MockResponse
  { responseCode :: Int
  , responseHeader :: Maybe [(Text,Text)]
  , responseBody :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass(ToJSON, FromJSON)

mkMockResponse :: Text -> MockResponse
mkMockResponse x =
  MockResponse
    { responseCode = 200
    , responseHeader = Nothing
    , responseBody = x
    }

defaultMockResponse = mkMockResponse "{ \"message\": \"Default Body\"}"

updateResponse = mkMockResponse "Store Updated"

errorResponse :: MockResponse
errorResponse =
  MockResponse
    { responseCode = 500
    , responseHeader = Nothing
    , responseBody = "there was an error processing your request"
    }

{-# NOINLINE globalStore #-}
globalStore :: IORef (Map Text MockResponse)
globalStore = unsafePerformIO $ newIORef Map.empty

getResponse :: Either String (Map Text MockResponse) -> [Text] -> ByteString -> IO MockResponse
getResponse request path reqString = do
  currentValue <- readIORef globalStore
  putStr "path " >> print path
  let isset = path == [ "set" ]
  let isget = path == [ "get" ]
  if isset
    then setNew
    else
      if isget then sendStore else sendOld
  where
    sendStore = do
      ref <- readIORef globalStore
      let response = encode ref
      pure $ mkMockResponse $ decodeUtf8 $ toStrict response
    setNew = do
      print "setting global store..."
      case request of
        Left e -> do
          print $ "Json decode error: " <> e
          pure errorResponse
        Right m -> do
          writeIORef globalStore $ addLength m
          print $ "written to Global Store: " <> show (addLength m)
          pure $ updateResponse
    sendOld = do
      let pathStr = fold $ intersperse "/" path
      print "checking path - incoming"
      print path
      ref <- readIORef globalStore
      let response = fromMaybe defaultMockResponse  $ Map.lookup pathStr ref
      print "path map keys:"
      print $ Map.keys ref
      pure response


