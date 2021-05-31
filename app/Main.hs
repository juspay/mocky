{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (unwords, length, map)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsMemory)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
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
import Data.ByteString as BS (ByteString, concat)
import Data.Map (Map, map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.IO.Unsafe(unsafePerformIO)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import Data.ByteString.Base64 (decodeBase64)
import Data.Either.Combinators (fromRight')
import Control.Concurrent

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
  forkIO $ run 3000 application
  runTLS (tlsSettingsMemory cert key) (setPort 443 defaultSettings) application

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

cert = fromRight' $ decodeBase64 $ "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSURFVENDQWZrQ0ZIdUFCOXNqMDhPeGFHdTlEQmRzaEpONnNNMUVNQTBHQ1NxR1NJYjNEUUVCQ3dVQU1FVXgKQ3pBSkJnTlZCQVlUQWtGVk1STXdFUVlEVlFRSURBcFRiMjFsTFZOMFlYUmxNU0V3SHdZRFZRUUtEQmhKYm5SbApjbTVsZENCWGFXUm5hWFJ6SUZCMGVTQk1kR1F3SGhjTk1qRXdOREl6TVRNd09URTNXaGNOTWpFd05USXpNVE13Ck9URTNXakJGTVFzd0NRWURWUVFHRXdKQlZURVRNQkVHQTFVRUNBd0tVMjl0WlMxVGRHRjBaVEVoTUI4R0ExVUUKQ2d3WVNXNTBaWEp1WlhRZ1YybGtaMmwwY3lCUWRIa2dUSFJrTUlJQklqQU5CZ2txaGtpRzl3MEJBUUVGQUFPQwpBUThBTUlJQkNnS0NBUUVBdkYwc0l6S2RsTVY1bnE0bHhSa0tZWjlVWGRJY0ZYWkJHczJsWDk3dGxsbExGYUxLCnI1MVBkSjVodG9aZGlKRFZHV0VGYUk5NmFqMlA5dXdmaXFjTVhCZnVibkQ5TFR6ZWQ5S1E0UE9jSm9lRGFKL3kKR25UdTJrNkhYSStxZXRJMDRDWmJsMnFoTnUwTG14VW4rcGVnOW9lVWtqb2ZOZmlGQ2szQUpWMlVmZmZ1blJPcwp2VlFuM3EwbmlVTVlIVnZjQVYwcDBhN05aVUlnaSsxbnlIU0R2blhwUitqcGFoYkhJb1RzdEpvT3hEcWJST1FRCmxEamIzdzQ2dnBOM1gxQ1prRGwxMG5GOG9heG54RFpDczlPK1hMSUduanRVT0gwTFdZMFJqbFJFSU5WRUZ2cWYKQWQ5WWdJTGxoRERwMGE2Z0MxN3VxR1c2WVVYQ0dOL1BxU3c0YXdJREFRQUJNQTBHQ1NxR1NJYjNEUUVCQ3dVQQpBNElCQVFCZE1qWDdYQksrTE1KWFdjVEwwNWJ6ajlDa0trY2Job0NzaU5rOFcxcmErNXoyb3lpZlJSZTRaaVRQCno3WU1wd1VEQzUxRDcxcGhTa05sZ3hlYjM0d1ZFRWdGbHlvNXRCY29PaXhUcjRCQVhwdmRHbVpFRGhhaE9ScWEKelIyQjVGUUtjYWpGL3JTOGVmRWpJb2R5UXM4YzhXbmtZY04reUQvTSs5eVBua0cyUGJXY2M1WTY2bmxQZ3RCSgp3aDVlcVduaG5XNVVoeDFQRkFwQlZHbllkSXpWd2RpVlBWRE9pNlJpWU5jc1h5WHNPd2J1QnRjUVVOM1JzWTl3Cnh0V3F2UXZFNlZQMTlHMUsxek5GNTgwOGFzT0kySWsrSWpwL0NTaE01NGFYdXd6T0JvVFJaSzZROFB6enVkZ2QKcVNQVTIyMEZiRVlrbnMvUWtyV1drWllmdU1mYQotLS0tLUVORCBDRVJUSUZJQ0FURS0tLS0tCg=="



key = fromRight' $ decodeBase64 $ "LS0tLS1CRUdJTiBSU0EgUFJJVkFURSBLRVktLS0tLQpNSUlFb2dJQkFBS0NBUUVBdkYwc0l6S2RsTVY1bnE0bHhSa0tZWjlVWGRJY0ZYWkJHczJsWDk3dGxsbExGYUxLCnI1MVBkSjVodG9aZGlKRFZHV0VGYUk5NmFqMlA5dXdmaXFjTVhCZnVibkQ5TFR6ZWQ5S1E0UE9jSm9lRGFKL3kKR25UdTJrNkhYSStxZXRJMDRDWmJsMnFoTnUwTG14VW4rcGVnOW9lVWtqb2ZOZmlGQ2szQUpWMlVmZmZ1blJPcwp2VlFuM3EwbmlVTVlIVnZjQVYwcDBhN05aVUlnaSsxbnlIU0R2blhwUitqcGFoYkhJb1RzdEpvT3hEcWJST1FRCmxEamIzdzQ2dnBOM1gxQ1prRGwxMG5GOG9heG54RFpDczlPK1hMSUduanRVT0gwTFdZMFJqbFJFSU5WRUZ2cWYKQWQ5WWdJTGxoRERwMGE2Z0MxN3VxR1c2WVVYQ0dOL1BxU3c0YXdJREFRQUJBb0lCQUhZUEg3N2Yxb3lNU1BILwp6SVNBY1ZWR2U2WmlMbC8xUWxEK1c0eC9PUUptN1B5ZWo5TUtFZWJSN2dSc3F1c1JlNzd3Wkdkb3BoMDRnS2NPCno2WWpsVWVUYUVtb1g4YWV4QUR0NVFoWko2R3VoZHBwaFdDM3NZejZjelM4aHVsOEhzL3V1ckZvajZyUlg5alAKcGNNV2pqaDJPZFI4d1ZzZWJLUk1ud1hNWldtZGl2UlYyQUtqNDh4bjZyUlJIWTF2eVVDdXlGMVhydFpZQmhJNQpoQnk0QUlsa2lhZW1hYVlob2dkUUpLMUUzWFNrRzczVkhBdVBSbnJIcXdmM2N6SWRqSVRxR1hCellKd2Zjend1CnJ2V21Eak5McEF2T0w4eW0wNmt2a1RNdWIyWkdtai94OUxFb2V4ait1dnJwbW8xMmRVcE1nQ0F1dm9xbDl5eE0KaHkzMVVHRUNnWUVBOU9aT3NpMDc4cWovMDMxazVqUEEzb05INUp5ZUNiTDVXOUd5czhBdXZxTFRuYnlucUdaSQplVTF1VXVHTFptU0hoL2M4cG95dU5NTVo3UzIvRmdjTVo0aDdvZTcxSWcxZ1cvYmREVzVBaWtDVTVwQWhvOXdwCmVMeFNtT3NMV2ZKeVpEWGE2V0taR29lcmRUU1g2ZmdsZ1lqZWRBaEtOcGJObFI0aW9ZdjdhazhDZ1lFQXhPYmEKY05tUDUxUVJXY0QweTFaS0RYZ21VTVlVV21ndEhvSlhGVkZkWlN2QnIraVhFK29JaWNHNnd0TVNsRjc5NHRwNQpqcXZueHJ0VzZaT0JxM004UUYvTS9ybnl3d2xkZWNyK1B2My9FdkgvSFVRMVNjaU50TVJ5ZVFVQ3BjdWczdVFwCitmV01ONkY2UUxYaEhHeTJaaFBOM3FOT05FaEk4clNRaU9xVXRTVUNnWUE5RURQdEgzUkVKRGx0cldWOE5NbFIKY2lCNTgwZEFtZHl0TlZNK21KdlBNdUJiQTFBMm9CSlFGS3QraFBoeU5uekl2Uy9sUTB5b29VRVVyNFJtNUtrRwpJN1RZQXJJaXBIMHpVUkY3S05OMDVYZDdLZE1KZlhybHZPZEhjOHZueUprWFBZcHhDUUd0Nk1ncXVFdHNybEFECnhYSGFNcnIyYzVHZExkOXFzbkdrNXdLQmdBNDhLbHF5UCtjWGV0R2tVNDVIeEw3SWl1eVlCV3lyejJjL1FSQjMKZTJzUENNZXc1d3MzaGpwL1dRTXd3MDBhdVB3bmsxWXZNMmg0cnNYZ05pV3hCUkFhWFBXbGQ5bUYyYzR1VU1jNQpFYkhLOFFpcW43ZWYyaTdSYlAwMjBZUXUvb2hlc25JeHdGdGRQenRpYXMzbFJpS0hHQUVhaVZMYVR2dk05ZzRQCjk0UkpBb0dBRGZHVXcxb1ZqSXZQVUpUVllZM1hVVkN0K0RmejFlN2NPeTlHaEFJSnVzakU3K1ZKM0o2TzlEU2MKcEJKWitiZUJ2blh1REtQZXNJN3VJTkhJUjRLZ2x6ZFVVWkZ0SjhIMnlVY2EwUS9sOERHSlRUckNtakVWbjJPZgo5Q0tqQXNzdnNQQ05YWktURFhpckVrTXJPVC8rVFNqQkV2TnRkUlFMNE83VWVZRHpLYm89Ci0tLS0tRU5EIFJTQSBQUklWQVRFIEtFWS0tLS0tCg=="

