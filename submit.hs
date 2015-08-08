{-# LANGUAGE
    OverloadedStrings
  #-}

-- import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import System.Environment
import System.IO
import System.Exit

import qualified Game

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      lbs <- LBS.readFile arg
      mgr <- HTTP.newManager HTTPS.tlsManagerSettings -- HTTP.defaultManagerSettings

      mApiToken <- lookupEnv "API_TOKEN"
      password <-
        case mApiToken of
          Nothing -> hPutStrLn stderr "$API_TOKEN is not set" >> exitFailure
          Just s -> return (BS.pack s)
      let user = ""
      
      initReq <- HTTP.parseUrl "https://davar.icfpcontest.org/teams/110/solutions"
      let req = HTTP.applyBasicAuth user password $ initReq
                { HTTP.method = "POST"
                , HTTP.requestBody = HTTP.RequestBodyLBS lbs
                , HTTP.requestHeaders = ("Content-Type", "application/json") : HTTP.requestHeaders initReq
                }
      res <- HTTP.httpLbs req mgr
      print $ HTTP.responseStatus res 
      print $ HTTP.responseHeaders res
      LBS.putStrLn $ HTTP.responseBody res
