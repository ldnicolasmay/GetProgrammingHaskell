module Main where

-- {-# LANGUAGE OverloadedStrings #-} -- in `package.yaml`

-- import Lib
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types
-- import Data.CaseInsensitive as CI

myToken :: BC.ByteString
myToken = "YhiugufSrfXeMrwCoeEwxuClPgWIumzl"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

-- main :: IO ()
-- -- main = someFunc
-- main = print "hi"

-- GHCI> import Network.HTTP.Simple
-- GHCI> response = httpLBS "http://news.ycombinator.com"

-- GHCI> getResponseStatusCode <$> response 
-- 200

-- GHCI> getResponseHeaders <$> response
-- [("Server","nginx"),("Date","Tue, 12 Nov 2019 02:38:56 GMT"),("Content-Type","text/html; charset=utf-8"),("Transfer-Encoding","chunked"),("Connection","keep-alive"),("Vary","Accept-Encoding"),("Cache-Control","private; max-age=0"),("X-Frame-Options","DENY"),("X-Content-Type-Options","nosniff"),("X-XSS-Protection","1; mode=block"),("Referrer-Policy","origin"),("Strict-Transport-Security","max-age=31556900"),("Content-Security-Policy","default-src 'self'; script-src 'self' 'unsafe-inline' https://www.google.com/recaptcha/ https://www.gstatic.com/recaptcha/ https://cdnjs.cloudflare.com/; frame-src 'self' https://www.google.com/recaptcha/; style-src 'self' 'unsafe-inline'"),("Content-Encoding","gzip")]

-- GHCI> getResponseHeader "date" <$> response
-- ["Tue, 12 Nov 2019 02:40:26 GMT"]

--------------------------------------------------------------------------------

buildRequest :: BC.ByteString ->
                BC.ByteString ->
                BC.ByteString ->
                BC.ByteString ->
                Request
buildRequest token host method path =
  setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath


-- main :: IO ()
-- main = do
--   response <- httpLBS request
--   let status = getResponseStatusCode response
--   if status == 200
--     then do
--       print "saving request to file"
--       let jsonBody = getResponseBody response
--       L.writeFile "data.json" jsonBody
--     else do
--       print "request failed with error"


-- Q39.1

buildRequestNoSSL :: BC.ByteString ->
                     BC.ByteString ->
                     BC.ByteString ->
                     BC.ByteString ->
                     Request
buildRequestNoSSL token host method path =
  setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure False
  $ setRequestPort 80
  $ defaultRequest


-- Q39.2

main :: IO ()
main = do
  --
  response  <- httpLBS request
  let status = getResponseStatus response
  let sCode  = statusCode status
  let sMssg  = statusMessage status
  --
  if sCode == 200
    then do
      putStrLn "Saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else do
      print sMssg


