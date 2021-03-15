{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Responder
  ( module Network.HTTP.Types,
    Responder,
    runResponder,
    parseRequest,
    getEnv,
    getCookie,
    getHeader,
    getMethod,
    getPath,
    getRawPath,
    getParam,
    getFile,
    getRequest,
    getQueryParam,
    hasQueryParam,
    setCookie,
    redirect301,
    redirect302,
    redirect303,
    ok204,
    html,
    plaintext,
    json,
    attachment,
    send,
    FileInfo (..),
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators (mapRight)
import Data.Int
import qualified Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Parse
import Web.Cookie

data ParsedRequest
  = ParsedRequest
      { reqParams :: [Param],
        reqFiles :: [File BL.ByteString],
        waiReq :: Request
      }

-- | Responder is a Reader-like monad that can "short-circuit" and return a WAI
-- response while computing the result from a given environment. This provides
-- convenient branching with do notation for redirects, error responses, etc.
--
-- Also provided are various utility functions for working with requests and
-- responses.
newtype Responder e m a = Responder (ParsedRequest -> e -> m (Either Response a))

instance (MonadIO m) => Functor (Responder e m) where
  fmap f (Responder g) = Responder $ \r -> \e -> mapRight f `fmap` g r e

instance (MonadIO m) => Applicative (Responder e m) where
  pure = return
  (<*>) = ap

instance (MonadIO m) => Monad (Responder e m) where
  return a = Responder $ \_ -> \_ -> return (Right a)
  (Responder act) >>= fn = Responder $ \r -> \e -> do
    eres <- act r e
    case eres of
      Left res -> return (Left res)
      Right a -> do
        let (Responder fres) = fn a
        fres r e

instance MonadIO (Responder e IO) where
  liftIO act = Responder $ \_ -> \_ -> act >>= \a -> return (Right a)

-- | Get the responder environment.
getEnv :: (MonadIO m) => Responder e m e
getEnv = Responder $ \_ -> \e -> return (Right e)

getParsedReq :: (MonadIO m) => Responder e m ParsedRequest
getParsedReq = Responder $ \r -> \_ -> return (Right r)

-- | Get the 'Request'.
getRequest :: (MonadIO m) => Responder e m Request
getRequest = Responder $ \r -> \_ -> return (Right (waiReq r))

-- | Get the first header value for 'HeaderName'.
getHeader :: (MonadIO m) => HeaderName -> Responder e m (Maybe Text)
getHeader name = do
  req <- getRequest
  return (decodeUtf8 . snd <$> L.find ((==) name . fst) (requestHeaders req))

-- | Get the header values for 'HeaderName'.
getHeaders :: (MonadIO m) => HeaderName -> Responder e m [Text]
getHeaders name = do
  req <- getRequest
  return (decodeUtf8 . snd <$> L.filter ((==) name . fst) (requestHeaders req))

-- | Get the cookie value for 'CookieName'.
getCookie :: (MonadIO m) => CookieName -> Responder e m (Maybe Text)
getCookie name = do
  req <- getRequest
  let cookies = snd <$> L.find ((==) hCookie . fst) (requestHeaders req)
  return (decodeUtf8 . snd <$> (L.find ((==) name . fst) . parseCookies =<< cookies))

-- | Get the 'Request' method.
getMethod :: (MonadIO m) => Responder e m Text
getMethod = decodeUtf8 . requestMethod <$> getRequest

-- | Get the URL-decoded 'Request' path pieces separated by "/".
getPath :: (MonadIO m) => Responder e m [Text]
getPath = fmap urlDecodePlus . pathInfo . waiReq <$> getParsedReq
  where
    urlDecodePlus = decodeUtf8 . urlDecode True . encodeUtf8

-- | Get the raw 'Request' path.
getRawPath :: (MonadIO m) => Responder e m B.ByteString
getRawPath = rawPathInfo . waiReq <$> getParsedReq

-- | Get a request body parameter value.
--
-- 'Nothing' is returned for parameters with an empty ("") value.
getParam :: (MonadIO m) => ByteString -> Responder e m (Maybe Text)
getParam name = do
  params <- reqParams <$> getParsedReq
  return (emptyToNothing =<< (decodeUtf8 . snd <$> L.find ((==) name . fst) params))
  where
    emptyToNothing "" = Nothing
    emptyToNothing x = Just x

-- | Get a file from the parsed request body.
getFile :: (MonadIO m) => ByteString -> Responder e m (Maybe (FileInfo BL.ByteString))
getFile name = do
  files <- reqFiles <$> getParsedReq
  let fileInfo = (snd <$> L.find ((==) name . fst) files)
  case fileInfo of
    Nothing -> return Nothing
    Just fi -> return $ if B.null (fileName fi) || (fileName fi) == "\"\"" then Nothing else Just fi

-- | Get a query parameter value.
--
-- 'Nothing' is returned for query parameters with no value.
getQueryParam :: (MonadIO m) => ByteString -> Responder e m (Maybe Text)
getQueryParam name = do
  qs <- queryString <$> getRequest
  return (decodeUtf8 <$> (emptyToNothing =<< snd =<< L.find ((==) name . fst) qs))
  where
    emptyToNothing "" = Nothing
    emptyToNothing x = Just x

-- | Check if request has a query parameter.
hasQueryParam :: (MonadIO m) => ByteString -> Responder e m Bool
hasQueryParam name = do
  qs <- queryString <$> getRequest
  return (isJust (L.find ((==) name . fst) qs))

addHeader :: HeaderName -> ByteString -> Response -> Response
addHeader name val = mapResponseHeaders (\headers -> (name, val) : headers)

-- | Add a cookie to the 'Response'.
setCookie :: SetCookie -> Response -> Response
setCookie cookie =
  let cookieHeader = (hSetCookie, B.concat (BL.toChunks (toLazyByteString (renderSetCookie cookie))))
   in mapResponseHeaders (\hs -> cookieHeader : hs)

redirect301 :: Text -> Response
redirect301 path = responseLBS status301 [(hLocation, encodeUtf8 path)] ""

redirect302 :: Text -> Response
redirect302 path = responseLBS status302 [(hLocation, encodeUtf8 path)] ""

redirect303 :: Text -> Response
redirect303 path = responseLBS status303 [(hLocation, encodeUtf8 path)] ""

ok204 :: Response
ok204 = responseLBS status204 [] ""

html :: Status -> BL.ByteString -> Response
html status html =
  let contentType = (hContentType, "text/html; charset=utf-8")
      contentLength = (hContentLength, Char8.pack (show (BL.length html)))
   in responseLBS status [contentType, contentLength] html

json :: Status -> BL.ByteString -> Response
json status json =
  let contentType = (hContentType, "application/json; charset=utf-8")
      contentLength = (hContentLength, Char8.pack (show (BL.length json)))
   in responseLBS status [contentType, contentLength] json

plaintext :: Status -> BL.ByteString -> Response
plaintext status text =
  let contentType = (hContentType, "text/plain; charset=utf-8")
      contentLength = (hContentLength, Char8.pack (show (BL.length text)))
   in responseLBS status [contentType, contentLength] text

attachment :: MimeType -> FileName -> FileContent -> Response
attachment mimetype filename content =
  let contentType = (hContentType, mimetype <> "; charset=utf-8")
      contentLength = (hContentLength, Char8.pack (show (BL.length content)))
      contentDisposition = (hContentDisposition, "attachment; filename=\"" <> filename <> "\"")
   in responseLBS status200 [contentType, contentDisposition, contentLength] content

send :: (MonadIO m) => Response -> Responder e m a
send res = Responder $ \_ -> \_ -> return (Left res)

type MaxRequestSizeBytes = Int64

type CookieName = ByteString

type FileName = ByteString

type FileContent = BL.ByteString

type MimeType = ByteString

-- Parse a WAI 'Request' into 'ParsedRequest' used to run a 'Responder'.
parseRequest :: MaxRequestSizeBytes -> Request -> IO ParsedRequest
parseRequest max req = do
  let parseBodyOpts = setMaxRequestFileSize max (setMaxRequestNumFiles 99 defaultParseRequestBodyOptions)
  (params, files) <- parseRequestBodyEx parseBodyOpts lbsBackEnd req
  return $
    ParsedRequest
      { reqParams = params,
        reqFiles = files,
        waiReq = req
      }

-- | Run the 'Responder' and retrieve result.
runResponder :: (MonadIO m) => e -> ParsedRequest -> Responder e m a -> m (Either Response a)
runResponder env req (Responder f) = f req env
