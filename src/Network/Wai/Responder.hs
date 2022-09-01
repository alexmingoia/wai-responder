-- | Responder is a tiny web application framework for WAI
--
-- - `ResponderM` for composing responses with do notation.
-- - Routing with path captures.
-- - Parameter parsing for cookies, path, query, and body.
-- - Helpers for redirects, headers, status codes, and errors.
--
-- @
-- {-# language OverloadedStrings #-}
--
-- import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Responder
--
-- main :: IO ()
-- main = do
--   run 8080 $ app defaultResponderOpts () $ do
--     get "/" index
--     post "/echo/:name" echo
--
-- index :: ResponderM s a
-- index = html status200 $ "Hello World!"
--
-- echo :: ResponderM s a
-- echo = do
--   name <- param "name"
--   html status200 $ "Hello, " <> name
--
-- missing :: ResponderM s a
-- missing = html status404 $ "Not found..."
-- @
module Network.Wai.Responder
  ( ResponderM,
    app,
    responder,
    defaultResponderOpts,

    -- * Routing
    get,
    put,
    patch,
    post,
    delete,
    route,

    -- * Requests
    param,
    paramEither,
    paramMaybe,
    params,
    queryParam,
    queryParamMaybe,
    queryParamEither,
    queryParams,
    pathParam,
    pathParamMaybe,
    pathParamEither,
    pathParams,
    cookieParam,
    cookieParamMaybe,
    cookieParamEither,
    cookieParams,
    file,
    fileMaybe,
    files,
    fromBody,
    header,
    headers,
    request,

    -- * Responses
    send,
    redirect301,
    redirect302,
    redirect303,
    text,
    html,
    json,
    xml,
    css,
    setHeader,
    setCookie,
    setCookie',
    expireCookie,

    -- * App state
    getAppState,
    setAppState,
    withAppState,

    -- * Errors
    HttpError (..),

    -- * Parameters
    ParsableParam (..),

    -- * Re-exports
    module Network.HTTP.Types,
    module Network.Wai,
    FileInfo (..),
  )
where

import Control.Exception (SomeException, handle)
import Control.Monad.Catch (throwM)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Either.Combinators (rightToMaybe)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import qualified Data.Vault.Lazy as V
import Data.Word (Word64)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse hiding (Param)
import Network.Wai.Request
import System.Environment (lookupEnv)
import Web.Cookie
import Network.Wai.Responder.Internal
import Network.Wai.Responder.Types

defaultResponderOpts :: ResponderOptions
defaultResponderOpts =
  ResponderOptions
    { parseBodyOpts = noLimitParseRequestBodyOptions
    }

app :: ResponderOptions -> s -> ResponderM s a -> Application
app opts appState (ResponderM f) req respond = do
  let st = responderState opts appState req
  eres <- f st
  case eres of
    Left (Respond res) -> respond res
    _ -> respond $ responseLBS status204 [] ""

responder :: ResponderOptions -> s -> ResponderM s a -> Middleware
responder opts appState (ResponderM f) app req respond = do
  let st = responderState opts appState req
  eres <- f st
  case eres of
    Left (Respond res) -> respond res
    _ -> app req respond

get :: PathPattern -> ResponderM s () -> ResponderM s ()
get = route (Just "GET")

put :: PathPattern -> ResponderM s () -> ResponderM s ()
put = route (Just "PUT")

patch :: PathPattern -> ResponderM s () -> ResponderM s ()
patch = route (Just "PATCH")

post :: PathPattern -> ResponderM s () -> ResponderM s ()
post = route (Just "POST")

delete :: PathPattern -> ResponderM s () -> ResponderM s ()
delete = route (Just "DELETE")

-- | Route request matching optional `Method` and `PathPattern` to `ResponderM`.
route :: Maybe Method -> PathPattern -> ResponderM s () -> ResponderM s ()
route method pat rest = do
  req <- getRequest
  case match method pat req of
    Nothing -> return ()
    Just pathParams -> do
      withResponderState $ \st -> st { reqPathParams = pathParams }
      rest

-- | Get a parameter. Looks in query, path, cookie, and body (in that order).
--
-- If no parameter is found or parameter fails to parse, an `HttpError` is thrown.
param :: ParsableParam a => Text -> ResponderM s a
param name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  maybe (throwM (missingParam name)) (either throwM pure . parseParam) pM

-- | Get a parameter or `HttpError` if missing or parse failure.
paramEither :: ParsableParam a => Text -> ResponderM s (Either HttpError a)
paramEither name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  return $ case pM of
    Nothing ->
      Left $ HttpError status422 ("missing parameter: " <> T.unpack name)
    Just p -> parseParam p

-- | Get an optional parameter.
--
-- Returns `Nothing` for missing parameter.
-- Throws `HttpError` on parse failure.
paramMaybe :: ParsableParam a => Text -> ResponderM s (Maybe a)
paramMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  maybe (pure Nothing) (either throwM (pure . Just) . parseParam) pM

-- | Get all parameters from query, path, cookie, and body (in that order).
params :: ResponderM s [Param]
params = concatParams <$> parseBodyForm

-- | Get a query parameter.
--
-- If no parameter is found or parameter fails to parse, an `HttpError` is thrown.
queryParam :: ParsableParam a => Text -> ResponderM s a
queryParam name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> queryParams
  maybe (throwM (missingParam name)) (either throwM pure . parseParam) pM

-- | Get a query parameter or `HttpError` if missing or parse failure.
queryParamEither :: ParsableParam a => Text -> ResponderM s (Either HttpError a)
queryParamEither name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> queryParams
  return $ case pM of
    Nothing ->
      Left $ HttpError status422 ("missing parameter: " <> T.unpack name)
    Just p -> parseParam p

-- | Get an optional query parameter.
--
-- Returns `Nothing` for missing parameter.
-- Throws `HttpError` on parse failure.
queryParamMaybe :: ParsableParam a => Text -> ResponderM s (Maybe a)
queryParamMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> queryParams
  maybe (pure Nothing) (either throwM (pure . Just) . parseParam) pM

-- | Get all query parameters.
queryParams :: ResponderM s [Param]
queryParams = reqQueryParams <$> parseBodyForm

-- | Get a path parameter.
--
-- If no parameter is found or parameter fails to parse, an `HttpError` is thrown.
pathParam :: ParsableParam a => Text -> ResponderM s a
pathParam name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> pathParams
  maybe (throwM (missingParam name)) (either throwM pure . parseParam) pM

-- | Get a path parameter or `HttpError` if missing or parse failure.
pathParamEither :: ParsableParam a => Text -> ResponderM s (Either HttpError a)
pathParamEither name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> pathParams
  return $ case pM of
    Nothing ->
      Left $ HttpError status422 ("missing parameter: " <> T.unpack name)
    Just p -> parseParam p

-- | Get an optional path parameter.
--
-- Returns `Nothing` for missing parameter.
-- Throws `HttpError` on parse failure.
pathParamMaybe :: ParsableParam a => Text -> ResponderM s (Maybe a)
pathParamMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> pathParams
  maybe (pure Nothing) (either throwM (pure . Just) . parseParam) pM

-- | Get all path parameters.
pathParams :: ResponderM s [Param]
pathParams = reqPathParams <$> parseBodyForm

-- | Get a cookie parameter.
--
-- If no parameter is found or parameter fails to parse, an `HttpError` is thrown.
cookieParam :: ParsableParam a => Text -> ResponderM s a
cookieParam name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> cookieParams
  maybe (throwM (missingParam name)) (either throwM pure . parseParam) pM

-- | Get a cookie parameter or error if missing or parse failure.
cookieParamEither :: ParsableParam a => Text -> ResponderM s (Either HttpError a)
cookieParamEither name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> cookieParams
  return $ case pM of
    Nothing ->
      Left $ HttpError status422 ("missing parameter: " <> T.unpack name)
    Just p -> parseParam p

-- | Get an optional cookie parameter.
--
-- Returns `Nothing` for missing parameter.
-- Throws `HttpError` on parse failure.
cookieParamMaybe :: ParsableParam a => Text -> ResponderM s (Maybe a)
cookieParamMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> cookieParams
  maybe (pure Nothing) (either throwM (pure . Just) . parseParam) pM

-- | Get all cookie parameters.
cookieParams :: ResponderM s [Param]
cookieParams = reqCookieParams <$> parseBodyForm

-- | Get uploaded `FileInfo`.
--
-- If missing parameter or empty file, throw `HttpError`.
file :: Text -> ResponderM s (FileInfo BL.ByteString)
file name = maybe (throwM (missingParam name)) pure =<< fileMaybe name

-- | Get optional uploaded `FileInfo`.
--
-- `Nothing` is returned for missing parameter or empty file content.
fileMaybe :: Text -> ResponderM s (Maybe (FileInfo BL.ByteString))
fileMaybe name = do
  fM <- fmap snd . L.find ((==) (encodeUtf8 name) . fst) <$> files
  case fileContent <$> fM of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just _ -> return fM

-- | Get all uploaded files.
files :: ResponderM s [File BL.ByteString]
files = fs . reqBody <$> parseBodyForm
  where
    fs bodyM = case bodyM of
      Just (FormBody (_, fs)) -> fs
      _ -> []

-- | Get the JSON value from request body.
fromBody :: JSON.FromJSON a => ResponderM s a
fromBody = do
  json <- parseBodyJson
  case JSON.fromJSON json of
    JSON.Error msg -> throwM $ HttpError status400 msg
    JSON.Success a -> return a

-- | Get the value of a request `Header`. Header names are case-insensitive.
header :: Text -> ResponderM s (Maybe Text)
header name = do
  let ciname = CI.mk (encodeUtf8 name)
  fmap (decodeUtf8 . snd) . L.find ((==) ciname . fst) <$> headers

-- | Get the request headers.
headers :: ResponderM s [Header]
headers = requestHeaders <$> request

-- | Get the WAI `Request`.
request :: ResponderM s Request
request = getRequest

-- | Send a raw byte string `Response`.
--
-- Sets the Content-Length header.
send :: Status -> BL.ByteString -> ResponderM s a
send status body =
  ResponderM $ \st -> return $ Left (Respond (response st status body))

-- | Send a `Text` response.
--
-- Sets the Content-Type and Content-Length headers.
text :: Status -> Text -> ResponderM s a
text status body = do
  setHeader (hContentType, "text/plain; charset=utf-8")
  send status . BL.fromStrict . encodeUtf8 $ body

-- | Send an HTML response.
--
-- Sets the Content-Type and Content-Length headers.
html :: Status -> BL.ByteString -> ResponderM s a
html status body = do
  setHeader (hContentType, "text/html; charset=utf-8")
  send status body

-- | Send a JSON response using `ToJSON`.
--
-- Sets the Content-Type and Content-Length headers.
json :: ToJSON a => Status -> a -> ResponderM s a
json status body = do
  setHeader (hContentType, "application/json; charset=utf-8")
  send status $ JSON.encode body

-- | Send a CSS response.
--
-- Sets the Content-Type and Content-Length headers.
css :: Status -> BL.ByteString -> ResponderM s a
css status body = do
  setHeader (hContentType, "text/css; charset=utf-8")
  send status body

-- | Send an XML response.
--
-- Sets the Content-Type and Content-Length headers.
xml :: Status -> BL.ByteString -> ResponderM s a
xml status body = do
  setHeader (hContentType, "application/xml; charset=utf-8")
  send status body

-- | Add a `Header` to response.
setHeader :: Header -> ResponderM s ()
setHeader header =
  withResponderState $ \st -> st { resHeaders = header : resHeaders st }

-- | Add a cookie to the response with the given key and value.
--
-- Note: This uses `defaultSetCookie`.
setCookie :: Text -> Text -> ResponderM s ()
setCookie key val =
  let setCookie =
        defaultSetCookie
          { setCookieName = encodeUtf8 key,
            setCookieValue = encodeUtf8 val,
            setCookiePath = Just "/",
            setCookieHttpOnly = True
          }
      header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in withResponderState $ \st -> st { resHeaders = header : resHeaders st }

-- | Add a `SetCookie` to the response.
setCookie' :: SetCookie -> ResponderM s ()
setCookie' setCookie =
  let header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in withResponderState $ \st -> st { resHeaders = header : resHeaders st }

-- | Add a header to expire (unset) a cookie with the given key.
expireCookie :: Text -> ResponderM s ()
expireCookie key =
  let zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      setCookie =
        defaultSetCookie
          { setCookieName = encodeUtf8 key,
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just zeroTime
          }
      header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in withResponderState $ \st -> st { resHeaders = header : resHeaders st }

-- | Send a redirect response with 301 status (Moved Permanently).
redirect301 :: Text -> ResponderM s a
redirect301 url = do
  setHeader (hLocation, encodeUtf8 url)
  send status301 ""

-- | Send a redirect response with 302 status (Found).
redirect302 :: Text -> ResponderM s a
redirect302 url = do
  setHeader (hLocation, encodeUtf8 url)
  send status302 ""

-- | Send a redirect response 303 status (See Other).
redirect303 :: Text -> ResponderM s a
redirect303 url = do
  setHeader (hLocation, encodeUtf8 url)
  send status303 ""

-- | Get app state
getAppState :: ResponderM s s
getAppState = appState <$> getResponderState

-- | Set app state
setAppState :: s -> ResponderM s ()
setAppState s = withResponderState $ \st -> st { appState = s }

-- | Apply setter to app state
withAppState :: (s -> s) -> ResponderM s ()
withAppState f = withResponderState $ \st -> st { appState = f (appState st) }
