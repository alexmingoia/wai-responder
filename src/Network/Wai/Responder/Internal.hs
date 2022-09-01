module Network.Wai.Responder.Internal where

import Control.Exception (handle, throwIO)
import Control.Monad (join)
import Control.Monad.Catch (throwM, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 as Char8
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Vault.Lazy as V
import Data.Word (Word64)
import Network.HTTP.Types (Method, Status, hContentLength, hCookie, mkStatus, status204, status422, status400, status413, status500)
import Network.HTTP2.Frame (ErrorCodeId (..), HTTP2Error (..))
import Network.Wai (Application, Middleware, Request (..), Response, lazyRequestBody, queryString, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Parse (File, ParseRequestBodyOptions, lbsBackEnd, parseRequestBodyEx)
import Network.Wai.Request (RequestSizeException (..), requestSizeCheck)
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie, parseCookiesText, renderSetCookie)
import Network.Wai.Responder.Types

responderState :: ResponderOptions -> s -> Request -> ResponderState s
responderState opts appState req =
  ResponderState
    { appState = appState
    , responderOpts = opts
    , waiRequest = req
    , reqBody = Nothing
    , reqCookieParams = parseCookieParams req
    , reqPathParams = []
    , reqQueryParams = decodeQueryParam <$> queryString req
    , resHeaders = []
    , resBody = ""
    }

getRequest :: ResponderM s Request
getRequest = waiRequest <$> getResponderState

getResponderState :: ResponderM s (ResponderState s)
getResponderState = ResponderM $ \st -> return (Right (st, st))

setResponderState :: ResponderState s -> ResponderM s ()
setResponderState st = ResponderM $ \_ -> return (Right ((), st))

withResponderState :: (ResponderState s -> ResponderState s) -> ResponderM s ()
withResponderState f = ResponderM $ \st -> return (Right ((), f st))

concatParams :: ResponderState s -> [Param]
concatParams
  ResponderState
    { reqBody = Just (FormBody (fps, _)),
      reqCookieParams = cps,
      reqPathParams = pps,
      reqQueryParams = qps
    } = qps <> pps <> cps <> fps
concatParams st =
  reqQueryParams st <> reqPathParams st <> reqCookieParams st

match :: Maybe Method -> PathPattern -> Request -> Maybe [Param]
match method (MatchPath f) req
  | maybe True (requestMethod req ==) method = f req
  | otherwise = Nothing

-- | Parse form request body.
parseBodyForm :: ResponderM s (ResponderState s)
parseBodyForm = do
  st <- getResponderState
  case reqBody st of
    Just (FormBody _) -> return st
    _ -> do
      let req = waiRequest st
          opts = parseBodyOpts $ responderOpts st
      (ps, fs) <- liftIO $ wrapErr $ parseRequestBodyEx opts lbsBackEnd req
      let parsedBody = FormBody (decodeBsParam <$> ps, fs)
          st' = st { reqBody = Just parsedBody }
      setResponderState st'
      return st

-- | Parse JSON request body.
parseBodyJson :: ResponderM s JSON.Value
parseBodyJson = do
  st <- getResponderState
  case reqBody st of
    Just (JSONBody json) -> return json
    _ -> do
      let req = waiRequest st
      jsonE <- liftIO $ wrapErr $ JSON.eitherDecode <$> lazyRequestBody req
      case jsonE of
        Left msg -> throwM $ HttpError status400 msg
        Right json -> do
          let st' = st { reqBody = Just (JSONBody json) }
          setResponderState st'
          return json

wrapErr = handle wrapMaxReqErr . handle wrapParseErr

wrapMaxReqErr :: RequestSizeException -> IO a
wrapMaxReqErr (RequestSizeException max) =
  throwIO $ HttpError status413 $
    "Request body size larger than " <> show max <> " bytes."

wrapParseErr :: HTTP2Error -> IO a
wrapParseErr (ConnectionError (UnknownErrorCode code) msg) = do
  let msg' = T.unpack $ decodeUtf8 msg
  throwIO $ HttpError (mkStatus (fromIntegral code) msg) msg'
wrapParseErr (ConnectionError _ msg) = do
  let msg' = T.unpack $ decodeUtf8 msg
  throwIO $ HttpError status500 msg'

parseCookieParams :: Request -> [Param]
parseCookieParams req =
  let headers = snd <$> L.filter ((==) hCookie . fst) (requestHeaders req)
   in join $ parseCookiesText <$> headers

setCookieByteString :: SetCookie -> B.ByteString
setCookieByteString setCookie =
  BL.toStrict (toLazyByteString (renderSetCookie setCookie))

decodeQueryParam :: (B.ByteString, Maybe B.ByteString) -> Param
decodeQueryParam (a, b) = (decodeUtf8 a, maybe "" decodeUtf8 b)

decodeBsParam :: (B.ByteString, B.ByteString) -> Param
decodeBsParam (a, b) = (decodeUtf8 a, decodeUtf8 b)

response :: ResponderState s -> Status -> BL.ByteString -> Response
response st status body =
  let hs = resHeaders st
      hs' = if L.any ((hContentLength ==) . fst) hs then hs else (len : hs)
      len = (hContentLength, Char8.pack (show (BL.length body)))
   in responseLBS status hs' body

missingParam :: Text -> HttpError
missingParam name =
  HttpError status422 $ "missing parameter: " <> T.unpack name
