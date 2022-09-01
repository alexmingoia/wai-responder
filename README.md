# Responder

[![Hackage](https://img.shields.io/hackage/v/wai-responder.svg?style=flat)](http://hackage.haskell.org/package/wai-responder)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

Responder is a tiny web application framework for
[WAI](http://hackage.haskell.org/package/wai).

- `ResponderM` for composing responses and threading state with do notation.
- Routing with path captures.
- Parameter parsing from cookies, path, query, and body.
- Helpers for redirects, headers, status codes, and errors.

```haskell
{-# language OverloadedStrings #-}
--
import Network.Wai.Handler.Warp (run)
import Network.Wai.Responder
--
main :: IO ()
main = do
  run 8080 $ app defaultResponderOpts () $ do
    get "/" index
    post "/echo/:name" echo
--
index :: ResponderM s a
index = html status200 $ "Hello World!"
--
echo :: ResponderM s a
echo = do
  name <- param "name"
  html status200 $ "Hello, " <> name
--
missing :: ResponderM s a
missing = html status404 $ "Not found..."
```

## Prior work

Responder is the successor to [Twain](https://github.com/alexmingoia/twain).
Responder's API parametizes `ResponderM` with application state.
