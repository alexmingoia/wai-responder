# wai-responder

> Simple interface for composing WAI responses.

Responder is a Reader-like monad that can "short-circuit" and return a WAI
response while computing the result from a given environment. This provides
convenient branching with do notation for redirects, error responses, etc.

Also provided are various utility functions for working with requests and
responses.
