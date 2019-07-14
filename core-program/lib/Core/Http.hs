{-# OPTIONS_HADDOCK not-home #-}

{-|
Tools to set up a simple HTTP based RESTful web service.

This is intended to be used directly:

@
import "Core.Http"
@

as this module re-exports all of the various components making up this
library's convenience wrapper of a RESTful HTTP server.
-}
module Core.Http
    (
        {-* Internal representation -}
{-|
Exposes 'HttpServer', a wrapper around the __snap__ framework's HTTP server monad.
-}
        module Core.Http.Server

    ) where

import Core.Http.Server

