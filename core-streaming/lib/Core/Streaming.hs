{-# OPTIONS_HADDOCK not-home #-}

{-|
Tools to do basic operations with and coversions between streaming I/O libraries.

This is intended to be used directly:

@
import "Core.Streaming"
@

as this module re-exports all of the machinery for interoperating with
different frameworks.
-}
module Core.Streaming
    (
        {-* Internal representation -}
{-|
Exposes 'HttpServer', a wrapper around the __snap__ framework's HTTP server monad.
-}
        module Core.Streaming.InputOutput

    ) where

import Core.Streaming.InputOutput

