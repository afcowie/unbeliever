{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}        -- FIXME
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- FIXME
{-# OPTIONS_HADDOCK prune #-}

{-|
Streaming I/O is the necessary antidote to the chronic disease of laziness.

As is often the case in the Haskell ecosystem, there are numerous
approaches to doing streaming I/O. While for some time there was
considerable competition between the teams working on the different
alternatives, it was fascinating to see them mostly converge on similar
usage patterns and even share internal implementation details. Indeed, most
libraries include a section in their documentation of how to convert from
one streaming framework to another. This module's 'Inbound' and 'Outbound'
typeclasses represent these conversions.

Most of the streaming I/O frameworks are polymorphic in the item being
iterated over; here we concentrate on the task of getting streams of
/bytes/ in or out of your program
-}
module Core.Streaming.InputOutput
    ( Input
    , readEntire
    , Output
    , sendUnto
    , Inbound(fromInput, intoInput)
    , Outbound(fromOutput, intoOutput)
    ) where

import Control.Exception (evaluate)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (foldl')
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Core.Text.Rope
import Core.Text.Bytes
import Core.System.Base

{-|
An output stream.
-}
newtype Input = Input (InputStream B.ByteString)

{-|
Reads the /entire/ input. This is often all you need when handling small
requests, but betwere that this will lead to significant memory pressure on
large inputs.
-}
-- Huh. Y'know? Redoing Bytes as a FingerTree over ShortByteString would be
-- interesting.
readEntire :: Input -> IO Bytes
readEntire (Input i) = do
    bs <- Streams.toList i
    let b' = foldl' (\acc b -> acc <> Builder.byteString b) mempty bs
    return (intoBytes b')

{-|
An output stream. This is /not/ a pure value; once data is written to it
you can't un-send it.
-}
newtype Output = Output (OutputStream B.ByteString)

{-|
Send the given data to the output stream.

If you need to send a simple text string the easiest way to do so is using
'fromRope` on a string literal:

>      sendUnto o (fromRope "That is all")

-}
sendUnto :: Output -> Bytes -> IO ()
sendUnto (Output o) b = Streams.write (Just (fromBytes b)) o

{-|
Adapt a streaming I/O type that can be read from into or from an 'Input'.
-}
class Inbound a where
    intoInput :: a -> Input
    fromInput :: Input -> a

instance Inbound (Input) where
    intoInput = id
    fromInput    = id

instance Inbound (InputStream B.ByteString) where
    intoInput i = Input i
    fromInput (Input i) = i

{-|
Adapt a streaming I/O type that can be written to into an 'Output'.
-}
class Outbound a where
    intoOutput :: a -> Output
    fromOutput :: Output -> a

instance Outbound (OutputStream B.ByteString) where
    intoOutput o = Output o
    fromOutput (Output o) = o

