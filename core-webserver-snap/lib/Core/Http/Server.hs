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
Tooling to take a mapping of routes to handler functions, and expose them
as a web service conforming to expecations of a client consuming a RESTful
API.

Underlying frameworks give you the ability to set up a complete and highly
customized web server, but in the general case you usually just need to
serve resources at predictable (and usually trivial) endpoints.
-}
module Core.Http.Server
    ( WebService
    , Handler(..)
    , Input
    , readEntire
    , Output
    , sendUnto
    , Inbound(fromInput, intoInput)
    , Outbound(fromOutput, intoOutput)
    , handle
    , replyTextPlain
    , emptyWebService
    , basicObjectEndpoint
    , launchWebServer
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (evaluate)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State (StateT(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (foldl')
import Network.Http.Types (Request, Response)
import Network.Http.Internal (Request(..), Response(..))
import Snap.Core (Snap, MonadSnap, route, writeBS)
import qualified Snap.Core as Snap (
      Request, getRequest, rqClientAddr, rqHeaders
    , Response
    )
import Snap.Http.Server (simpleHttpServe, Config, ConfigLog(ConfigNoLog)
    , emptyConfig, setAccessLog, setErrorLog, setBind, setPort, setHostname
    , setDefaultTimeout, setVerbose)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Unlift
import Core.Text.Rope
import Core.Text.Bytes
import Core.System.Base

data Internal = Internal
    { requestMeta :: Request
    , requestBody :: Input
    , responseMeta :: Response
    , responseBody :: Output
    , actualHandler :: Snap ()
    }

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

newtype Handler α = Handler (StateT Internal IO α)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Internal)

modifyUnderlying :: Snap () -> Handler ()
modifyUnderlying further = do
    internal <- get
    let action = actualHandler internal

    let action' = do
                    action
                    further

    put internal { actualHandler = action' }

{-|
Build a layer of a function to handle a web request. The key idea is that a
Handler represents a given web request's metadata (headers), body (if any),
and the building of its response metadata and body. This is made available
to anyone using this 'handle' function to define a 'Handler'; the signature
of the function to be passed:

>     layer :: (Request,Input) -> (Response,Output) -> IO (Response,Output)

makes the point that Handlers can be chained together.
-}
handle :: ((Request,Input) -> (Response,Output) -> IO (Response,Output)) -> Handler ()
handle handler = do
    internal <- get

    let q = requestMeta internal
        i = requestBody internal
        p = responseMeta internal
        o = responseBody internal

    (p',o') <- liftIO (handler (q,i) (p,o))
    put internal { responseMeta = p', responseBody = o' }

data ContentType
    = TextPlain
    | TextHtml
    | ImageJpeg
    | ImagePng
    | ApplicationJson
    | ApplicationPdf
    | ApplicationOctetStream
    | ContentType String

instance Show ContentType where
    show x = case x of
        TextPlain   -> "text/plain"
        TextHtml    -> "text/html"
        ImageJpeg   -> "image/jpeg"
        ImagePng    -> "image/png"
        ApplicationJson -> "application/json"
        ApplicationPdf  -> "application/pdf"
        ApplicationOctetStream  -> "application/octet-stream"
        ContentType other       -> other

{-|
Simplistic handler to set the response to be Content-Type @text/plain@ and
then send the supplied 'Rope'. 
-}
replyTextPlain :: Rope -> Handler ()
replyTextPlain text = handle f
  where
    f (_,_) (p,o) = do
        let p' = setResponseHeader "Content-Type" "text/plain" p
        sendUnto o (fromRope text)
        return (p',o)


setResponseHeader = undefined

{-


runHandler :: Handler α -> Snap α
runHandler handler = do
    request <- Snap.getRequest
    
    
    runState handler initial

  where
    adaptRequest =
      letSnap.rqHeaders 





-}

{-|
A web service. It describes an endpoint which will be handled by the
supplied action.
-}
data WebService = WebService
    { handlersFrom :: [(Rope,Handler ())]
    , serverNameFrom :: Rope
    , portNumFrom :: Int
    }

instance Semigroup WebService where
    (<>) w1 w2 = w2 { handlersFrom = handlersFrom w1 ++ handlersFrom w2 }

instance Monoid WebService where
    mempty = emptyWebService
    mappend = (<>)

emptyWebService :: WebService
emptyWebService = WebService
    { handlersFrom = []
    , serverNameFrom = "localhost"
    , portNumFrom = 58080
    }


basicObjectEndpoint :: Rope -> [(Rope,Handler ())] -> WebService
basicObjectEndpoint anchor pairs =
    emptyWebService { handlersFrom = pairs'}
  where
    pairs' = fmap f pairs
  
    f (field,handler) =
      let
        path = anchor <> "/:identifier/" <> field
      in
        (fromRope path,handler)

{-|
Run a web service. This will be run in its own thread.
-}
launchWebServer :: WebService -> Program t (Thread ())
launchWebServer service = fork $ do
    context <- getContext
    liftIO (launcher context service)

launcher :: Context t -> WebService -> IO ()
launcher context service = do
    simpleHttpServe config server
  where
    config :: Config Snap a
    config
        = setAccessLog ConfigNoLog
        . setErrorLog  ConfigNoLog
        . setVerbose False
        . setHostname (fromRope (serverNameFrom service))
        . setPort (portNumFrom service)
        . setBind "0.0.0.0"
        . setDefaultTimeout 30                              -- why?
        $ emptyConfig

--
-- This is a rip off of the code in Core.Program.Logging's write functions.
-- Doing this directly saves us using Context to unlifting to Program just
-- to run that monad and get the Context back out again.
--

    logResult :: MonadSnap m => Snap.Request -> m ()
    logResult req =
      let
        client = Snap.rqClientAddr req
        msg = intoRope client
        event' = liftIO . subProgram context . event
      in do
        event' msg

    server :: Snap ()
    server =
        route (handlers' service)
      where
        handlers' = fmap (\(path,handler) -> (fromRope path, convertHandler handler)) . handlersFrom

    convertHandler :: Handler a -> Snap ()
    convertHandler _ = do
        req <- Snap.getRequest

        writeBS "WOW!"
        logResult req
