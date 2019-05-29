{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
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
    , emptyWebService
    , basicObjectEndpoint
    , launchWebServer
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (evaluate)
import qualified Data.ByteString as B
import Snap.Core (Snap, MonadSnap, Request, getRequest, rqClientAddr
    , Response, route, writeBS)
import Snap.Http.Server (simpleHttpServe, Config, ConfigLog(ConfigNoLog)
    , emptyConfig, setAccessLog, setErrorLog, setBind, setPort, setHostname
    , setDefaultTimeout, setVerbose)

import Core.Program.Execute
import Core.Program.Logging
import Core.Program.Unlift
import Core.Text.Rope
import Core.System.Base


data Handler = Handler {
    action :: forall m. MonadIO m => Request -> m Response
}

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

    logResult :: MonadSnap m => Request -> m ()
    logResult req =
      let
        client = rqClientAddr req
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
        req <- getRequest

        writeBS "WOW!"
        logResult req
