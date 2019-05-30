{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


import Core.Program
import Core.Text
import Core.System
import Core.Http

import Network.Http.Types (Request, Response)
import Snap.Core (Snap, MonadSnap)
import qualified Snap.Core as Snap


handleHome :: MonadSnap m => m ()
handleHome = Snap.writeBS "Bada Boom"


-- TODO setup default handlers for 415 etc per the webmachine flowchart

taskStatusHandler :: Handler ()
taskStatusHandler = replyTextPlain "Hello world"

taskResultHandler :: Handler ()
taskResultHandler = handle f
  where
    f :: (Request,Input) -> (Response,Output) -> IO (Response,Output)
    f (q,i) (p,o) = do
        sendUnto o (fromRope "Try again")
        return (p,o)

-- HERE don't need arbitrary route mapping. A WebService is an anchor, id, and fields 
-- with some sensible defaults

service :: WebService
service = basicObjectEndpoint "task"
    [ ("status", taskStatusHandler)
    , ("result", taskResultHandler)
    ]

main :: IO ()
main = execute $ do
    event "Processing..."

    a <- launchWebServer service
    block a

    write "Done"


-- /task/96ea2669-c019-40f6-814c-1ae5c208e336/status

