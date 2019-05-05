{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import qualified Data.ByteString.Char8 as C

import Core.Program
import Core.Text
import Core.System
import Core.Http

import Snap.Core


handleHome :: MonadSnap m => m ()
handleHome = writeBS "Bada Boom"

routes :: MonadSnap m => [(Rope,m ())]
routes =
    [ ("hello", handleHome)
    , ("/", handleHome)
    ]

-- TODO setup default handlers for 415 etc per the webmachine flowchart

service :: WebService ()
service = emptyWebService

main :: IO ()
main = execute $ do
    event "Processing..."

    a <- launchWebServer service
    block a

    write "Done"
