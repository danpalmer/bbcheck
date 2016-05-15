{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import Network.Wai.Handler.Warp    (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.Configurator (load, require)
import Data.Configurator.Types (Config, Worth(Required))
import Control.Monad (liftM)

import Server (contextForConfig, makeLogger, siteServer)

main :: IO ()
main = do
    config <- getConfig
    ctx <- contextForConfig config
    logger <- makeLogger config
    writeStartupLog config
    port <- liftM read $ require config "port"
    run port $ logger $ simpleCors $ siteServer ctx

getConfig :: IO (Config)
getConfig = do
    configFile <- lookupEnv "CONFIG_FILE"
    case configFile of
        Just path -> do
            config <- load [Required path]
            return config
        Nothing -> do
            putStrLn "$CONFIG_FILE not defined."
            exitFailure

writeStartupLog :: Config -> IO ()
writeStartupLog config = do
    port <- require config "port"
    putStrLn $ "\n---\nServing App on port " ++ port ++ "\n---"
