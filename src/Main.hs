{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator           (load, require)
import           Data.Configurator.Types     (Config, Worth (Required))
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           System.Environment          (lookupEnv)
import           System.Exit                 (exitFailure)

import           Server                      (contextForConfig, makeLogger,
                                              siteServer)

main :: IO ()
main = do
    config <- getConfig
    let ctx = contextForConfig config
    logger <- makeLogger config
    writeStartupLog config
    port <- read <$> require config "port"
    run port $ logger $ simpleCors $ siteServer ctx

getConfig :: IO Config
getConfig = do
    configFile <- lookupEnv "CONFIG_FILE"
    case configFile of
        Just path -> load [Required path]
        Nothing -> do
            putStrLn "$CONFIG_FILE not defined."
            exitFailure

writeStartupLog :: Config -> IO ()
writeStartupLog config = do
    port <- require config "port"
    putStrLn $ "\n---\nServing App on port " ++ port ++ "\n---"
