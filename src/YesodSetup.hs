{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module YesodSetup
    ( defaultMain
    ) where

import           Control.Exception
import qualified Data.ByteString.Char8        as ByteString
import qualified Data.HashMap.Strict          as HashMap
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.IO                 as Text
import           Data.Yaml                    (FromJSON (..), Value (..), (.:))
import qualified Data.Yaml                    as Yaml
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           System.Console.ANSI
import           System.Directory
import           System.Directory.ProjectRoot
import           System.Exit
import           System.FilePath

data DatabaseSettings = DatabaseSettings { dsHost     :: Text
                                         , dsPort     :: Text
                                         , dsUser     :: Text
                                         , dsPassword :: Text
                                         , dsName     :: Text
                                         }
    deriving(Generic, Show)

instance FromJSON DatabaseSettings where
    parseJSON (Object o) = DatabaseSettings <$>
        (dropStart <$> (o .: "host")) <*>
        (dropStart <$> (o .: "port")) <*>
        (dropStart <$> (o .: "user")) <*>
        (dropStart <$> (o .: "password")) <*>
        (dropStart <$> (o .: "database"))
      where
        dropStart s =
            let s' = Text.dropWhile (/= ':') s
            in if Text.null s' then s
               else let s'' = Text.dropWhile (/= ':') (Text.tail s')
                    in if Text.null s'' then s'
                       else Text.tail s''
    parseJSON _ = fail "Couldn't parse"

defaultMain :: IO ()
defaultMain = do
    mpr <- getProjectRootCurrent
    pr <- case mpr of
        Just pr -> return pr
        Nothing -> do
            putStrLn "Couldn't find the project root"
            getCurrentDirectory
    putStrLn ("Looking for yesod settings in " ++ takeBaseName pr)
    mvalue <- Yaml.decodeFile
        (pr </> "config/settings.yml") :: IO (Maybe Value)
    case mvalue of
        Just (Object h) -> case HashMap.lookup "database" h of
            Just database -> do
                mdb <- Yaml.parseMonad parseJSON  database
                    :: IO (Maybe DatabaseSettings)
                case mdb of
                    Nothing -> do
                        putStrLn "Failed to parse settings.yml"
                        exitFailure
                    Just db -> do
                        Text.putStrLn ("Connecting to " <> dsHost db <> "...")
                        conn <- connectPostgreSQL (Text.encodeUtf8 (
                                                "host='" <> dsHost db <> "' "
                                                <> "port=" <> dsPort db <> "' "
                                                <> "user='postgres'"
                                                <> "dbname='postgres'"))

                        let handleSqlError (SqlError{..}) = do
                                setSGR [SetColor Foreground Vivid Yellow]
                                ByteString.putStrLn sqlErrorMsg
                                setSGR [Reset]
                                return 10

                        Text.putStrLn ("Creating role " <> dsUser db <> "...")
                        _ <- execute_ conn
                            (fromString (Text.unpack ("create role \"" <> dsUser db <> "\"")))
                            `catch` handleSqlError
                        _ <- execute_ conn
                            (fromString (Text.unpack ("alter role \"" <> dsUser db <> "\" with login")))
                            `catch` handleSqlError
                        Text.putStrLn ("Creating database " <> dsName db <> "...")
                        _ <- execute_ conn
                            (fromString (Text.unpack ("create database \"" <> dsName db <> "\"")))
                            `catch` handleSqlError
                        putStrLn "Setting password..."
                        _ <- execute_ conn
                            (fromString (Text.unpack ("alter role \"" <>
                                                      dsUser db <>
                                                      "\" with password '" <>
                                                      dsPassword db <> "'")))
                            `catch` handleSqlError
                        putStrLn "Granting privileges..."
                        _ <- execute_ conn
                            (fromString (Text.unpack ("grant all privileges on database \"" <>
                                                      dsName db <>
                                                      "\" to \"" <>
                                                      dsUser db <> "\"")))
                            `catch` handleSqlError
                        return ()
            Nothing -> do
                putStrLn "Failed to parse settings.yml"
                exitFailure
        _ -> do
            putStrLn "Failed to parse settings.yml"
            exitFailure
