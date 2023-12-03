{-# LANGUAGE OverloadedStrings #-}
module Input (fetchInput) where

import System.Directory
import Network.HTTP.Simple
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Logger
import qualified Data.ByteString
import System.Environment (lookupEnv)
import Data.ByteString (pack)
import Data.Char (ord)

-- From https://mmhaskell.com/blog/2023/1/30/advent-of-code-fetching-puzzle-input-using-the-api
isCached :: FilePath -> IO Bool
isCached fp = do
  exists <- doesFileExist fp
  if not exists
    then return False
    else do
    size <- getFileSize fp
    return $ size > 0

fetchInputToFile :: (MonadLogger m, MonadThrow m, MonadIO m) => Int -> Int -> FilePath -> m ()
fetchInputToFile year day fp = do
  cached <- liftIO $ isCached fp
  token <- liftIO $ lookupEnv "AOC_TOKEN"
  case (cached, token) of
    (True, _) -> logDebugN "input is cached!"
    (False, Nothing) -> logErrorN "Not cached but didn't find session token!"
    (False, Just token') -> do
        let route = "http://www.adventofcode.com/" ++ show year ++ "/day/" ++ show day ++ "/input"
        req <- parseRequest route
        let req' = addRequestHeader "cookie" t req
        resp <- getResponseBody <$> httpBS req'
        liftIO $ Data.ByteString.writeFile fp resp
     where
       t = pack (map (fromIntegral . ord) token')

fetchInput :: Int -> Int -> FilePath -> IO String
fetchInput year day fp = do
  runStdoutLoggingT $ fetchInputToFile year day fp
  readFile fp
