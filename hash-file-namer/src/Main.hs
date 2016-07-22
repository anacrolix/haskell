module Main where

import System.Directory
import Text.Regex.PCRE
import System.FilePath
import Control.Monad
import Crypto.Hash.MD5 (hashlazy)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16 (encode)
import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Environment

data Flag = Rehash

options = [ 
  Option "u" ["rehash"] (NoArg Rehash) "rehash" ]
main :: IO ()
main = do 
  args <- getArgs
  let (opts, pos, errs) = getOpt Permute options args
  print errs
  return ()
  hashFileNames "."

hashFileNames dir = do
  dc <- listDirectory dir
  forM_ dc $ \e -> do
    when (needsHashing e) $ do
      putStrLn $ "hashing " ++ (show e)
      digest <- hash e
      let newName = withHash e digest
      putStrLn $ "renaming " ++ (show e) ++ " to " ++ (show newName)
      renameFile e newName

withHash :: FilePath -> B.ByteString -> FilePath
withHash fp h = dropExtension fp ++ "." ++ (C.unpack $ encode h) ++ takeExtension fp
      
hasHash :: FilePath -> Bool
hasHash fp = takeBaseName fp =~ "[[:xdigit:]]{32}"

needsHashing :: FilePath -> Bool
needsHashing fp = (not $ hasHash fp) && not ("." `isPrefixOf` (takeFileName fp))

hash :: FilePath -> IO B.ByteString
hash = liftM hashlazy . LB.readFile