{-# LANGUAGE ViewPatterns #-}

module Main where

import Assembler
import System.Environment
import System.IO
import Data.Maybe
import Data.List

data Config = Config {
    isHelp :: Bool,
    infile :: Maybe FilePath,
    outfile :: Maybe FilePath
} deriving (Show)

defConfig :: Config
defConfig = Config {
    isHelp = False,
    infile = Nothing,
    outfile = Nothing
}

help :: String -> String
help prog = "\
    \Usage: " ++ prog ++ " [options] <file>\n\
    \Options:\n\
    \  --help                 Display this help message and exit\n\
    \  -o <output>            Place the output into <file>\n\
    \ \n"

opt :: [String] -> Config -> Config
opt [] c = c
opt ("--help":ps) c = opt ps $ c { isHelp = True }
opt ("-o":n:ps) c = opt ps $ c { outfile = Just n }
opt (n:ps) c = opt ps $ c { infile = Just n }

use :: Maybe FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
use Nothing ReadMode = ($ stdin)
use Nothing WriteMode = ($ stdout)
use (Just file) mode = withFile file mode

main = do
    args <- getArgs
    let config = opt args defConfig

    if isHelp config then do
        prog <- getProgName
        putStr $ help prog
    else do
        assembleFiles config


assembleFiles config = do
    use (infile config) ReadMode $ \hin -> do
    use (outfile config) WriteMode $ \hout -> do
        input <- hGetContents hin

        case assembleToMif (BFCode input) of
            Left e   -> error $ getError e
            Right code -> mapM_ (hPutStrLn hout) code
