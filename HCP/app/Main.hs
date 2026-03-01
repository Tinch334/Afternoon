module Main (main) where

import qualified Data.ByteString as BS

import           System.IO.Error (tryIOError, ioeGetErrorType, ioeGetFileName)
import           System.Exit (exitFailure)
import           System.FilePath
import           Options.Applicative hiding (action)

import           Algorithms

---------------------------------
-- ARGUMENT PARSER FUNCTIONS
---------------------------------
data Action = Compress String | Decompress String
    deriving (Show, Eq)

data Options = Options
    { action    :: Action
    , inFile    :: FilePath
    , outFile   :: Maybe FilePath
    } deriving (Show, Eq)


actionParser :: Parser Action
actionParser = compress <|> decompress
  where
    compress :: Parser Action
    compress = Compress <$> strOption ( long "compress"
        <> short 'c'
        <> metavar "ALGORITHM"
        <> help "Compression algorithm to be used" )
    decompress :: Parser Action
    decompress = Decompress <$> strOption ( long "decompress"
        <> short 'd'
        <> metavar "ALGORITHM"
        <> help "Decompression algorithm to be used" )

optionParser :: Parser Options
optionParser = Options <$> actionParser <*> inFileParser <*> outFileParser
  where
    inFileParser :: Parser FilePath
    inFileParser = argument str ( metavar "FILENAME"
        <> help "File to be compressed/decompressed" )
    outFileParser :: Parser (Maybe FilePath)
    outFileParser = optional $ strOption ( metavar "FILENAME"
        <> long "output"
        <> short 'o'
        <> help "File where the result of the action will be stored" )


---------------------------------
-- AUXILARY FUNCTIONS
---------------------------------
-- Show an IO error prettily.
showIOError :: IOError -> String
showIOError e =
    let reason = "\nReason: " ++ show (ioeGetErrorType e)
    in case ioeGetFileName e of
        Nothing -> "An IO error occurred" ++ reason
        Just f -> "The file \"" ++ f ++ "\" could not be accessed" ++ reason



main :: IO ()
main = do
    opts <- execParser getOpts
    -- Read input file.
    maybeFile <- tryIOError $ BS.readFile (inFile opts)

    case maybeFile of
        Left err -> do
            putStrLn $ showIOError err
            exitFailure
        -- If file could be read act according to action.
        Right contents -> case action opts of
            Compress maybeAlg -> case getAlgorithm availableAlgorithms maybeAlg of
                Nothing -> putStrLn $ "Unknown algorithm " ++ "\"" ++ maybeAlg ++ "\""
                Just algorithm -> do
                    let processedFile = (compress algorithm) contents
                    let outFile = getCompressedName opts (extention algorithm)
                    BS.writeFile outFile processedFile

            Decompress maybeAlg -> case getAlgorithm availableAlgorithms maybeAlg of
                Nothing -> do
                    putStrLn $ "Unknown algorithm " ++ "\"" ++ maybeAlg ++ "\""
                    exitFailure
                Just algorithm ->
                    if takeExtension (inFile opts) == "." ++ (extention algorithm)
                    then do
                        let processedFile = (decompress algorithm) contents
                        let outFile = getDecompressedName opts
                        BS.writeFile outFile processedFile
                    else do
                        putStrLn $ "Invalid file extension, expected " ++ "\"" ++ (extention algorithm) ++ "\""
                        exitFailure
    
  where
    getOpts = info (optionParser <**> helper) 
         ( fullDesc
        <> progDesc "A file compressor/decompressor" )

    getAlgorithm [] _ = Nothing
    getAlgorithm (a:as) n = if (shortName a == n) then Just a else getAlgorithm as n

    getCompressedName opts ext = case (outFile opts) of
        Nothing -> (inFile opts) <.> ext
        Just out -> out <.> ext

    getDecompressedName opts = case (outFile opts) of
        Nothing -> dropExtension $ inFile opts
        Just out -> out