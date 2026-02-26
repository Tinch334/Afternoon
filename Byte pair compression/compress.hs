import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.List as L

import System.IO.Error (tryIOError, ioeGetErrorType, ioeGetFileName)


-- The Maybe in the second element is to account for a file with an odd number of bytes.
data Pair = Pair Word8 (Maybe Word8)
    deriving (Show, Eq, Ord)

-- Show an IO error prettily.
showIOError :: IOError -> String
showIOError e =
    let reason = "\nReason: " ++ show (ioeGetErrorType e)
    in case ioeGetFileName e of
        Nothing -> "An IO error occurred" ++ reason
        Just f -> "The file \"" ++ f ++ "\" could not be accessed" ++ reason

-- Turn the given file into a list of Pairs.
getPairs :: ByteString -> [Pair]
getPairs bs = case BS.uncons bs of
    Just (b, bs') -> maybe [Pair b Nothing] (\(b', bs'') -> Pair b (Just b') : getPairs bs'') (BS.uncons bs')
    Nothing -> []

-- Get the Pair with the most occurrences from a list of Pairs.
getBestPair :: [Pair] -> (Pair, Int)
getBestPair ps = getInner MS.empty (tail ps) (head ps, 1)
  where
    getInner :: MS.Map Pair Int -> [Pair] -> (Pair, Int) -> (Pair, Int)
    getInner _ [] bp = bp
    getInner pMap (p:pairs) bp@(_, bestCount) =
        case MS.lookup p pMap of
            Just c ->
                let
                    newCount = c + 1
                    newMap = MS.insert p newCount pMap
                -- Check if new pair is best pair.
                in if newCount > bestCount
                    then getInner newMap pairs (p, newCount)
                    else getInner newMap pairs bp
            -- No point in checking for best pair, since the best pair will have at least one occurrence.
            Nothing -> let newMap = MS.insert p 1 pMap in getInner newMap pairs bp

main :: IO ()
main = do
    -- Temporary testing solution.
    let file = "compress.hs"
    let outCompressedFile = "compressed.bp"
    let outDecompressedFile = "decompressed.bp"

    -- Compress.
    res <- tryIOError $ BS.readFile file
    case res of
        Left err -> putStrLn $ showIOError err
        Right contents -> do
            let (compressedCnt, replacementLst) = compress contents
            let byteReplacementLst = L.foldl' convertLst BS.empty replacementLst
            -- There's no risk of data loss since there can never be more than 255 replacements, since a free byte code is needed for the
            -- replacement, and there are only 255 available values for all bytes.
            let lengthWord = (fromIntegral $ length replacementLst) :: Word8
            let finalBS = BS.append (BS.singleton lengthWord) $ BS.append byteReplacementLst compressedCnt

            BS.writeFile outCompressedFile finalBS

    -- Decompress.
    resDc <- tryIOError $ BS.readFile outCompressedFile
    case resDc of
        Left err -> putStrLn $ showIOError err
        Right contents -> case BS.uncons contents of
            Nothing -> error "Empty file"
            Just (replacementCountWord, bytes) -> do
                -- Get all bytes that map replacements.
                let replacementCount = fromIntegral replacementCountWord :: Int
                let tableSize = replacementCount * 3
                let (replacementBytes, compressedBytes) = BS.splitAt tableSize bytes

                let decompressedBytes = decompress compressedBytes (makePairsFromBytes replacementBytes MS.empty)
                BS.writeFile outDecompressedFile decompressedBytes

  where
    convertLst bs (Pair c1 (Just c2), rc) = BS.append (BS.pack [c1, c2, rc]) bs
    convertLst _ (Pair _ Nothing, _) = error "Invalid compressed list"

    -- Takes bytes from compressed files, converts them into an easily searchable map.
    makePairsFromBytes :: ByteString -> MS.Map Word8 Pair -> MS.Map Word8 Pair
    makePairsFromBytes rb pairMap = if BS.null rb
        then pairMap
        else
            let
                Just (b1, r) = BS.uncons rb
                Just (b2, r') = BS.uncons r
                Just (br, r'') = BS.uncons r'
            in MS.insert br (Pair b1 $ Just b2) (makePairsFromBytes r'' pairMap)

    -- Printing function for debugging.
    mapMBS bs = case BS.uncons bs of
        Nothing -> return ()
        Just (byte, bs') -> do
            print byte
            mapMBS bs'

-- Compresses the given ByteString, returns the result and a map of the given replacements.
compress :: ByteString -> (ByteString, [(Pair, Word8)])
compress contents = compressInner contents []
  where
    -- Converts the given Pair to the given Word8 and returns the result.
    replacePair :: ByteString -> Pair -> Word8 -> ByteString
    replacePair bs pair byte = case BS.uncons bs of
        Just (b, bs') -> case BS.uncons bs' of
                -- Check if byte pair is equal to the given pair, if so replace it, otherwise reinsert it into ByteString.
                Just (b', bs'') -> if Pair b (Just b') == pair
                    then BS.append (BS.singleton byte) (replacePair bs'' pair byte)
                    else BS.append (BS.pack [b, b']) (replacePair bs'' pair byte)
                Nothing -> BS.singleton b
        Nothing -> BS.empty

    compressInner cnt rl = 
        let
            pairs = getPairs cnt
            (bestPair, bestCount) = getBestPair pairs
        in if bestCount == 1
            then (cnt, rl) -- String cannot be compressed further, return compressed result and compression map.
            else
                let
                    usedBytes = BS.foldl' (flip S.insert) S.empty cnt
                    freeBytes = S.difference (S.fromAscList [0..255]) usedBytes
                in case S.lookupMin freeBytes of
                    Nothing -> (cnt, rl) -- No free bytes, string cannot be compressed further.
                    Just minByte ->
                        let
                            newMap = (bestPair, minByte):rl
                            newCnt = replacePair cnt bestPair minByte
                        in compressInner newCnt newMap

decompress :: ByteString -> MS.Map Word8 Pair -> ByteString
decompress bs rm = BS.concatMap expand bs
  where
    expand :: Word8 -> ByteString
    expand byte = case MS.lookup byte rm of
        Nothing -> BS.singleton byte
        Just (Pair b1 (Just b2)) -> expand b1 <> expand b2
        _ -> error "Invalid pair"