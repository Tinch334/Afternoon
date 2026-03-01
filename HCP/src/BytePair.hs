module BytePair (compress, decompress) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.List as L

-- The Maybe in the second element is to account for a file with an odd number of bytes.
data Pair = Pair Word8 (Maybe Word8)
    deriving (Show, Eq, Ord)

compress :: ByteString -> ByteString
compress contents = 
    let (compressedCnt, replacementLst) = compressHandler contents
        byteReplacementLst = L.foldl' convertLst BS.empty replacementLst
        -- There's no risk of data loss since there can never be more than 255 replacements, since a free byte code is needed for the
        -- replacement, and there are only 255 available values for all bytes.
        lengthWord = (fromIntegral $ length replacementLst) :: Word8
    in BS.append (BS.singleton lengthWord) $ BS.append byteReplacementLst compressedCnt

  where
    convertLst bs (Pair c1 (Just c2), rc) = BS.append (BS.pack [c1, c2, rc]) bs
    convertLst _ (Pair _ Nothing, _) = error "Invalid compressed list"

-- Compresses the given ByteString, returns the result and a map of the given replacements.
compressHandler :: ByteString -> (ByteString, [(Pair, Word8)])
compressHandler contents = compressInner contents [] (BS.foldl' (flip S.insert) S.empty contents)
  where
    -- Converts the given Pair to the given Word8 and returns the result.
    replacePair :: ByteString -> Pair -> Word8 -> ByteString
    replacePair bs pair byte = case BS.uncons bs of
        Just (b, bs') -> case BS.uncons bs' of
                -- Check if byte pair is equal to the given pair, if so replace it, otherwise reinsert it into ByteString.
                Just (b', bs'') -> if Pair b (Just b') == pair
                    then BS.append (BS.singleton byte) (replacePair bs'' pair byte)
                    else BS.append (BS.pack [b, b']) (replacePair bs'' pair byte)
                Nothing -> BS.singleton b -- File had an odd number of bytes.
        Nothing -> BS.empty

    -- Compression algorithm.
    compressInner cnt rl ub = 
        let
            pairs = getPairs cnt
            (bestPair, bestCount) = getBestPair pairs
        in if bestCount == 1
            then (cnt, rl) -- String cannot be compressed further, return compressed result and compression map.
            else
                let
                    freeBytes = S.difference (S.fromAscList [0..255]) ub
                in case S.lookupMin freeBytes of
                    Nothing -> (cnt, rl) -- No free bytes, string cannot be compressed further.
                    Just minByte ->
                        let
                            newMap = (bestPair, minByte):rl
                            newCnt = replacePair cnt bestPair minByte
                            -- Get all bytes used in this iteration.
                            iterationBytes = BS.foldl' (flip S.insert) S.empty newCnt
                        -- Continue compressing, add all new used bytes to used bytes.
                        in compressInner newCnt newMap (S.union ub iterationBytes)

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

    -- Turn the given file into a list of Pairs.
    getPairs :: ByteString -> [Pair]
    getPairs bs = case BS.uncons bs of
        Just (b, bs') -> maybe [Pair b Nothing] (\(b', bs'') -> Pair b (Just b') : getPairs bs'') (BS.uncons bs')
        Nothing -> []

decompress :: ByteString -> ByteString
decompress contents = case BS.uncons contents of
    Nothing -> error "Empty file"
    Just (replacementCountWord, bytes) ->
        -- Get all bytes that map replacements.
        let replacementCount = fromIntegral replacementCountWord :: Int
            tableSize = replacementCount * 3
            (replacementBytes, compressedBytes) = BS.splitAt tableSize bytes
        in decompressHandler compressedBytes (makePairsFromBytes replacementBytes MS.empty)
  where
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


decompressHandler :: ByteString -> MS.Map Word8 Pair -> ByteString
decompressHandler bs rm = BS.concatMap expand bs
  where
    expand :: Word8 -> ByteString
    expand byte = case MS.lookup byte rm of
        Nothing -> BS.singleton byte
        Just (Pair b1 (Just b2)) -> expand b1 <> expand b2
        _ -> error "Invalid pair"