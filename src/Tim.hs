module Tim
  ( Start,
    Duration,
    Topic,
    Log (..),
    Entry (..),
    safeDuration,
    writeTopics,
    writeLog,
    mkEpoch,
    mkEntry,
    timDir,
    getTopic,
    getTopics,
    foldEntries,
    addTopic,
    getEpoch,
    startEntry,
    activeEntry,
    stopActiveEntry,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word ()
import System.Directory
import System.FilePath
import System.IO

timDir :: FilePath
timDir = ".tim"

type Epoch = Word64

type Start = Word32

type Duration = Word16

type Topic = Word16

topic_len :: Int
topic_len = 128

topicify :: String -> String
topicify s = (exactly ' ' $ topic_len - 1) s ++ "\n"

type Topics = [String]

data Log = Log Epoch [Entry]

instance Binary Log where
  put (Log e es) = putWord64le e <> putList' es
  get = Log <$> getWord64le <*> getList get

data Entry = Entry Start Duration Topic
  deriving (Show)

instance Binary Entry where
  put (Entry s d t) = putWord32le s <> putWord16le d <> putWord16le t
  get = Entry <$> getWord32le <*> getWord16le <*> getWord16le

putList' :: Binary a => [a] -> Put
putList' = \case
  [] -> mempty
  (x : xs) -> put x <> putList' xs

getList :: Get a -> Get [a]
getList getOne = do
  empty <- isEmpty
  if empty
    then return []
    else do
      x <- getOne
      xs <- getList getOne
      return (x : xs)

topicsp :: FilePath -> FilePath
topicsp p = p </> "topics"

logp :: FilePath -> FilePath
logp p = p </> "log"

exactly :: a -> Int -> [a] -> [a]
exactly def sz l = l''
  where
    l' = take sz l
    l'' = l' ++ replicate (sz - (length l')) def

prepPath :: FilePath -> IO ()
prepPath p = do
  createDirectoryIfMissing True $ takeDirectory p
  fe <- doesPathExist p
  when fe $
    error $ "tim: will not overwrite " <> p

writeTopics :: FilePath -> Topics -> IO ()
writeTopics p ts = do
  let p' = topicsp p
  prepPath p'
  writeFile p' $ concatMap topicify ts

writeLog :: FilePath -> Log -> IO ()
writeLog p l = do
  let p' = logp p
  prepPath p'
  encodeFile p' l

safeRound :: forall a b. (RealFrac a, Integral b, Bounded b) => a -> Maybe b
safeRound x =
  case x < fromIntegral (minBound :: b) || x > fromIntegral (maxBound :: b) of
    True -> Nothing
    False -> Just $ round x

mkEpoch :: UTCTime -> Maybe Epoch
mkEpoch t =
  safeRound (utcTimeToPOSIXSeconds t)

safeDuration :: (Integral a, Bounded a) => UTCTime -> UTCTime -> Maybe a
safeDuration from to =
  safeRound $ diffUTCTime to from

mkEntry :: UTCTime -> UTCTime -> UTCTime -> Topic -> Maybe Entry
mkEntry epoch start end topic = do
  start' <- safeDuration epoch start
  dur <- safeDuration start end
  return $ Entry start' dur topic

getEpoch :: FilePath -> IO UTCTime
getEpoch p =
  withFile (logp p) ReadMode $
    ( \lh -> do
        epochBs <- B.hGet lh 8
        let epoch = runGet getWord64le epochBs
        return $ posixSecondsToUTCTime $ fromIntegral epoch
    )

splitEvery :: Int -> BC.ByteString -> [BC.ByteString]
splitEvery len bs =
  case BC.null bs of
    True -> []
    False -> b : splitEvery len bs'
      where
        (b, bs') = BC.splitAt len bs

trimTopic :: BC.ByteString -> BC.ByteString
--- XXX I want to use dropWhileEnd isSpace
trimTopic = BC.reverse . (BC.dropWhile isSpace) . BC.reverse

getTopics :: FilePath -> (BC.ByteString -> Bool) -> IO (M.Map Topic String)
getTopics p sel = do
  let tp = topicsp p
  bs <- BC.readFile tp
  let tbs = map trimTopic $ splitEvery topic_len bs
  let f m (tb, idx) = if sel tb then M.insert idx (BC.unpack tb) m else m
  return $ foldl' f mempty $ zip tbs [0 ..]

addTopic :: FilePath -> String -> IO ()
addTopic p t = do
  let tp = topicsp p
  h <- openFile tp AppendMode
  hPutStr h $ topicify t
  hClose h

getTopic :: FilePath -> Topic -> IO String
getTopic p t = do
  withFile (topicsp p) ReadMode $
   ( \th -> do 
     hSeek th AbsoluteSeek (fromIntegral $ (fromIntegral t) * topic_len)
     tbs <- BC.hGet th topic_len
     return $ BC.unpack $ trimTopic tbs )

foldEntries :: FilePath -> (Epoch -> Entry -> a -> a) -> a -> IO a
foldEntries p f a = do
  let lp = logp p
  Log epoch es <- decodeFile lp
  return $ foldl' (flip (f epoch)) a es

startEntry :: FilePath -> Start -> Topic -> IO Bool
startEntry p now t = do
  ae <- activeEntry p
  case ae of
    Just _ -> return False
    Nothing ->
      withFile (logp p) AppendMode $
        ( \lh -> do
            B.hPut lh $ encode (Entry now 0 t)
            return True)

readActiveEntry :: Handle -> IO Entry
readActiveEntry lh = do
  sz <- hFileSize lh
  hSeek lh AbsoluteSeek (sz - 8)
  entryBs <- B.hGet lh 8
  return $ decode entryBs

activeEntry :: FilePath -> IO (Maybe Entry)
activeEntry p =
  withFile (logp p) ReadMode $
    ( \lh -> do
        entry@(Entry _ dur _) <- readActiveEntry lh
        case dur == 0 of
          False -> return $ Nothing
          True -> return $ Just entry)

stopActiveEntry :: FilePath -> Duration -> IO ()
stopActiveEntry p dur = do
  withFile (logp p) ReadWriteMode $
   (\lh -> do
        Entry start _ t <- readActiveEntry lh
        sz <- hFileSize lh
        hSeek lh AbsoluteSeek (sz - 8)
        B.hPut lh $ encode (Entry start dur t))