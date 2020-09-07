module Tim
  ( Start,
    Duration,
    Topic,
    Log (..),
    Entry (..),
    writeTopics,
    writeLog,
    mkEpoch,
    mkEntry,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
---import Data.Bits
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
---import Data.Time.Format
---import Data.Time.LocalTime
import Data.Word ()
import System.Directory
import System.FilePath

type Epoch = Word64

type Start = Word32

type Duration = Word16

type Topic = Word16

topic_len :: Int
topic_len = 128

type Topics = [String]

data Log = Log Epoch [Entry]

instance Binary Log where
  put (Log e es) = putWord64le e <> putList' es
  get = Log <$> getWord64le <*> getList get

data Entry = Entry Start Duration Topic

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
  fe <- doesFileExist p
  when fe $
    removeFile p

writeTopics :: FilePath -> Topics -> IO ()
writeTopics p ts = do
  let p' = topicsp p
  prepPath p'
  let ts' = intercalate "\n" $ map (exactly ' ' $ topic_len - 1) ts
  writeFile p' ts'

writeLog :: FilePath -> Log -> IO ()
writeLog p l = do
  let p' = logp p
  prepPath p'
  encodeFile p' l

safeRoundM :: forall a b. (RealFrac a, Integral b, Bounded b) => a -> Maybe b
safeRoundM x =
  case x < fromIntegral (minBound :: b) || x > fromIntegral (maxBound :: b) of
    True -> Nothing
    False -> Just $ round x

safeRound :: forall a b. (Show a, RealFrac a, Integral b, Bounded b) => String -> a -> b
safeRound lab x =
  case safeRoundM x of
    Nothing -> error $ "safeRound: cannot convert " <> show x <> " to " <> lab
    Just y -> y

mkEpoch :: UTCTime -> Epoch
mkEpoch t =
  safeRound "epoch" (utcTimeToPOSIXSeconds t)

safeDuration :: (Integral a, Bounded a) => String -> UTCTime -> UTCTime -> a
safeDuration lab from to =
  safeRound lab $ diffUTCTime to from

mkEntry :: String -> UTCTime -> UTCTime -> UTCTime -> Topic -> Entry
mkEntry lab epoch start end topic =
  Entry (safeDuration ("start" <> lab) epoch start) (safeDuration ("duration" <> lab) start end) topic