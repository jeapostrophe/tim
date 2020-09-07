module Tim
  ( Start,
    Duration,
    Topic,
    Log (..),
    Entry (..),
    writeTopics,
    writeLog,
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List
import Data.Word()
import System.FilePath
import System.Directory

type Epoch = Word64

type Start = Word32

type Duration = Word16

type Topic = Word16

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
  where l' = take sz l
        l'' = l' ++ replicate (sz - (length l')) def

writeTopics :: FilePath -> Topics -> IO ()
writeTopics p ts = do
  createDirectoryIfMissing True p
  writeFile (topicsp p) $ intercalate "\n" ts

writeLog :: FilePath -> Log -> IO ()
writeLog p l = do
  createDirectoryIfMissing True p
  encodeFile (logp p) l
