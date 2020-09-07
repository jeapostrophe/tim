module Main (main) where

import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.OrgMode
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Word

--- Parsing
type Parser = Parsec () String

type LocalClockEntry = (LocalTime, LocalTime)

pTime :: Int -> String -> Parser LocalTime
pTime len fmt = do
  void $ char '['
  s <- count len anySingle
  lt <- parseTimeM False defaultTimeLocale fmt s
  void $ char ']'
  return lt

pClockTime :: Parser LocalTime
pClockTime = try (pTime 20 "%F %a %R") <|> pTime 23 "%F %a %T"

pClockEntry :: Parser LocalClockEntry
pClockEntry = do
  void $ many (char ' ')
  void $ string "CLOCK: "
  from <- pClockTime
  void $ string "--"
  to <- pClockTime
  return (from, to)

parseClockEntry :: String -> Maybe LocalClockEntry
parseClockEntry s =
  either (const Nothing) Just $ runParser pClockEntry "" s

--- Converting
type App = ReaderT Env IO

type Path = [String]

data ClockEntry = ClockEntry Path UTCTime UTCTime
 deriving Eq

instance Ord ClockEntry where
    (ClockEntry _ x _) <= (ClockEntry _ y _) = x <= y

data Env = Env
  { ePath :: Path,
    eTimeZone :: TimeZone,
    ePathsR :: IORef (S.Set Path),
    eEntriesR :: IORef [ClockEntry],
    eEpochR :: IORef UTCTime
  }

mkEnv0 :: IO Env
mkEnv0 = do
  let ePath = []
  zt <- getZonedTime
  let eTimeZone = zonedTimeZone zt
  ePathsR <- newIORef mempty
  eEntriesR <- newIORef []
  eEpochR <- newIORef $ zonedTimeToUTC zt
  return $ Env {..}

add :: LocalClockEntry -> App ()
add (lfrom, lto) = do
  Env {..} <- ask
  let f = localTimeToUTC eTimeZone
  let from = f lfrom
  let to = f lto
  liftIO $ modifyIORef ePathsR (S.insert ePath)
  liftIO $ modifyIORef eEpochR (min from)
  liftIO $ modifyIORef eEntriesR ((ClockEntry ePath from to) :)
  return ()

lgo :: TextLine -> App ()
lgo (TextLine _ l ln) =
  case parseClockEntry l of
    Just lce -> add lce
    Nothing -> liftIO $ putStrLn $ "Skipped: " <> ln' <> l
  where
    ln' = maybe "" ((++ ": ") . show) ln

cgo :: NodeChild -> App ()
cgo = \case
  ChildNode n -> ngo n
  ChildDrawer (Drawer _ _ ls) ->
    mapM_ lgo ls
  ChildText tl ->
    lgo tl
  _ -> return ()

ngo :: Node -> App ()
ngo n = do
  env <- ask
  let path' = ePath env ++ [nTopic n]
  local (\e -> e {ePath = path'}) $ do
    mapM_ cgo $ nChildren n

fmtDuration :: Integral a => UTCTime -> UTCTime -> a
fmtDuration from to =
  round $ diffUTCTime to from

writeEntry :: UTCTime -> M.Map Path Word16 -> ClockEntry -> IO ()
writeEntry epoch p2idx (ClockEntry p from to) = do
  putStrLn $ show (p2idx M.! p) <> " " <> (show (fmtDuration epoch from :: Word32)) <> " " <> (show (fmtDuration from to :: Word16))

writePath :: Path -> Word16 -> IO ()
writePath p idx = do
  putStrLn $ show idx <> " -> " <> intercalate ":" p

main :: IO ()
main = do
  [f] <- getArgs
  c <- readFile f
  let d = orgFile c
  env0 <- mkEnv0
  flip runReaderT env0 $ mapM_ ngo $ odNodes d
  let Env {..} = env0
  ePaths <- readIORef ePathsR
  let pathsToIntL :: [(Path, Word16)] = zip (S.toAscList ePaths) [0 ..]
  mapM_ (uncurry writePath) pathsToIntL
  let pathsToIntM = M.fromList pathsToIntL
  eEntries <- readIORef eEntriesR
  eEpoch <- readIORef eEpochR
  putStrLn $ formatTime defaultTimeLocale rfc822DateFormat eEpoch
  mapM_ (writeEntry eEpoch pathsToIntM) $ sort eEntries
