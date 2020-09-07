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
import Tim

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

data ClockEntry = ClockEntry LineNumber Path UTCTime UTCTime
  deriving (Eq)

instance Ord ClockEntry where
  (ClockEntry _ _ x _) <= (ClockEntry _ _ y _) = x <= y

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

add :: LineNumber -> LocalClockEntry -> App ()
add ln (lfrom, lto) = do
  Env {..} <- ask
  let f = localTimeToUTC eTimeZone
  let from = f lfrom
  let to = f lto
  liftIO $ modifyIORef ePathsR (S.insert ePath)
  liftIO $ modifyIORef eEpochR (min from)
  liftIO $ modifyIORef eEntriesR ((ClockEntry ln ePath from to) :)
  return ()

lgo :: TextLine -> App ()
lgo (TextLine _ l ln) =
  case parseClockEntry l of
    Just lce -> add ln lce
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

makeEntry :: UTCTime -> M.Map Path Topic -> ClockEntry -> Entry
makeEntry epoch p2idx (ClockEntry ln p from to) = 
  mkEntry ln' epoch from to $ p2idx M.! p
  where ln' = maybe "" ((" at line " ++) . show) ln

main :: IO ()
main = do
  [f, dest] <- getArgs
  c <- readFile f
  let d = orgFile c
  env0 <- mkEnv0
  flip runReaderT env0 $ mapM_ ngo $ odNodes d
  let Env {..} = env0
  ePaths <- readIORef ePathsR
  let ePathsL = S.toAscList ePaths
  writeTopics dest $ map (intercalate ":") ePathsL
  let pathsToIntM :: M.Map Path Topic = M.fromList $ zip ePathsL [0 ..]
  eEntries <- readIORef eEntriesR
  eEpoch <- readIORef eEpochR
  writeLog dest $ Log (mkEpoch eEpoch) $ map (makeEntry eEpoch pathsToIntM) $ sort eEntries
