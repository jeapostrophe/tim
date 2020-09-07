module Main (main) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Tim

type App = ReaderT Env IO

data Env = Env
  {ePath :: FilePath}

findInParent :: FilePath -> FilePath -> IO (Maybe FilePath)
findInParent target cwd = do
  let target' = cwd </> target
  de <- doesDirectoryExist target'
  case de of
    True -> Just <$> makeAbsolute target'
    False ->
      let cwd' = takeDirectory cwd
       in case cwd' == "/" of
            False -> findInParent target cwd'
            True -> return Nothing

mkEnv0 :: IO Env
mkEnv0 = do
  cwd <- getCurrentDirectory
  cwd' <- makeAbsolute cwd
  mp <- findInParent timDir cwd'
  case mp of
   Nothing -> sfail $ "No " <> timDir <> " in parents"
   Just ePath -> return $ Env {..}

tshow :: Integer -> String
tshow x = f xh "h" <> f xm "m" <> (show xs) <> "s"
  where
    (xh, x') = quotRem x (60 * 60)
    (xm, xs) = quotRem x' 60
    f amt suffix = if amt == 0 then "" else (show amt) <> suffix <> " "

getTopics' :: FilePath -> String -> IO (M.Map Topic String)
getTopics' ep tr = do
  let trb = BC.pack tr
  let trl = BC.length trb
  let sel x = trb == BC.take trl x
  getTopics ep sel

tSum :: String -> App ()
tSum tr = do
  Env {..} <- ask
  liftIO $ do
    tm <- getTopics' ePath tr
    let f _ (Entry _ d t) (s, m) = (s', M.adjust (+ d') t m)
          where
            d' :: Integer = fromIntegral d
            s' = if M.member t m then (s + d') else s
    (s, m) <- foldEntries ePath f $ (0, M.map (const (0 :: Integer)) tm)
    let g t ts = putStrLn $ (tm M.! t) <> "\n> " <> tshow ts
    mapM_ (uncurry g) $ M.toList m
    putStrLn $ "Total\n> " <> tshow s

sfail :: String -> IO a
sfail s = do
  putStrLn s
  void $ exitWith $ ExitFailure 1
  error "Impossible"

tNew :: String -> App ()
tNew t = do
  Env {..} <- ask
  liftIO $ do
    tm <- getTopics ePath (BC.pack t ==)
    case M.null tm of
      True -> addTopic ePath t
      False -> sfail $ "Topic exists: '" <> t <> "'"

tStart :: String -> App ()
tStart tr = do
  Env {..} <- ask
  liftIO $ do
    tm <- getTopics' ePath tr
    case M.toList tm of
      [(t, _)] -> do
        epoch <- getEpoch ePath
        now <- getCurrentTime
        case safeDuration epoch now of
          Nothing -> sfail $ "Could not safely turn to duration"
          Just start -> do
            ok <- startEntry ePath start t
            case ok of
              True -> return ()
              False -> sfail $ "Timer running"
      _ -> sfail $ "No unique topic matching: '" <> tr <> "'"

tActiveStop :: Bool -> App ()
tActiveStop isStop = do
  Env {..} <- ask
  liftIO $ do
    epoch <- getEpoch ePath
    ae <- activeEntry ePath
    case ae of
      Nothing -> exitWith $ ExitFailure 1
      Just (Entry start _ t) -> do
        let start' = addUTCTime (fromIntegral start) epoch
        now <- getCurrentTime
        let dur :: Integer = round $ diffUTCTime now start'
        topics <- getTopic ePath t
        tz <- getCurrentTimeZone
        let start'' = utcToLocalTime tz start'
        let s = formatTime defaultTimeLocale "%c" start''
        putStrLn $ topics <> " for " <> tshow dur <> " since " <> s
        when isStop $
          case safeDuration start' now of
              Just dur' -> stopActiveEntry ePath dur'
              Nothing -> sfail "Duration too long" --- XXX split and log

tActive :: App ()
tActive = tActiveStop False

tStop :: App ()
tStop = tActiveStop True

usage :: ExitCode -> IO ()
usage ec = do
  putStrLn $ "tim"
  putStrLn $ "       - show active timer"
  putStrLn $ "   sum - sum timers of topic"
  putStrLn $ "   new - new topic"
  putStrLn $ " start - start timer"
  putStrLn $ "  stop - stop timer"
  putStrLn $ "  help - show this message"
  exitWith ec

main :: IO ()
main = do
  a <- getArgs
  env0 <- mkEnv0
  let c = intercalate " "
  flip runReaderT env0 $
    case a of
      [] -> tActive
      ("sum" : ts) -> tSum $ c ts
      ("new" : ts) -> tNew $ c ts
      ("start" : ts) -> tStart $ c ts
      ["stop"] -> tStop
      ["help"] -> liftIO $ usage ExitSuccess
      _ -> liftIO $ usage $ ExitFailure 1