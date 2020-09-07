module Main (main) where

import Control.Monad.Reader
---import Data.Time.Clock
---import Data.Time.Format
---import Data.Time.LocalTime
import Data.List
import qualified Data.Map.Strict as M
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Tim
import qualified Data.ByteString.Char8 as BC

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
       in case cwd' == "" of
            False -> findInParent target cwd'
            True -> return Nothing

mkEnv0 :: IO Env
mkEnv0 = do
  cwd <- getCurrentDirectory
  cwd' <- makeAbsolute cwd
  Just ePath <- findInParent timDir cwd'
  return $ Env {..}

tshow :: Integer -> String
tshow x = f xh "h" <> f xm "m" <> f xs "s"
 where (xh, x') = quotRem x (60*60)
       (xm, xs) = quotRem x' 60
       f amt suffix = if amt == 0 then "" else (show amt) <> suffix <> " "

tActive :: App ()
tActive = error "XXX active"

tSum :: String -> App ()
tSum tr = do
  Env {..} <- ask
  liftIO $ do
    let trb = BC.pack tr
    let trl = BC.length trb
    let sel x = trb == BC.take trl x
    tm <- getTopics ePath sel
    let f _ (Entry _ d t) (s, m) = (s', M.adjust (+ d') t m)
            where d' :: Integer = fromIntegral d
                  s' = if M.member t m then (s + d') else s
    (s, m) <- foldEntries ePath f $ (0, M.map (const (0 :: Integer)) tm)
    let g t ts = putStrLn $ (tm M.! t) <> "\n> " <> tshow ts
    mapM_ (uncurry g) $ M.toList m
    putStrLn $ "Total\n> " <> tshow s

tNew :: String -> App ()
tNew _t = error "XXX new"

tStart :: String -> App ()
tStart _t = error "XXX start"

tStop :: App ()
tStop = error "XXX stop"

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