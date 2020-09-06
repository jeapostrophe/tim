module Main(main) where
    
import Data.OrgMode
import Data.List
import System.Environment
import Control.Monad.Reader

type App = ReaderT Env IO

data Env = Env {
    ePath :: [String]
}

env0 :: Env
env0 = Env {
    ePath = []
}

lgo :: TextLine -> App ()
lgo (TextLine _ l _) = do
    liftIO $ putStrLn l

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
    liftIO $ putStrLn $ intercalate ":" path'
    local (\e -> e { ePath = path' }) $ do
        mapM_ cgo $ nChildren n

main :: IO ()
main = do
    [ f ] <- getArgs
    c <- readFile f
    let d = orgFile c
    flip runReaderT env0 $ mapM_ ngo $ odNodes d
