import Control.Applicative
import Control.Monad
import Control.Monad.Instances ()
import Control.Exception       (bracket)

import Data.Char
import Data.List           ((\\), intersect, isPrefixOf)

import System.Environment  (getArgs)
import System.Process
import System.Directory
import System.Exit


----------------------------------------------------------------
-- List helpers
----------------------------------------------------------------

-- | '||' in applicative context
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixl 4 <||>

-- | Find common prefix 
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix []     = []
commonPrefix [x]    = x
commonPrefix (x:xs) = foldl go x xs
    where
      go p y = map fst $ takeWhile (uncurry (==)) $ zip p y

-- | Find commom postfix
commonPostfix :: Eq a => [[a]] -> [a]
commonPostfix = reverse . commonPrefix . map reverse

-- | Remove common prefix
stripPrefix :: Eq a => [[a]] -> [[a]]
stripPrefix xs = map (drop $ length $ commonPrefix xs) xs

-- | Remove common postfix
stripPostfix :: Eq a => [[a]] -> [[a]]
stripPostfix xs = map (reverse . drop (length $ commonPostfix xs) . reverse) xs


----------------------------------------------------------------
-- Repository manipulations
----------------------------------------------------------------


-- | Get list of remote repos
remoteRepos :: String           -- ^ Host name
            -> String           -- ^ Path to repo root
            -> IO [String]
remoteRepos host path =     
  stripPostfix . lines <$> 
  readProcess "ssh" [host,"cd "++path++"; find -name .hg -type d"] ""

-- | Get list of local repos located in the current directory
localRepos :: IO [String]
localRepos = 
  stripPostfix . lines <$> 
  readProcess "find" ["-name",".hg","-type","d"] ""

inCd :: FilePath -> IO a -> IO a
inCd dir action = do
  bracket getCurrentDirectory setCurrentDirectory $ 
    const $ 
      setCurrentDirectory dir >> action


-- | Clone remote repo
clone :: String                 -- ^ Host name
      -> String                 -- ^ Repositoties root
      -> String                 -- ^ Relative path to repo
      -> IO ()
clone host path dir = do
  putStrLn $ " * Clone: " ++ dir
  createDirectoryIfMissing True dir
  mapM_ putStrLn . filter (isPrefixOf "added ") . lines =<<
    readProcess "hg" ["clone", "ssh://"++host++"/"++path++"/"++dir, dir] ""

-- | Update repository
update :: String                 -- ^ Host name
       -> String                 -- ^ Repositoties root
       -> String                 -- ^ Relative path to repo
       -> IO ()
update host path dir = do
  putStrLn $ " * Update: " ++ dir
  mapM_ putStrLn . filter (isPrefixOf "added ") . lines =<< 
    inCd dir (readProcess "hg" ["pull", "ssh://"++host++"/"++path++"/"++dir] "")


-- | Mirror repositories
mirror :: String                 -- ^ Host name
       -> String                 -- ^ Repositoties root
       -> IO ()
mirror host path = do
  unless (all ((`elem` "@.") <||> isLetter <||> isDigit) host) $ do
    putStrLn $ "Bad host name: " ++ host
    exitFailure
  unless (all ((=='.')       <||> isLetter <||> isDigit) path) $ do
    putStrLn $ "Bad path: " ++ path
    exitFailure
  remote <- remoteRepos host path
  local  <- localRepos
  -- Remove orphan repositories
  putStrLn "==== Remove orphans ===="
  forM_ (local \\ remote) $ \n -> do
    putStrLn $ " * Removing repository: " ++ n
    rawSystem "rm" ["-rfv", n]
  -- Cleanup empty directories"
  _ <- system "find -depth -type d -empty -exec rmdir -v {} \\;"
  -- Clone missing repositories
  putStrLn "==== Clone missing ===="
  forM_ (remote \\ local) $ clone host path
  -- Update rest
  putStrLn "==== Update ===="
  forM_ (remote `intersect` local) $ update host path
    
    


main :: IO ()
main = do
  args <- getArgs
  case args of
    [host,dir]      -> mirror host dir
    [host,dir,dest] -> setCurrentDirectory dest >> mirror host dir
    _               -> do
      putStrLn "Invalid parameters"
      exitFailure
