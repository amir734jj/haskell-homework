{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, listDirectory, Permissions(..),
                         getModificationTime, getPermissions, getFileSize)
import System.FilePath ((</>), takeExtension)
import Control.Exception (handle, SomeException(..))

import Data.Time (UTCTime(..))
import Data.Time.Calendar(toGregorian)
import Data.Time.Clock(utctDay,UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

epoch = epochToUTC 0

match :: String -> String -> Bool
match s p
  | length s < l = False
  | otherwise = (take l s) == p || match (drop 1 s) p
  where l = length p

class Applicative m => Predicate m where
   (>?) :: (Ord a) => m a -> a -> m Bool
   p >? a = (> a) <$> p
   infixl 4 >?

   (<?) :: (Ord a) => m a -> a -> m Bool
   p <? a = (< a) <$> p
   infixl 4 <?

   (==?) :: (Ord a) => m a -> a -> m Bool
   p ==? a = (== a) <$> p
   infixl 4 ==?

   (!=?) :: (Ord a) => m a -> a -> m Bool
   p !=? a = (/= a) <$> p
   infixl 4 !=?

   (~?) :: m String -> String -> m Bool
   p ~? s = (`match` s) <$> p
   infixl 4 ~?

   (&&?) :: m Bool -> m Bool -> m Bool
   p1 &&? p2 = (&&) <$> p1 <*> p2
   infixl 3 &&?

   (||?) :: m Bool -> m Bool -> m Bool
   p1 ||? p2 = (||) <$> p1 <*> p2
   infixl 3 ||?

   notP :: m Bool -> m Bool
   notP = fmap (not)

data FileInfo = FileInfo {
                    filePath :: FilePath,
                    filePermissions :: Maybe Permissions,
                    fileSize :: Maybe Integer,
                    fileLastModified :: Maybe UTCTime
                }

newtype FilePredicate a = FileP { runFileP :: FileInfo -> a } deriving (Functor,Applicative)

instance Predicate FilePredicate

pathP :: FilePredicate String
pathP = FileP filePath

extP :: FilePredicate String
extP = takeExtension <$> pathP

sizeP :: FilePredicate Integer
sizeP = maybe (-1) id <$> FileP fileSize

getPermissionP :: (Permissions -> Bool) -> FilePredicate Bool
getPermissionP f = maybe False f <$> FileP filePermissions

searchP:: FilePredicate Bool
searchP = getPermissionP searchable

readP :: FilePredicate Bool
readP = getPermissionP readable

writeP :: FilePredicate Bool
writeP = getPermissionP writable

execP :: FilePredicate Bool
execP = getPermissionP executable

timeP :: FilePredicate UTCTime
timeP = maybe epoch id <$> FileP fileLastModified

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _) -> return Nothing) (fmap Just act)

getInfo :: FilePath -> IO FileInfo
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (getFileSize path)
  modified <- maybeIO (getModificationTime path)
  return (FileInfo path perms size modified)


data FileAction = Done | Skip | Continue

type FileState s =  StateT s IO FileAction

find :: forall s. (FileInfo -> FileState s) -> FilePath -> FileState s

find getState path = do
   names <- lift $ listDirectory path
   iterate names

   where iterate :: [FilePath] -> FileState s
         -- we need to provide a FileState s for the empty list,
         -- for that case we give the FileState s that returns Done
         iterate [] = do
              return Done
         -- the inductive case
         iterate (x:xs) = do
              let fullPath = path </> x
              fileInfo <- lift $ getInfo fullPath

              -- we need a state s and run getState fileInfo on s to know the FileAction
              -- that is, we need to know if we are Done, or we have to Skip or Continue
              s <- get
              (action, newState) <- lift $ runStateT (getState fileInfo) $ s

              -- next we do things depending on the action
              case action of
                  Done -> do
                    -- nothing to do, we are done. So we return Done
                    return Done

                  Skip -> do
                    -- we disregard x and continue the iteration with xs
                    iterate xs

                  Continue -> do
                    -- first we check if it is a directory
                    isDirectory <- lift $ doesDirectoryExist fullPath
                    if isDirectory then do
                        -- if it is a directory then we call find recursively
                        find getState fullPath
                      else do
                        -- if it is a file then we run getState fileInfo
                        getState fileInfo
                    -- after we handled Continue, we continue the iteration
                    iterate xs

main = do
   -- Path, make sure it's correct
   let downloads = "C:\\Users\\tzhao\\Downloads"
   
   let yearP = ((\(x,_,_) -> x) . toGregorian . utctDay) <$> timeP
   let recurseP = yearP >? 2018

   let filterP = (extP ==? ".pdf" &&? sizeP >? 2^22) ||? execP

   let iter :: FileInfo -> FileState [(FilePath, Integer, UTCTime)]
       iter = \info -> do
            let eval = \p -> runFileP p info

            s <- get
            let r = (eval pathP, eval sizeP, eval timeP)
            let s' = if eval filterP then r : s else s
            put s'

            let a = if length s' >= 10 then Done
                    else if eval recurseP then Continue else Skip
            return a

   results <- execStateT (find iter downloads) []

   forM_ results $ \(p,n,t) ->
                            handle (\(SomeException _) -> return ())
                          $ putStrLn
                          $ (take 100 p) ++ "\t"
                              ++ show (round $ fromInteger n / 2^10) ++ " KB\t"
                              ++ show (utctDay t)
