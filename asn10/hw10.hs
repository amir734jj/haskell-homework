import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

canceller1 :: Int -> TVar Int -> IO()   -- canceller1 n cTVar is the piece of code that is executed in the canceller part of the main
canceller1 n cTVar = atomically $ do
  count <- readTVar cTVar           -- we get the current counter
  writeTVar cTVar (count + 1)       -- then we increase the counter

finish :: [IO () -> IO b] -> Int -> IO [b]
finish actions n = do
  cTVar <- newTVarIO 0       -- counter TVar with initial value of 0
  a <- mapM (\x -> async $ x (canceller1 n cTVar)) actions   -- we run all the actions passing canceller1 n cTVar
  let g x = if x < n
         then do      -- if we havent reached n then...
             c <- atomically $ readTVar cTVar   -- we get the value of the counter
             g c           -- and then run g again but with the current value of the counter
         else mapM_ cancel a    -- if we reached n then we cancel
  g 0           -- runs g with the initial value 0 of the counter
  mapM wait a   -- we wait for all the results (some of them cancelled)

main = do
  let urls = [("http://www.yahoo.com/", "test1.txt"),
              ("http://www.google.com/", "test2.txt"),
              ("http://www.msn.com/", "test3.txt"),
              ("http://www.bing.com/", "test4.txt")]

  let actions = map request urls
  ms <- finish actions 2
  mapM_ putStrLn ms
 where
    request :: (String, String) -> IO () -> IO String
    request (url, f) canceller = do
         d <- simpleHttp url
         canceller
         L.writeFile f d
         return $ url ++ " done"
         `catch` handler
         where handler = \(SomeException e) -> return (url ++ ": " ++ displayException e)

