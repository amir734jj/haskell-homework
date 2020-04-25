import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

canceller :: Int -> TVar Int -> IO()
canceller n valueTVar = atomically $ do
  count <- readTVar valueTVar
  writeTVar valueTVar (count + 1)

finish :: [IO () -> IO b] -> Int -> IO [b]
finish actions n = do
  v1 <- newTVarIO 0       -- counter mvar
  a <- mapM (\x -> async $ x (canceller n v1)) actions   -- we run all the actions
  let g x = if x <= n
         then do
             value <- atomically $ readTVar v1
             g value
         else mapM_ cancel a
  g 0             
  l <- mapM wait a   -- we wait for all the results (some of them cancelled)
  return $ l         -- we return the list of results

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

