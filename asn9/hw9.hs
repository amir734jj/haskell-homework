import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

canceller :: Int -> MVar Int -> MVar Int -> IO()
canceller n valueMVar cancelMVar = do
  count <- modifyMVar valueMVar (\x -> return (x+1,x+1))  -- increase the counter and get its value
  if count >= n then putMVar cancelMVar 1 else return ()  -- take action according to the number of finished requests


finish :: [IO () -> IO b] -> Int -> IO [b]
finish actions n = do
  v1 <- newMVar 0       -- counter mvar
  v2 <- newEmptyMVar    -- cancel mvar
  a <- mapM (\x -> async $ x (canceller n v1 v2)) actions   -- we run all the actions
  forkIO $ do           -- fork that waits for the cancel signal
      takeMVar v2       -- we wait for the cancel signal
      mapM_ cancel a    -- we cancel all unfinished tasks after the cancel signal
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

