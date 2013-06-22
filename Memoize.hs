-- Memoize.hs
-- memoization using State monad
--
import Control.Monad.State
import qualified Data.Map as Map

fib :: Integer -> Integer
fib n = memoizeM fibM n

fibM :: Monad m => (Integer -> m Integer) -> Integer -> m Integer
fibM f 0 = return 0
fibM f 1 = return 1
fibM f n = liftM2 (+) a b
  where
      a = f $ n - 1
      b = f $ n - 2

ackermann :: Integer -> Integer -> Integer
ackermann m n
    | n < 0 || m < 0  = error "negative arguments"
    | otherwise       = memoizeM ackerM (m,n)

ackerM :: Monad m => ((Integer, Integer) -> m Integer) ->
          (Integer, Integer) -> m Integer
ackerM a' (0,n) = return $ n + 1
ackerM a' (m,0) = a' (m-1, 1)
ackerM a' (m,n) = a' (m, n-1) >>= \n' -> a' (m-1, n')

type StateMap a b = State (Map.Map a b) b

memoizeM :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) 
                     -> (a -> b)
memoizeM t x = evalState (f x) Map.empty 
  where
      g x = do
          y <- t f x  
          m <- get
          put $ Map.insert x y m;
          return y
      f x = get >>= \m -> maybe (g x) return (Map.lookup x m)

