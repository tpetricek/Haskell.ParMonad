-- ======================================================================================
--   Explicit speculative parallelism for Haskell 
--   Tomas Petricek (tomas@tomasp.net)
-- ======================================================================================

import Data.Int
import Control.DeepSeq
import System.Environment
import Control.Monad.Par
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Conc (pseq, par)

import Data.Unamb

-----------------------------------------------------------------------------------------
-- Utilities & basic numeric functions
-----------------------------------------------------------------------------------------

type NumType = Int64

measure s f = do
  putStrLn ("Starting:" ++ s)
  start <- getCurrentTime
  putStrLn $ "Result: " ++ (show f)
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"

fib :: NumType -> NumType
fib x | x < 1 = 1
fib x = fib (x-2) + fib (x-1)

ffib :: NumType -> NumType
ffib x = f 1 1 x
  where f a b 0 = b
        f a b n = f b (a+b) (n-1)

isPrime :: NumType -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = divisible 2
  where
    divisible m
      | n `rem` m == 0 = False
      | m * m > n = True
      | otherwise = divisible (m + 1)

-----------------------------------------------------------------------------------------
-- Example #1: Calculating Fibbonacci
-----------------------------------------------------------------------------------------

rfib :: NumType -> NumType
rfib n | n < 25 = fib n 
rfib n = x `par` y `pseq` (x+y)
  where 
    x = rfib (n-1)
    y = rfib (n-2)  

pfib :: Int64 -> Par Int64
pfib n | n < 25 = return (fib n)
pfib n = do 
  av <- spawn (pfib (n - 1))
  b  <- pfib (n - 2)  
  a  <- get av
  return (a + b)

main1 = do 
  let size = 32
  measure "Seq" $ fib size
  measure "MonadPar" $ runPar $ pfib size
  measure "Sparks" $ rfib size

  putStrLn ""
  measure "Seq" $ fib size
  measure "MonadPar" $ runPar $ pfib size
  measure "Sparks" $ rfib size

-----------------------------------------------------------------------------------------
-- Example #2: Unambiguous choice
-----------------------------------------------------------------------------------------

tryBoth :: (t -> a) -> (t -> a) -> t -> a
tryBoth f1 f2 n =
  unamb (f1 n) (f2 n)

main2 = do
  measure "Fib 32" (fib 32)
  measure "Ffib 32" (ffib 32)
  measure "Unamb" (tryBoth fib ffib 32)


-----------------------------------------------------------------------------------------
-- Example #3: ...
-----------------------------------------------------------------------------------------

pffib :: Int64 -> Par Int64
pffib n | n < 25 = return (ffib n)
pffib n = do 
  av <- spawn (pffib (n - 1))
  b  <- pffib (n - 2)  
  a  <- get av
  return (a + b)

punamb :: NFData a => Par a -> Par a -> Par a
punamb p1 p2 = do
  r <- newBlocking
  ct1 <- newCancelToken
  ct2 <- newCancelToken
  forkWith ct1 (do
    a <- p1
    cancel ct2 
    put r a)
  forkWith ct2 (do
    a <- p2
    cancel ct1
    put r a)
  v <- get r
  return v

main = do
  measure "Pfib 32" $ runPar $ pfib 32
  measure "Pffib 32" $ runPar $ pffib 32
  measure "Unamb" $ runPar $ punamb (pfib 32) (pffib 32)

-----------------------------------------------------------------------------------------
-- Example #4: Tree processing example
-----------------------------------------------------------------------------------------

data Tree a 
  = Leaf a
  | Node (Tree a) (Tree a)

makeTree (x:[]) = Leaf x
makeTree list =
  case splitAt ((length list) `div` 2) list of 
  (l1, l2) -> Node (makeTree l1) (makeTree l2)

treeLength (Leaf _) = 1
treeLength (Node a b) = treeLength a + treeLength b

instance Show a => Show (Tree a) where
  show (Leaf n) = show n
  show (Node a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"



forall :: (a -> Bool) -> Tree a -> Bool
forall p (Leaf v)           = p v
forall p (Node left right)  = forall p left && forall p right

forallp :: (a -> Bool) -> Tree a -> Par Bool
forallp p (Leaf num) = return $ p num
forallp p (Node left right) = do
  al' <- spawn $ forallp p left
  ar  <- forallp p right
  al  <- get al'
  return (al && ar)
 
foralls :: (a -> Bool) -> Tree a -> Par Bool
foralls p tree = do
    tok <- newCancelToken
    r <- forall' tok tree
    cancel tok 
    return r
  where 
    forall' tok (Leaf v) = return (p v)
    forall' tok (Node left right) = do
      leftRes <- new
      rightRes <- new
      finalRes <- newBlocking
      forkWith tok (forall' tok left >>= 
        completed leftRes rightRes finalRes)
      forkWith tok (forall' tok right >>= 
        completed rightRes leftRes finalRes)
      get finalRes
    
    completed varA varB fin resA = do
      put varA resA
      if not resA then put fin False
      else get varB >>= put fin . (&& resA)
        

main4 = do
  -- Generate tree with some random primes & force its evaluation
  let range = [ 5000000000 .. 5000005000 ] 
  let primes = [ n | n <- range, isPrime n ] 
  let tree1 = makeTree primes
  let tree2 = Node (makeTree primes) (Leaf 256)
  putStrLn $ "Length 1: " ++ (show $ treeLength tree1)
  putStrLn $ "Length 2: " ++ (show $ treeLength tree2)
  
  -- Run various evaluation strategies
  measure "All primes (Seq) " (forall isPrime tree1)
  measure "All primes (Par) " (runPar $ forallp isPrime tree1)
  measure "All primes (Shr) " (runPar $ foralls isPrime tree1)

  measure "Non-prime num (Seq) " (forall isPrime tree2)
  measure "Non-prime num (Par) " (runPar $ forallp isPrime tree2)
  measure "Non-prime num (Shr) " (runPar $ foralls isPrime tree2)
