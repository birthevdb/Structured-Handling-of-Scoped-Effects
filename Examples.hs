{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples where

import Prelude hiding (fail, or)
import Control.Monad (ap, guard)
import Control.Applicative

import Debug.Trace

-- * Scoped Programs

data Prog f g a
  = Return a
  | Call (f (Prog f g a))
  | Enter (g (Prog f g (Prog f g a)))


instance (Functor f , Functor g) => Functor (Prog f g) where
    fmap f (Return x) = Return (f x)
    fmap f (Call t)   = Call (fmap (fmap f ) t)
    fmap f (Enter t)  = Enter (fmap (fmap (fmap f )) t)

instance (Functor f , Functor g) => Applicative (Prog f g) where
    pure  = return
    (<*>) = ap

instance (Functor f, Functor g) => Monad (Prog f g) where
    return = Return
    Return x >>= f = f x
    Call op  >>= f = Call (fmap (>>= f) op)
    Enter sc >>= f = Enter (fmap (fmap (>>= f)) sc)

-----------------------------------------------------------------------------------------------------------
-- * Indexed algebras

data Nat = Zero | Succ Nat

data IndexAlg f g a = IndexAlg
    { action :: forall n . f (a n) -> a n
    , demote :: forall n . g (a (Succ n)) -> a n
    , promote :: forall n . a n -> a (Succ n) }

foldHybrid :: (Functor f, Functor g) => IndexAlg f g a -> Prog f g (a n) -> a n
foldHybrid alg (Return x) = x
foldHybrid alg (Call op)  = (action alg . fmap (foldHybrid alg)) op
foldHybrid alg (Enter sc) = (demote alg . fmap (foldHybrid alg . fmap (promote alg . foldHybrid alg))) sc

run :: (Functor f, Functor g) => (r -> a Zero) -> IndexAlg f g a -> Prog f g r -> a Zero
run gen alg prog = foldHybrid alg (fmap gen prog)

-----------------------------------------------------------------------------------------------------------
-- Endoalgebras

data EndoAlg f g c = EndoAlg
    { callE     :: forall x. f (c x) -> c x
    , enterE    :: forall x. g (c (c x)) -> c x
    , returnE   :: forall x. x -> c x
    }

data BaseAlg f g c d = BaseAlg
    { callB   :: f d -> d
    , enterB  :: g (c d) -> d
    }

type f ~> f' = forall a . f a -> f' a

-- horizontal composition
(~.) :: (Functor f) => (f ~> g) -> (a -> b) -> (f a -> g b)
g ~. f = g . fmap f

hcata :: (Functor f, Functor g) => (EndoAlg f g c) -> Prog f g a -> c a
hcata alg (Return x)   = returnE alg x
hcata alg (Call op)    = (callE alg . fmap (hcata alg)) op
hcata alg (Enter sc)   = (enterE alg . fmap (hcata alg . fmap (hcata alg))) sc

foldEndo :: (Functor f, Functor g) => (EndoAlg f g c) -> (BaseAlg f g c b) -> (a -> b) -> Prog f g a -> b
foldEndo alg ealg gen (Return x)   = gen x
foldEndo alg ealg gen (Call op)    = (callB ealg . fmap (foldEndo alg ealg gen)) op
foldEndo alg ealg gen (Enter sc)   = (enterB ealg . fmap (hcata alg ~. foldEndo alg ealg gen)) sc

-----------------------------------------------------------------------------------------------------------
-- * Example Nondeterminism

data Choice a = Fail | Or a a deriving Functor
data Once a   = Once a deriving Functor

-- smart constructors

fail :: Prog Choice sc a
fail = Call Fail

or :: Prog Choice sc a -> Prog Choice sc a -> Prog Choice sc a
or x y = Call (Or x y)

once :: Prog Choice Once a -> Prog Choice Once a
once x = Enter (Once (fmap return x))

-- once(or(1,5)) >>= λx. or(x,x+1)
prog1 :: Prog Choice Once Int
prog1 = do
    x <- once (Examples.or (return 1) (return 5))
    Examples.or (return x) (return (x + 1))

data CarrierND a n = ND [CarrierND1 a n]
data CarrierND1 a :: Nat -> * where
    CZND :: a -> CarrierND1 a Zero
    CSND :: [CarrierND1 a n] -> CarrierND1 a (Succ n)

-- indexed algebras

genHybrid1 :: a -> CarrierND a Zero
genHybrid1 x = ND [CZND x]

algHybrid1 :: IndexAlg Choice Once (CarrierND a)
algHybrid1 = IndexAlg {..} where
    action :: forall n a. Choice (CarrierND a n) -> CarrierND a n
    action Fail = ND []
    action (Or (ND l) (ND r)) = ND (l ++ r)

    demote :: forall n a. Once (CarrierND a (Succ n)) -> CarrierND a n
    demote (Once (ND [])) = ND []
    demote (Once (ND (CSND l:_))) = ND l

    promote :: forall n a. CarrierND a n -> CarrierND a (Succ n)
    promote (ND l) = ND [CSND l]

-- [1, 2]
exampleHybrid1 :: [Int]
exampleHybrid1 = toList (run genHybrid1 algHybrid1 prog1) where
    toList :: CarrierND a Zero -> [a]
    toList (ND l) = map (\(CZND x) -> x) l

-- endo algebras

genEndo1 :: a -> [a]
genEndo1 x = [x]

algEndo1 :: EndoAlg Choice Once []
algEndo1 = EndoAlg {..} where
    callE :: Choice [a] -> [a]
    callE Fail = []
    callE (Or x y) = x ++ y

    enterE :: Once [[a]] -> [a]
    enterE (Once x) = head x

    returnE :: a -> [a]
    returnE x = [x]

ealgEndo1 :: BaseAlg Choice Once [] [a]
ealgEndo1 = BaseAlg {..} where
    callB :: Choice [a] -> [a]
    callB Fail = []
    callB (Or x y) = x ++ y

    enterB :: Once [[a]] -> [a]
    enterB (Once x) = head x

-- [1, 2]
exampleEndo1 :: [Int]
exampleEndo1 = foldEndo algEndo1 ealgEndo1 genEndo1 prog1

-----------------------------------------------------------------------------------------------------------
-- * Example State with Local Variables

type Name = String
data State s a = Get Name (s -> a) | Put Name s a deriving Functor
data Local s a = Local Name s a deriving Functor

-- smart constructors
get :: Name -> Prog (State s) (Local s) s
get x = Call (Get x return)

put :: Name -> s -> Prog (State s) (Local s) ()
put x s = Call (Put x s (return ()))

local :: Name -> s -> Prog (State s) (Local s) a -> Prog (State s) (Local s) a
local x s p = Enter (fmap (fmap return) (Local x s p))

type Memory s = Name -> Maybe s

retrieve :: Name -> Memory s -> s
retrieve x m = case m x of
    Just s  -> s
    Nothing -> error ("var undefined")

update :: Name -> s -> Memory s -> Memory s
update x s m y | x == y    = Just s
               | otherwise = m y

incr :: Name -> Int -> Prog (State Int) (Local Int) ()
incr x i = do
    v <- get x
    put x (v + i)

prog2 :: Prog (State Int) (Local Int) (Int, Int)
prog2 = do
    put "x" 1
    put "y" 1
    local "x" 100 (do incr "x" 100
                      v <- get "x"
                      incr "y" v)
    incr "x" 2
    incr "y" 2
    vx <- get "x"
    vy <- get "y"
    return (vx, vy)

-- indexed algebras

data CarrierLS s a n = LS {runLS :: (Memory s -> (CarrierLS1 s a n, Memory s))}
data CarrierLS1 s a :: Nat -> * where
    CZLS :: a -> CarrierLS1 s a Zero
    CSLS :: (Memory s -> (CarrierLS1 s a n, Memory s)) -> CarrierLS1 s a (Succ n)

genHybrid2 :: a -> CarrierLS s a Zero
genHybrid2 x = LS (\m -> (CZLS x, m))

algHybrid2 :: IndexAlg (State s) (Local s) (CarrierLS s a)
algHybrid2 = IndexAlg {..} where
    action :: forall s n a. (State s) (CarrierLS s a n) -> CarrierLS s a n
    action (Put x s (LS f)) = LS (f . update x s)
    action (Get x p)        = LS (\m -> runLS (p (retrieve x m)) m)

    demote :: forall s n a. Local s (CarrierLS s a (Succ n)) -> CarrierLS s a n
    demote (Local x s (LS f)) = LS (\m -> case f (update x s m) of
        (CSLS g, n) -> g (update x (retrieve x m) n))

    promote :: forall s n a. CarrierLS s a n -> CarrierLS s a (Succ n)
    promote l = LS (\m -> (CSLS (runLS l), m))

-- (3, 203)
exampleHybrid2 :: (Int, Int)
exampleHybrid2 = case fst (runLS (run genHybrid2 algHybrid2 prog2) (const Nothing)) of
    CZLS a -> a

-- endo algebras

data C s a = C {unC :: Memory s -> (a, Memory s)}

genEndo2 :: (Int, Int) -> C Int (Int, Int)
genEndo2 x = C (\m -> (x, m))

algEndo2 :: EndoAlg (State s) (Local s) (C s)
algEndo2 = EndoAlg {..} where
    callE :: (State s) (C s a) -> C s a
    callE (Put x s (C f)) = C (f . update x s)
    callE (Get x p)       = C (\m -> unC (p (retrieve x m)) m)

    enterE :: (Local s) (C s (C s a)) -> C s a
    enterE (Local x s (C f)) = C (\m -> case f (update x s m) of
        (C g, n) -> g (update x (retrieve x m) n))

    returnE :: a -> C s a
    returnE x = C (\m -> (x, m))

ealgEndo2 :: BaseAlg (State Int) (Local Int) (C Int) (C Int (Int, Int))
ealgEndo2 = BaseAlg {..} where
    callB :: (State Int) (C Int (Int, Int)) -> C Int (Int, Int)
    callB (Put x s (C f)) = C (f . update x s)
    callB (Get x p)       = C (\m -> unC (p (retrieve x m)) m)

    enterB :: (Local Int) (C Int (C Int (Int, Int))) -> C Int (Int, Int)
    enterB (Local x s (C f)) = C (\m -> case f (update x s m) of
        (C g, n) -> g (update x (retrieve x m) n))

-- (3, 203)
exampleEndo2 :: (Int, Int)
exampleEndo2 = fst ((unC (foldEndo algEndo2 ealgEndo2 genEndo2 prog2)) (const Nothing))

-----------------------------------------------------------------------------------------------------------
-- * Example Concurrency

data Act m a = Act (m a) | Kill deriving Functor
data Con a   = Spawn a a | Atomic a deriving Functor

-- smart constructors

act :: Functor m => m a -> Prog (Act m) Con a
act m = Call (Act (fmap return m))

kill :: Prog (Act m) Con a
kill = Call Kill

spawn :: Functor m => Prog (Act m) Con a -> Prog (Act m) Con b -> Prog (Act m) Con a
spawn p q = Enter (fmap (fmap return) (Spawn p (q >> kill)))

atomic :: Functor m => Prog (Act m) Con a -> Prog (Act m) Con a
atomic p = Enter (fmap (fmap return) (Atomic p))

data Resumption m a = More (m (Resumption m a)) | Done a deriving Functor

retraction :: Monad m => Resumption m a -> m a
retraction (More m) = m >>= retraction
retraction (Done x) = return x

interleaveL :: Monad m => Resumption m a -> Resumption m b -> Resumption m a
interleaveL (Done x) r = fmap (const x) r
interleaveL r (Done _)  = r
interleaveL (More m1) (More m2) = More (do {r1 <- m1; r2 <- m2; return (interleaveL r1 r2)})

say :: String -> Prog (Act IO) Con ()
say = act . putStr

prog3 :: Prog (Act IO) Con ()
prog3 = do
    spawn (say "hello " >> say "world ")
          (say "goodbye " >> say "cruel " >> say "world ")

-- prog4 :: Prog (Act IO) Con ()
prog4 = do
    spawn (atomic (spawn (mapM say ["a", "b", "c"])
                         (mapM say ["A", "B", "C"])))
          (atomic (spawn (mapM say ["1", "2", "3"])
                         (mapM say ["-", "-", "-"])))

-- indexed algebras

data CarrierCon m a n = CC {runCC :: Resumption m (CarrierCon1 m a n)}
data CarrierCon1 m a :: Nat -> * where
    CZCC :: a -> CarrierCon1 m a Zero
    CSCC :: Resumption m (CarrierCon1 m a n) -> CarrierCon1 m a (Succ n)

rJoin :: Functor m => Resumption m (CarrierCon1 m a (Succ n))
                   -> Resumption m (CarrierCon1 m a n)
rJoin (Done (CSCC r)) = r
rJoin (More m)        = More (fmap rJoin m)

genHybrid3 :: Monad m => a -> CarrierCon m a Zero
genHybrid3 a = CC (Done (CZCC a))

algHybrid3 :: Monad m => IndexAlg (Act m) Con (CarrierCon m a)
algHybrid3 = IndexAlg {..} where
    action :: forall m n a. Monad m => (Act m) (CarrierCon m a n)
                                    -> CarrierCon m a n
    action (Act m) = CC (More (fmap runCC m))
    action (Kill)  = CC (Done (error "main process killed"))

    demote :: forall m n a. Monad m => Con (CarrierCon m a (Succ n))
                                    -> CarrierCon m a n
    demote (Atomic (CC r))          = CC (More (fmap (\(CSCC s) -> s) (retraction r)))
    demote (Spawn  (CC r1) (CC r2)) = CC (rJoin (interleaveL r1 r2))

    promote :: forall m n a. CarrierCon m a n -> CarrierCon m a (Succ n)
    promote (CC r) = CC (Done (CSCC r))

-- hello goodbye world cruel world
exampleHybrid3 :: IO ()
exampleHybrid3 = retraction (fmap (\(CZCC x) -> x) (runCC (run genHybrid3 algHybrid3 prog3)))

-- aAbBcC1-2-3-
exampleHybrid4 :: IO ()
exampleHybrid4 = retraction (fmap (\(CZCC x) -> head x) (runCC (run genHybrid3 algHybrid3 prog4)))

-- endo algebras

genEndo3 :: () -> Resumption IO ()
genEndo3 = Done

genEndo4 :: [()] -> Resumption IO ()
genEndo4 = Done . head

iJoin :: Functor m => Resumption m (Resumption m a) -> Resumption m a
iJoin (Done x) = x
iJoin (More m) = More (fmap iJoin m)

algEndo3 :: Monad m => EndoAlg (Act m) Con (Resumption m)
algEndo3 = EndoAlg {..} where
    callE :: Monad m => (Act m) (Resumption m a) -> Resumption m a
    callE (Act m) = More m
    callE Kill    = Done (error "main process killed")

    enterE :: Monad m => Con (Resumption m (Resumption m a)) -> Resumption m a
    enterE (Atomic r)    = More (retraction r)
    enterE (Spawn r1 r2) = iJoin (interleaveL r1 r2)

    returnE :: a -> Resumption m a
    returnE = Done

ealgEndo3 :: BaseAlg (Act IO) Con (Resumption IO) (Resumption IO ())
ealgEndo3 = BaseAlg {..} where
    callB :: (Act IO) (Resumption IO ()) -> (Resumption IO ())
    callB (Act m) = More m
    callB Kill    = Done (error "main process killed")

    enterB :: Con (Resumption IO (Resumption IO ())) -> (Resumption IO ())
    enterB (Atomic r)    = More (retraction r)
    enterB (Spawn r1 r2) = iJoin (interleaveL r1 r2)

-- hello goodbye world cruel world
exampleEndo3 :: IO ()
exampleEndo3 = retraction (foldEndo algEndo3 ealgEndo3 genEndo3 prog3)

-- aAbBcC1-2-3-
exampleEndo4 :: IO ()
exampleEndo4 = retraction (foldEndo algEndo3 ealgEndo3 genEndo4 prog4)


-----------------------------------------------------------------------------------------------------------
-- * Example of Depth Bound Search

instance Functor sc => Alternative (Prog Choice sc) where
  empty = fail
  (<|>) = or

-- queens n = [c_1, c_2, ... , c_n] where
--   (i, c_i) is the (row, column) of a queen
queens :: Functor sc => Int -> Prog Choice sc [Int]
queens n = go [1 .. n] []
  where
    -- `go cs qs` searches the rows `cs` for queens that do
    -- not threaten the queens in `qs`
    go :: Functor sc => [Int] -> [Int] -> Prog Choice sc [Int]
    go [] qs =  return qs
    go cs qs =  do (c, cs') <- select cs
                   guard (noThreat qs c 1)
                   go cs' (c:qs)

    -- `noThreat qs r c` returns `True` if there is no threat
    -- from a queen in `qs` to the square given by `(r, c)`.
    noThreat :: [Int] -> Int -> Int -> Bool
    noThreat []      c r  = True
    noThreat (q:qs)  c r  = abs (q - c) /= r && noThreat qs c (r+1)

-- |select xs| will return all |(y, ys)| pairs where |y| has
-- been selected from |xs|, and elements in |ys| have not.
select :: Functor sc => [a] -> Prog Choice sc (a, [a])
select []      =  empty
select (x:xs)  =  return (x, xs)  <|>  do  (y, ys) <- select xs
                                           return (y, x:ys)

-- |list p| lists all the solutions in |p|.
list :: Prog Choice Once a -> [a]
list = foldEndo algEndo1 ealgEndo1 genEndo1 where
  algEndo1 :: EndoAlg Choice Once []
  algEndo1 = EndoAlg {..} where
      callE :: Choice [a] -> [a]
      callE Fail = []
      callE (Or xs ys) = xs ++ ys

      enterE :: Once [[a]] -> [a]
      enterE (Once xs) = head xs

      returnE :: a -> [a]
      returnE x = [x]

  ealgEndo1 :: BaseAlg Choice Once [] [a]
  ealgEndo1 = BaseAlg {..} where
      callB :: Choice [a] -> [a]
      callB Fail = []
      callB (Or xs ys) = xs ++ ys

      enterB :: Once [[a]] -> [a]
      enterB (Once xs) = head xs

exampleFailOnce = once (return 0 `or` return 1) >>= (\x -> if x == 0 then fail else return x)

-- |exampleQueens| returns all the 92 solutions to the 8 queens problem.
exampleQueens :: [[Int]]
exampleQueens = list (queens 8)

data Depth a = Depth Int a deriving Functor

depth :: Int -> Prog Choice Depth a -> Prog Choice Depth a
depth d p = Enter (Depth d (fmap return p))

deeper :: Functor sc => Int -> Prog Choice sc Int
deeper n = return n <|> deeper (n-1)

newtype DepthCarrier a = DepthCarrier { unDepthC :: Int -> [a] } deriving Functor

dbsList :: Prog Choice Depth a -> [a]
dbsList = foldEndo endoAlg baseAlg gen where
  endoAlg :: EndoAlg Choice Depth DepthCarrier
  endoAlg = EndoAlg {..} where
    callE :: Choice (DepthCarrier a) -> DepthCarrier a
    callE Fail = DepthCarrier (const [])
    callE (Or (DepthCarrier fxs) (DepthCarrier fys))
      = DepthCarrier (\d -> if d == 0 then [] else fxs (d-1) ++ fys (d-1))

    enterE :: Depth (DepthCarrier (DepthCarrier a)) -> DepthCarrier a
    enterE (Depth d (DepthCarrier fxs)) =
        DepthCarrier (\d' -> concat [ fys d' | DepthCarrier fys <- fxs d])

    returnE :: a -> DepthCarrier a
    returnE x = DepthCarrier (const [x])

  baseAlg :: BaseAlg Choice Depth DepthCarrier [a]
  baseAlg = BaseAlg {..} where
    callB :: Choice [a] -> [a]
    callB Fail        = []
    callB (Or xs ys)  = xs ++ ys

    enterB :: Depth (DepthCarrier [a]) -> [a]
    enterB (Depth d (DepthCarrier fxs)) = concat (fxs d)

  gen :: a -> [a]
  gen x = [x]



-----------------------------------------------------------------------------------------------------------
-- * Example Search Strategy

data Strategy a = DFS a | BFS a | DBS Int a deriving Functor

dfs :: Prog Choice Strategy a -> Prog Choice Strategy a
dfs p = Enter (DFS (fmap return p))

dbs :: Int -> Prog Choice Strategy a -> Prog Choice Strategy a
dbs n p = Enter (DBS n (fmap return p))

bfs :: Prog Choice Strategy a -> Prog Choice Strategy a
bfs p = Enter (BFS (fmap return p))

class SearchAlg m where
  --toList :: m a -> [a]
  fromList :: [a] -> m a
  saEmpty :: m a
  saOr :: m a -> m a -> m a
  saJoin :: m (m a) -> m a
  saPure :: a -> m a

instance SearchAlg [] where
  --toList = id
  fromList = id
  saEmpty = []
  saOr = (++)
  saJoin = concat
  saPure x = [x]

instance SearchAlg DepthCarrier where
  fromList xs = DepthCarrier (\n -> xs)

  saEmpty = DepthCarrier (\n -> [])

  saOr (DepthCarrier xs) (DepthCarrier ys) =
     DepthCarrier (\n -> if n == 0 then [] else xs (n - 1) ++ ys (n - 1))

  saJoin (DepthCarrier fxs) = DepthCarrier (\n -> do xs <- fxs n; unDepthC xs n)

  saPure x = DepthCarrier (\n -> [x])


newtype Levels a = Levels { unLevels :: [[a]] } deriving Functor

zipL :: Levels a -> Levels a -> Levels a
zipL (Levels xxs) (Levels yys) = Levels (go xxs yys) where
  go [] yys = yys
  go xxs [] = xxs
  go (x:xs) (y:ys) = (x ++ y) : go xs ys

wrapL :: Levels a -> Levels a
wrapL (Levels xs) = Levels ([] : xs)

instance SearchAlg Levels where
  fromList xs = Levels [xs]

  saEmpty = Levels []

  saOr xxs yys = zipL (wrapL xxs) (wrapL yys) where

  saJoin (Levels [])     = saEmpty
  saJoin (Levels (x:xs)) = foldr zipL saEmpty x `zipL` wrapL (saJoin (Levels xs))

  saPure x = Levels [[x]]

data SCarrier a = SCarrier [a] (DepthCarrier a) (Levels a) deriving Functor

dfsSC :: SCarrier a -> [a]
dfsSC (SCarrier x _ _) = x

dbsSC :: SCarrier a -> DepthCarrier a
dbsSC (SCarrier _ y _) = y

bfsSC :: SCarrier a -> Levels a
bfsSC (SCarrier _ _ z) = z

list' :: Prog Choice Strategy a -> [a]
list' = foldEndo ealg balg (\x -> [x]) where
  ealg :: EndoAlg Choice Strategy SCarrier
  ealg = EndoAlg {..} where
    callE :: Choice (SCarrier a) -> SCarrier a
    callE Fail = SCarrier saEmpty saEmpty saEmpty
    callE (Or (SCarrier x1 x2 x3) (SCarrier y1 y2 y3))
      = SCarrier (saOr x1 y1) (saOr x2 y2) (saOr x3 y3)

    enterE :: Strategy (SCarrier (SCarrier a)) -> SCarrier a
    enterE (DFS (SCarrier xs _ _)) = SCarrier (saJoin (fromList (fmap dfsSC xs)))
                                              (saJoin (fromList (fmap dbsSC xs)))
                                              (saJoin (fromList (fmap bfsSC xs)))

    enterE (DBS n (SCarrier _ ys _)) = SCarrier (saJoin (fromList (fmap dfsSC ys')))
                                                (saJoin (fromList (fmap dbsSC ys')))
                                                (saJoin (fromList (fmap bfsSC ys')))
      where ys' = unDepthC ys n

    enterE (BFS (SCarrier _ _ zs)) = SCarrier (saJoin (fromList (fmap dfsSC zs')))
                                              (saJoin (fromList (fmap dbsSC zs')))
                                              (saJoin (fromList (fmap bfsSC zs')))   -- This intuitively means two nested BFS { BFS {...}; ...} have different searching queues
      where zs' = concat (unLevels zs)


    returnE :: a -> SCarrier a
    returnE x = SCarrier (saPure x) (saPure x) (saPure x)

  balg ::  BaseAlg Choice Strategy SCarrier [a]
  balg = BaseAlg {..} where
      callB :: Choice [a] -> [a]
      callB Fail = []
      callB (Or xs ys) = xs ++ ys

      enterB ::  Strategy (SCarrier [a]) -> [a]
      enterB (DFS (SCarrier xs _ _)) = concat xs
      enterB (DBS n (SCarrier _ ys _)) = concat (unDepthC ys n)
      enterB (BFS (SCarrier _ _ zs)) = concat (concat (unLevels zs))

prog5 :: Prog Choice Strategy Int
prog5 = bfs (or (or (return 1)
                    (or (return 3)
                        (return 4)))
                (or (return 2)
                    (or (return 5)
                        (return 6))))

test5 = list' prog5 == [1,2,3,4,5,6]

prog6 :: Prog Choice Strategy Int
prog6 = dbs 2 (or (or (return 1)
                      (or (return 3)
                          (return 4)))
                  (or (return 2)
                      (or (return 5)
                          (return 6))))
test6 = list' prog6 == [1,2]

prog7 :: Prog Choice Strategy Int
prog7 = do i <- prog5
           or (return i)
              (or (return (-i))
                  (return i))
test7 = list' prog7 == [1,-1,1,2,-2,2,3,-3,3,4,-4,4,5,-5,5,6,-6,6]


prog8 :: Prog Choice Strategy Int
prog8 = bfs (or (return 1)
                (or (dfs (or (or (or (or (return 3)
                                         fail)
                                  fail)
                             fail)
                         fail))
                    (return 2)))
test8 = list' prog8 == [1,3,2]
