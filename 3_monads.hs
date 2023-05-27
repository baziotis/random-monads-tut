{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

-- I don't want to overcomplicate things with Haksell's
-- import system, so, we'll just paste the stuff
-- we need from 1_functors.hs and 2_app_functors.hs here.

----- APPLICATIVE FUNCTORS PRELUDE -----

class Functor' t where
  pure' :: a -> t a
  fmap' :: (a -> b) -> t a -> t b

instance Functor' [] where
  pure' x = [x]

  fmap' = map

instance Functor' Maybe where
  pure' x = Just x

  fmap' f Nothing = Nothing
  fmap' f (Just x) = Just (f x)


class Functor' t => AppFunctor' t where
  (<**>) :: t (a -> b) -> t a -> t b

instance AppFunctor' Maybe where
  (<**>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<**>) Nothing _ = Nothing
  (<**>) (Just f) ma = fmap' f ma

instance AppFunctor' [] where
  (<**>) :: [a -> b] -> [a] -> [b]
  (<**>) [] _ = []
  fs <**> xs = [f x | f <- fs, x <- xs]

----- END APPLICATIVE FUNCTORS PRELUDE -----


{-
-- A monad defines the operation >>=, which is defined in Haskell,
-- so to avoid cloberring it, we use >>=>. IMO, the best way to
-- understand the usefulness of monads is to simply describe what
-- a monad does and then slowly observe the use cases emerge.

-- It is useful to compare and contrast >>=> with fmap' and <**>.
-- All three get a function and a `t a`. But a >>=> has the order
-- of the arguments reversed. All three return a `t b`. Another
-- subtle difference is that in fmap' and <**>, the function passed
-- does not "know" anything about the `t`. In fmap we pass a function
-- (a -> b) - no `t` is mentioned - and in <**> we pass a function
-- boxed inside a `t` but the function itself does not know about `t`.
-- In >>=> this changes. In particular, the function passed returns
-- a `t b`, so it must be aware of the `t`.

-- Finally, roughly what >>=> is help us when we have a `t a` and
-- a function (a -> t b), and we want to apply this function to
-- the `a` inside the `t`. The monad "unboxes" the `a` and applies
-- the function to it.
-}

class AppFunctor' t => Monad' t where
  (>>=>) :: t a -> (a -> t b) -> t b

instance Monad' Maybe where
  (>>=>) Nothing _ = Nothing
  (>>=>) (Just x) f = f x


--------------- EXAMPLE 0: Exceptions

{-
-- A classic example is implementing the concept of exceptions. That is,
-- you want to compose some functions which may fail, by returning Nothing.
-- If that happens at any point, the whole composition should return Nothing.
-- Otherwise, we just call the next function in the sequence.

-- To make this concrete, suppose we have these functions f1 and g1.
-- It should be obvious that we can't do normal function composition,
-- i.e., we cannot do this:

g1 (f1 2)

-- That's because f1 returns `Maybe Int` but g1 gets an `Int`. A monad
-- lets us do exactly what we want. See the definition of `>>=>` for
-- `Maybe` above. If the result is `Nothing`, it returns `Nothing` (and
-- that will get propagated). Otherwise, it calls the "next function in
-- the sequence", which is its second argument.
-}


f1 :: Int -> Maybe Int
f1 x = Just x

g1 :: Int -> Maybe Int
g1 x = Nothing

tmp = (f1 2) >>=> g1

{-
-- This example allows us to talk a bit more abstractly about why
-- monads are cool. If you observe, we just _customized_ function
-- application. Function application is a core topic in math and functional
-- programming and monads let us customize it, which is why they're so cool.

-- To drive that point home a bit more, suppose that we have a value `v`
-- and a function `f`. Normal function application is `f v`, i.e., calling
-- `f` with `v` as an arg. A pretty boring >>=> could do exactly that
-- in v >>=> f. But, we can make >>=> do _arbitrary_ stuff. For example,
-- >>=> may call `f` with `v` non-deterministically. Or, it may pass it
-- `2*v` as an argument. Or, it may call `f` with some value and then
-- do something with `v` and the result of that call.

-- As a final abstract note, this value `v` is usually the result
-- of another function call, which means that monads effectively
-- lets us customize function composition.
-}


--------------- EXAMPLE 1: More Exceptions

{-
-- This is a longer example in the same spirit. We have a Person who
-- may have no parents, only a mother, only a father, or both. And we
-- have several functions acting on them. We can see that these functions
-- start getting hairy because we have to basically repeat over and over
-- the logic "call a function; if it fails, return Nothing, otherwise,
-- call the next function". For example, see `mothersPaternalGrandfather`.

-- Sidenote: The `case` stuff in `father` and `mother` is different from
-- the `case` stuff in `maternalGrandfather` 
-- and `mothersPaternalGrandfather`. The former is to handle the logic
-- that has to do with `Person`. The latter is to handle logic that
-- has to do with `Maybe.`

-- Monads allow us again to customize function application to implement
-- this machinery automatically.
-}

data Person = NoParents | HasMother Person | HasFather Person | HasBoth Person Person

father :: Person -> Maybe Person
father p =  case p of
              HasFather f -> Just f
              HasBoth _ f -> Just f
              _ -> Nothing

mother :: Person -> Maybe Person
mother p =  case p of
              HasMother m -> Just m
              HasBoth m _ -> Just m
              _ -> Nothing

-- i.e., the father of the mother
maternalGrandfather :: Person -> Maybe Person
maternalGrandfather s = case mother s of
                          Nothing -> Nothing
                          Just m  -> father m

-- i.e., the father of the father of the mother
mothersPaternalGrandfather :: Person -> Maybe Person
mothersPaternalGrandfather s = case mother s of
                                 Nothing -> Nothing
                                 Just m  -> case father m of
                                              Nothing -> Nothing
                                              Just gf -> father gf

mothersPaternalGrandfather' :: Person -> Maybe Person
mothersPaternalGrandfather' p = (pure p) >>=> mother >>=> father >>=> father

found :: Maybe Person -> Bool
found Nothing = False
found (Just x) = True

p1 :: Person
p1 = HasBoth NoParents NoParents
p2 = HasBoth (HasFather (NoParents)) NoParents
p3 = HasBoth (HasFather (HasBoth NoParents NoParents)) NoParents

res_p1 = found (mothersPaternalGrandfather' p1)
res_p2 = found (mothersPaternalGrandfather' p2)
res_p3 = found (mothersPaternalGrandfather' p3)



--------------- EXAMPLE 2: More Exceptions

{-
-- This is again an example on exceptions with monads, with
-- a simple evaluator of expressions which contain division.
-- If we try to divide by 0, it should return Nothing. I include
-- this example because it highlights two important points of
-- monads that I mention below.
-}

data DivExpr = Val Int | Div DivExpr DivExpr 
              deriving (Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv n m =  if m == 0 then Nothing else Just (n `div` m)

eval' ::  DivExpr -> Maybe Int
eval' (Val n)   =  Just n
eval' (Div x y) =
  case eval' x of
    Nothing -> Nothing
    Just n1 ->
      case eval' y of
        Nothing -> Nothing
        Just n2 -> n1 `safeDiv` n2

{-
-- The first important thing is that we could not implement
-- `eval` using applicative functors, even though the setup
-- looks similar to what we had before. Previously, we wanted
-- to apply a binary operation to 2 Maybe's and we did:

pure' (+) <**> Just 1 <**> Just 2

-- Here, we have the same situation. `eval x` is a Maybe as well
-- as `eval y` and we want to apply the binary operator `safeDiv`.
-- The problem is that <**> expects a Maybe (a -> b) but `safeDiv`
-- is a -> Maybe b, and so it does not fit the structure. So, the
-- the following fails:

eval_fail :: Expr -> Int
eval_fail e = pure' safeDiv <**> eval x <**> eval y

-- More abstractly, <**> does the steps (a) unpack
-- (the second argument), (b) pass it to the first argument (i.e., the function
-- t (a -> b)), (c) pack the result into a `t`. But `safeDiv` packs the result
-- itself.

-- The other important point is that this example showcases a common
-- pattern when using monads. Essentially below, we say `eval x`, and
-- if it succeeds, bind the result to a variable `n` so that I can use.
-- What we want to do is what we would do in an imperative language:

n = eval x
m = eval y
n `safeDiv` m

-- except this sequencing of statements would proceed only if a statement
-- succeeded, i.e., if it didn't return Nothing. The monad allows us to do
-- that. This:

eval x >>=> f

-- will evaluate `x` and if it returns `Just v`, it will call `f` passing
-- it this `v`. So, what should this `f` be? It's just an in-place lambda
-- with which we "grab" the result of `eval x`.

-}

eval :: DivExpr -> Maybe Int
eval (Val n)   =  Just n
eval (Div x y) =  eval x >>=> (\n ->
                    eval y >>=> (\m ->
                      n `safeDiv` m
                    )
                  )

{-
-- This pattern is so common that Haskell has some builtin notation
-- that looks a lot like imperative programming.
-}

-- Using Haskell's builtin notation.
eval2 :: DivExpr -> Maybe Int
eval2 (Val n)   = Just n
eval2 (Div x y) = do 
  n <- eval2 x
  m <- eval2 y
  n `safeDiv` m

e1 = Val 3
e2 = Div (Val 4) (Val 2)
e3 = Div (Val 4) (Val 0)




--------------- EXAMPLE 3: Logging (Keeping state around)

{-
-- Once you are able to customize function composition, you can do
-- some pretty interesting things. For example, you can keep
-- state around. Here, we have a very simple form of state: strings.

-- The functions don't consume state, but they produce state.
-- Once you've overwritten >>=>, this can do whatever you want.
-- In this case, when we have v >>=> g, `f` is a (value1, string1) pair.
-- >>=> gets that, calls `g` passing `value1` as an argument, gets a
-- new (value2, string2) pair back, and it returns (value2, string + string2).

-}

data Log a = Msg a [Char]

f :: Int -> Log Int
f n = Msg (2*n) "f was called"

g :: Int -> Log Int
g n = Msg (n+2) "g was called"

-- The instances of Functor' and AppFunctor' are kind of irrelevant.

instance Functor' Log where
  pure' x = Msg x ""
  fmap' f (Msg x msg) = Msg (f x) msg

-- Get the string `msg1` part of a message, and a whole message (i.e., a value
-- `v` and a string `msg2` pair) and return a new Msg with the same value
-- and the two strings concatenated
concatMsg :: [Char] -> Log a -> Log a
concatMsg msg1 (Msg v msg2) = Msg v (msg1 ++ ", " ++ msg2)

instance AppFunctor' Log where
  (<**>) (Msg f msg) l = concatMsg msg (fmap' f l)

instance Monad' Log where
  -- Call extract the value, i.e., `x` from the LHS, call `f` with
  -- it, get a new message back, and return a new Msg that has the
  -- return value and the two strings concatenated.
  (>>=>) :: Log a -> (a -> Log b) -> Log b
  (Msg x msg) >>=> f =  concatMsg msg (f x)

{-
-- If going through concatMsg seems complicated, you could do implement
-- it as follows:

(Msg x1 msg1) >>=> f = case f x1 of
                        Msg x2 msg2 -> Msg x2 (msg1 ++ ", " ++ msg2)
-}

logToStr :: Log Int -> [Char]
logToStr (Msg i s) = "Value: " ++ show i ++ ", Log: " ++ s

res = f 2 >>=> g -- This is the same as: (pure' 2) >>=> f >>=> g
res_2 = f 2 >>=> g >>=> f

{-
-- >>=> could be doing more elaborate things. `f` and `g` could
-- be producing hash maps which `>>=>` unifies.

-- Note that if `f` and `g` also consumed state, then there would
-- be no need for a monad; they would get a Log and return a Log.
-}