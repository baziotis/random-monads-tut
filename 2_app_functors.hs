{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- I don't want to overcomplicate things with Haksell's
-- import system, so, we'll just paste the stuff
-- we need from 1_functors.hs here.

----- FUNCTORS PRELUDE -----

myAdd :: Int -> Int -> Int
myAdd x y = x + y

class Functor' t where
  pure' :: a -> t a
  fmap' :: (a -> b) -> t a -> t b

instance Functor' [] where
  pure' :: a -> [a]
  pure' x = [x]

  fmap' :: (a -> b) -> [a] -> [b]
  fmap' = map

instance Functor' Maybe where
  pure' :: a -> Maybe a
  pure' x = Just x

  fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap' f Nothing = Nothing
  fmap' f (Just x) = Just (f x)

----- END FUNCTORS PRELUDE -----

{-
-- `fmap` works when you have _one_ `t` and you want
-- to transform the contents to another `t`.
-- But, what if you have a pair of `t`'s? For example, if
-- you want to add two `Maybe Int` and you want to add
-- their contents?
--
-- The concept of `fmap` should generalize. We would need a
-- function `fmap2` as follows:

fmap2 :: (a -> b -> c) -> t a -> t b -> t c

-- That is, we get a `t a` and a `t b`, we unbox `a` and `b`
-- from them, we plug them into the function (a -> b -> c),
-- we get a `c` back, and we wrap that into a `t` to get
-- a `t c`. For example, this `fmap2` would be useful
-- if you wanted to add two `Maybe`'s. You pass myAdd and two
-- `Maybe`'s.

-- In general, this concept can be generalized to any number
-- of `t`'s:

fmap1 :: (a -> b) -> t a -> t b
fmap2 :: (a -> b -> c) -> t a -> t b -> t c
fmap3 :: (a -> b -> c -> d) -> t a -> t b -> t c -> t d
...

-- Now, we want a _single_ function that can act as any of those.
-- So, we want a generalized `fmap` -- `genfmap` -- that would
-- have a type signature like this:

genfmap :: (a1 -> a2 -> ... -> aN -> b) -> t a1 -> t a2 -> ... -> t aN -> t b

-- But obviously, we cannot do that. So, we need a hack... To understand the hack,
-- let us first define `fmap2` for Maybe Int, to make it concrete.
-}


fmap2_maybe_int :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
fmap2_maybe_int _ Nothing Nothing = Nothing
fmap2_maybe_int f (Just x) (Just y) = Just (f x y)

{-
-- Now we can do this:
-}

tmp = fmap2_maybe_int myAdd (Just 1) (Just 2)  -- = Just 3


{-
-- Now, let's try to think how we can get the _effect_ of `fmap2_maybe_int`
-- but using only `fmap`. One of the problems is that `fmap2` gets a function
-- that gets _two_ arguments but `fmap` passes _one_ argument to it. What is
-- going to happen if we pass to `fmap` a function that gets _two_ arguments?
-}

tmp2 = fmap' myAdd (Just 1)

{-
-- In a standard imperative language, this would result in an error (because
-- `fmap` will pass one argument to a function that supposedly expects two). But
-- as we mentioned in the warmup, we can partially evaluate functions by
-- passing fewer arguments and the result is a _function_. So, `tmp` has type
-- `Maybe (Int -> Int)` and in particular, it is this the following term:

Just (myAdd 1)

-- Now, we can get the function inside `Just` and pass that _again_ to `fmap`
-- thereby resulting in full evaluation (i.e., we provided all the arguments).
-}

tmp3 = fmap' (myAdd 1) (Just 2)  -- = Just 3

{-
-- `tmp2` is the same as doing `fmap2 (+) (Just 2) (Just 3)`, but we did
-- it progressively. Here's `fmap2` for `Maybe` that uses this idea.
-}

fmap2_maybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
fmap2_maybe f x y = case fmap' f x of -- Call fmap passing f
  -- This will partially apply `f` and return this partially applied
  -- function, i.e., f2, wrapped inside a Just.
  --
  -- Extract it and pass it again through `fmap` to get a full
  -- evaluation.
  (Just f2) -> fmap' f2 y

{-
-- The only problem is that we cannot take the result of `fmap` and pass
-- it to another `fmap`. That is, this does not work:

fmap (fmap (+) (Just 2)) (Just 3)

-- The reason is that `fmap` returns `t (a -> b)` but `fmap` expects
-- (a -> b). In other words, in general the `fmap` returns a "wrapped"
-- result but `fmap` gets an "unwrapped" function.
--
-- To solve that, we will create an operator that "unwraps" the function
-- before passing it to `fmap`. If a type defines this operator, it is
-- an applicative functor. The syntax below says that an AppFunctor''
-- is also a Functor'. This is only to not have to redefine pure'.
-}

class (Functor' t) => AppFunctor'' t where
  genfmap :: t (a -> b) -> t a -> t b

instance AppFunctor'' Maybe where
  -- `genfmap` gets a function wrapped in a `t`, unwraps it (
  -- through pattern matching), and then passes this function
  -- to `fmap` along with the seconnd argument. The result
  -- of that is `t b`.
  genfmap :: Maybe (a -> b) -> Maybe a -> Maybe b
  genfmap Nothing _ = Nothing
  genfmap (Just f) ma = fmap f ma

{-
-- Now we can do this:
-}

inner = genfmap (pure' myAdd) (Just 1)  -- = Just (myAdd 1)
tmp4 = genfmap inner (Just 2)  -- = Just 3

{-
Or expanded:
-}

tmp5 = genfmap (genfmap (pure' myAdd) (Just 2)) (Just 3)

{-
-- You see why `pure` is useful. To get the ball rolling, we wrap the
-- initial function into a Maybe.
--
-- This may look much less convenient than our ideal `genfmap` above.
-- In practice, however, we define `genfmap` as an infix operator.
-- Usually, <*> is used, but because this is defined in Haskell, we'll
-- use our own <**>
-}

class Functor' t => AppFunctor' t where
  (<**>) :: t (a -> b) -> t a -> t b

instance AppFunctor' Maybe where
  (<**>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  -- Observe the syntax for an infix operator.
  Nothing <**> _ = Nothing
  (Just f) <**> ma = fmap' f ma
  -- We could have written this:
  -- (<**>) Nothing _ = Nothing
  -- (<**>) (Just f) ma = fmap' f ma

{-
-- Now, we can do the much more convenient:
-}

tmp6 = Just (+) <**> Just 1 <**> Just 2  -- = Just 3
tmp7 = Just (+) <**> (Just (+) <**> Just 1 <**> Just 2) <**> Just 3  -- = Just 6

{- 
(ADVANCED)

-- Lists are applicative functors too.
-}

instance AppFunctor' [] where
  (<**>) :: [a -> b] -> [a] -> [b]
  (<**>) [] _ = []
  fs <**> xs = [f x | f <- fs, x <- xs]

tmp8 = pure' (+) <**> [1, 2] <**> [3, 4]  -- = [4, 5, 5, 6]
