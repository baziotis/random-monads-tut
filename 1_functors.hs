{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-
-- Any generic type T might want to define a way in which, having a function (call
-- it f) a->b and an object T a, you can apply `f` on `a` to get a `b` and wrap the
-- result in a T. In other words, it’s the classic analogy where you think of T as
-- a box, and T says “you can unbox me, transform the thing inside into something,
-- and box that result back to a T”.

-- Notice that T must be a pretty generic parametric type for it to be a Functor.
-- That is, fmap gets a function that can return _any_ type, and it should be
-- possible to box this result into a T. So, T should be able to box anything. The
-- types we know like a list or Maybe are such types.

-- A Functor is useful because if you have a function a -> b and you want to apply
-- it to an `a` that is wrapped in a `t`, then you don't need to define a _new_
-- function t a -> t b. You can use `fmap` to apply the function to the contents of
-- `t`. Notice, however, that what you're getting is also a `t` and not `b`.

-- If you've used `map` at all, you know why a Functor is useful. If you have a
-- function that converts a Int -> String and a list of `Int`, you can use `map` to
-- apply this function to every element of the list to get a new list of String. If
-- list was not a functor, you would have to define a new function [Int] -> [String]
-}

class Functor' t where
  -- We define this utility function which will be useful in later parts. It
  -- trivially wraps an object into the type. So, for any
  -- `x`, the corresponding Maybe is just `Just x`.
  pure' :: a -> t a
  fmap' :: (a -> b) -> t a -> t b

instance Functor' [] where
  pure' x = [x]

  fmap' = map

instance Functor' Maybe where
  pure' x = Just x

  fmap' f Nothing = Nothing
  -- By using pattern matching, we implicitly unbox the second argument
  -- (the t a) to get the `a` from it (bound here to `x`). In other words,
  -- I deliberately did _not_ write `fmap' f m`, because then we don't have
  -- access to the element inside. By using pattern matching, I bind the
  -- element to the variable `x`.
  fmap' f (Just x) = Just (f x)

neg' :: Int -> Int
neg' x = -x

tmp = fmap' neg' (Just 2) -- = Just -2