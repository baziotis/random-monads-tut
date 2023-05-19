{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

--
-- ####### CURRIED FUNCTIONS #########
--

myAdd :: Int -> Int -> Int
myAdd x y = x + y

{-
-- We can supply all arguments as we would in any language and get
-- back an Int.
> myAdd 1 2
3

-- But we can also supply _some_ of the arguments and get back
-- a partially evaluated function. If we try it, we'll get an
-- error, but it's a printing error (i.e., the Haskell
-- interpreter does not know how to print it).

> myAdd 1
[FAILS]

But we can see its type:
> :t myMax 1
myAdd 1 :: Int -> Int

-- In terms of a procedural language, if the original function looked
-- like this:
def myAdd(a, b):
  return a + b

-- then myAdd 1 is another function that looks like this:
def myAdd_1(b):
  return 1 + b

-- We can also pass functions as arguments. For example:
-}

plusOneF :: (Int -> Int) -> Int -> Int
plusOneF f b = f b + 1

{-
> plusOneF (myAdd 1) 2
4
-}

{-
-- Let's now talk about `map`. It takes a function and applies
-- it to all elements of a list to get a new list. Its iterative
-- implementation would look like this:

def map(f, l):
  out = []
  for x in l:
    out.append(f(x))
  return out

-- Note that `a`, `b` below are type variables. They make the function generic.
-- It's sort of like templates in C++ (except you know... the Haskell version
-- is not an embarassment of generic programming).
-}

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

{-
> map' (myAdd 1) [1, 2, 3]
[2, 3, 4]
-}


{-

#########################################################################
#########################################################################
THE NEXT PART IS A BIG LONG AND NOT THAT NECESSARY FOR THE REST OF
THE TUTORIAL. YOU PROBABLY WON'T MISS A LOT, REGARDING THIS TUTORIAL,
IF YOU SKIP IT AND GO DIRECTLY TO THE DEFINITION OF Maybe (AT THE END) 
AND THE COMMENT ABOVE IT.
#########################################################################
#########################################################################

-}


--
-- ####### SUM TYPES #########
--

{-
-- This is not that necessary for the tutorial but I think
-- it's an important topic with many bad explanations. To
-- motivate sum types, let's think of an API for a function
-- that opens a file, like `open()` in Python.

-- The first argument will be the file name and it will
-- be a string. The second arument is the mode, i.e.,
-- whether we read, write or append to the file. Usually,
-- this is a string argument:

open(filename : string, mode : string)

-- However, this does not make an effective use of the type system.
-- In particular, there are only 3 acceptable values for `mode`:
-- "r", "w" and "a". But, a `string` has an infinite number of
-- possible values. The user can pass any of those without the
-- type system protecting them.

-- A standard alternative is to use an already defined type
-- with fewer available values, and then have an implicit
-- mapping from these values to the values we care about.
-- To make that concrete, imagine that we had only two available
-- modes: read and write. Then, we could use the type `bool`,
-- which has only two available values: true and false.
-- Now, we need an implicit mapping from these values to
-- read and write, e.g., true means read, false means write.

... open(filename : string, mode : bool)

-- This is a bit better in the sense that now the user can
-- pass only two values, both of which are acceptable options.
-- The problem is that we need this implicit mapping which
-- is not part of the type system. It happens externally,
-- i.e., through the operations. The other problem is that
-- we have _three_ options, not two.

-- What we need is a new type `FileMode`, which has only
-- three possible values: Read, Write and Append. And that's
-- what we can do in Haskell:

-}

data FileMode = Read | Write | Append

{-

-- A common question when seeing that is to ask "what is the value
-- of Read?". But this question results from some confusion. `Read`
-- does not have a value, i.e., it's not a variable or a constant,
-- but rather _is_ a value. We have some syntactic constructs which
-- are values - of some type. `Read` is a syntactic construct
-- of type `FileMode`.

-- Think of literals in most programming languages.
-- For example, 0 is an integer literal, i.e., a value, of type `int`.
-- 1 is also a literal, and generally, any literal following
-- the decimal notation is a value of type `int`. In the same
-- that 0 is a value of type `int`, `Read` is a value of type
-- `FileMode`. Further, `Read`, `Write`, and `Append` are the only
-- values of type `FileMode`.

-- It is perhaps important here to clarify that `FileMode` as defined
-- above in Haskell, is _not_ the same as the following `FileMode`
-- enum in e.g., C:

enum FileMode {
  READ,
  WRITE,
  APPEND
}

-- An enum in C, although people in practice use it to define a new
-- type with a certain number of values, is actually a way to define
-- integer constants. So, READ is 0, WRITE is 1 etc. Thats what
-- leads to the confusion of asking what the value of `Read` is.

-- Continuing with practical problems in mind, some values need to
-- be composite, i.e., they need to hold some other value of
-- some potentially other type. A classic example is a token type
-- in a lexer. We have several kinds of tokens like left parenthesis,
-- right parenthesis etc. and so, we would create a new type
-- TokenKind as we explained earlier:

data FileMode = LPar | RPar | Plus | ...

-- However, for _some_ kinds of tokens, we need a companion value.
-- For example, if we have a integer literal token, we need the value
-- of this literal. Or, if you have an identifier, you need the string
-- representation of the identifier. So, we have something like this:

data FileMode = LPar | RPar | Plus | ... | IntLit Int | Ident String

-- The classic way we'd do that in C would be something like this:

enum TokenKind {
  LPAREN,
  RPAREN,
  PLUS,
  ...
  IDENT,
  INT_LIT
}

struct Token {
  TokenKind kind;
  union {
    int v;
    string id;
  }
}

-- The problem with this is that it is not expressed anywhere, in
-- the type system, that the field `v` in `Token` should be accessed
-- only if `kind` is `INT_LIT`. In other words, there is no pairing
-- of which `kind`'s match with the other fields. So, we can access
-- the field `v` for any `kind` and the type system won't correct
-- us.
-}

{-
-- Finally, we can have parametric types, which are again like templates
-- in C++. `Maybe` is the equivalent of `Optional` in C++ or Rust.
-- It is used to capture the result of an operation that may fail.
-- For example, say a function returns an `Int`, but it may fail.
-- Then, it will return `Maybe Int`. If it returns `Just x`, for some
-- `x` that is `Int`, it means it succeeded and we can extract
-- the `Int`. Otherwise, it returns `Nothing` to signify that it failed.
-}
data Maybe a = Just a | Nothing