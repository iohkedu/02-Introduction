# Weekly Assignments 5

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the scope
of a task, by all means ask.
Please try to write high-quality code at all times! This means in particular that
you should add comments to all parts that are not immediately obvious. Please
also pay attention to stylistic issues. The goal is always to submit code that does
not just correctly do what was asked for, but also could be committed without
further changes to an imaginary company codebase.

## W5.1 Packaging

Prepare a Cabal package to contain all your solutions so that it can easily be
built using cabal-install or stack.
Note that one package can contain one library (with arbitrarily many modules)
and possibly several executables and test suites.
Please include aREADMEfile in the end explaining clearly where within the
package the solutions to the individual subtasks are located.
Please do NOT try to upload your package to Hackage.

## W5.2 Some prisms

In this exercise, we want to explore some non-standard prisms.

### W5.2.1

Define a prism

```haskell id="P02" file="src/P02.hs"
module P02 where

import           Numeric.Natural

_Natural :: Prism'Integer Natural

-- ^ Examples of _Natural
-- >>> Preview _Natural 42
-- Just 42
-- >>> Preview _Natural -7
-- Nothing
```

(You can find theNaturaltype of arbitrary-precision natural numbers in module
Numeric.Natural in the base libraries.)

### W5.2.2

Define a function of type

```haskell id="P02"
  <<function_documentation_for_TheOne>>

_TheOne :: Eq a => a -> Prism' a ()
_TheOne = undefined

```

Given an /a/, the resulting prism’s focus should be the given element:

```haskell id="function_documentation_for_TheOne"
-- | focus should be the given element
--
-- Examples:
--
-- >>> preview (_TheOne'x') 'x'
-- Just ()
--- >>> preview (_TheOne'x') 'y'
-- Nothing
-- >>> preview (_TheOne'x') ()
--'x'
```

### W5.2.3

Let’s define the following wrapper type:

```haskell id="P02"
newtype Checked a = Checked { unChecked :: a } deriving Show
```

Define a function

```haskell id="P02"
  <<function_documentation_for_Check>>
_Check :: (a -> Bool) -> Prism'a (Checked a)
_Check = undefined
```

The idea is that the prism finds only elements that fulfill the given predicate.
(This will only be a law-abiding prism if we agree to never put anainto the
Checked-wrapper which does not satisfy the predicate.)

```haskell id="function_documentation_for_Check"
-- >>> preview (_Check odd) 42
-- Nothing
-- >>> preview (_Check odd) 17
-- Just (Checked {unChecked = 17})
-- >>> review (_Check odd) (Checked 3)
-- 3
```

## W5.3

Consider the following tree type:

```haskell id="P03" file="src/P03.hs"
module P03 where

data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show
```

### W5.3.1

Define three traversals

```haskell id="P03"
inorder, preorder, postorder :: Traversal (Tree a) (Tree b) a b
inorder = undefined

preoder = undefined

postorder = undefined
```

which traverse the nodes in _inorder_ (left, value, right), _preorder_ (value, left,
right) and _postorder_ (left, right, value), respectively.

### W5.3.2

Define two functions

```haskell id="P03"
printNodes :: Show a => Traversal (Tree a) a -> Tree a -> IO ()
printNodes = undefined

labelNodes
  :: Traversal (Tree a) (Tree (a, Int)) a (a, Int) -> Tree a -> Tree (a, Int)
labelNodes = undefined
```

given a traversal, =printNodes= should print all values stored in the tree
_in order of the traversal_ , whereas =labelNodes= should label all nodes,
starting at 1, again in the order of the given traversal.

Test your functions oninorder,preorderandpostorderfrom W7.4.1 on at
least the following example tree:

```haskell id="P03"
tree :: Tree Char            --       c
tree = Node                  --      / \
  (Node                      --     /   \
        (Node Tip 'a' Tip)   --    b     d
        'b'                  --   / \   / \
        Tip
  )                         --  /
  'c'                        -- a
  (Node Tip'd' Tip)         -- / \
```

## W5.4 Delayed computations

The type constructor =Delayed= can be used to describe possibly non-terminating
computations in such a way that they remain “productive”, i.e., that they
produce some amount of progress information after a finite amount of time.

```haskell id="P04" file="src/P04.hs"
module P04 where

import           Control.Alternative.Free

data Delayed a = Now a | Later (Delayed a)
```

We can now describe a productive infinite loop as follows:

```haskell id="P04"
loop :: Delayed a
loop = Later loop
```

This is productive in the sense that we can always inspect more of the result,
and get more and more invocations ofLater.

We can also useLaterin other computations as a measure of cost or effort. For
example, here is a version of the factorial function in the =Delayed= type:

```haskell id="P04"
factorial :: Int -> Delayed Int
factorial = go 1
 where
  go acc n | n <= 0    = Now acc
           | otherwise = Later (go (n \* acc) (n - 1))
```

We can extract a result from aDelayedcomputation by traversing it all the way
down until we hit a =Now=, at the risk of looping if there never is one:

```haskell id="P04"
unsafeRunDelayed :: Delayed a -> a
unsafeRunDelayed (Now   x) = x
unsafeRunDelayed (Later d) = unsafeRunDelayed d
```

### Subtask 5.4.1

Define a function

```haskell id="P04"
-- | 5.4.1
runDelayed :: Int -> Delayed a -> Maybe a
runDelayed = undefined
```

that extracts a result from a delayed computation if it is guarded by at most
the given number of =Later= constructors, and =Nothing= otherwise.

### Subtask 5.4.2

The type =Delayed= forms a monad, where return is Now, and />>=/combines the
number of=Later= constructors that the left and the right argument are guarded
by.
Define the Functor,Applicative, and Monad instances for =Delayed=.

```haskell id="P04"
-- | 5.4.2 Functor,Applicative, and Monad instances for =Delayed=.
```

### Subtask 5.4.3

Assume we have

```haskell id="P04"
-- | 5.4.3

tick :: Delayed ()
tick = Later (Now ())

psum :: [Int] -> Delayed Int
psum xs = sum <\$> mapM (\x -> tick >> return x) xs
```

Describe what psum does. As Haddock comment

### Subtask 5.4.4

The type =Delayed= is actually a free monad. Define the functor =DelayedF= such
that =Free DelayedF= is isomorphic to =Delayed=, and provide the witnesses of the
=isomorphism=:

```haskell id="P04"
-- | 5.4.4

fromDelayed :: Delayed a -> Free Delayed a
fromDelayed = undefined

toDelayed :: Free Delayed a -> Delayed a
toDelayed = undefined
```

### Subtask 5.4.5

We can also provide an instance ofAlternative:

```haskell id="P04"

instance Alternative Delayed where
empty = loop
(<|>) = merge

merge :: Delayed a -> Delayed a -> Delayed a
merge (Now x)   _         = Now x
merge _         (Now   x) = Now x
merge (Later p) (Later q) = Later (merge p q)
```

Define a function

```haskell id="P04"
-- | 5.4.5
<<function_documentation_for_firstSum>>
firstSum :: [[Int]] -> Delayed Int
firstSum = undefined

```

that performs =psum= on every of the integer lists and returns the result that can
be obtained with as few delays as possible.

```haskell id="function_documentation_for_firstSum"
-- Example:
--
-- >>> runDelayed 100 $ firstSum [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
-- Just 9.
```

### Subtask 5.4.6

Unfortunately, =firstSum= will not work on infinite (outer) lists and

```haskell
runDelayed 200 $
  firstSum $
    cycle [repeat 1, [1,2,3], [4,5], [6,7,8], cycle [5,6]]
```

will loop.

The problem is that =merge= schedules each of the alternatives in a fair way. When
usingmergeon an infinite list, all computations are evaluated one step before
the firstLateris produced. Define

```haskell id="P04"
biasedMerge :: Delayed a -> Delayed a -> Delayed a
biasedMerge = undefined

```

that works on infinite outer lists by running earlier lists slightly sooner than
later lists. Write

```haskell id="P04"
<<documentation_for_biasedFirstSum_function>>

biasedFirstSum :: [[Int]] -> Delayed Int
biasedFirstSum = undefined

```

which is =firstSum= in terms of =biasedMerge=. Note that =biasedFirstSum= will
not necessarily always find the shortest computation due to its biased nature,
but it should work on the infinite outer list example above and also in

```haskell id="documentation_for_biasedFirstSum_function"
-- | 5.4.6 firstSum in terms of biasedMerge
-- >>> :{
--   runDelayed 200 $
--     biasedFirstSum $
--       replicate 100 (repeat 1) ++ [[1]] ++ repeat (repeat 1)
-- :}
-- Just 1
```

to return Just 1.

## W5.5 Traversals (Bonus!)

Implement the following functions operating on traversals. This is quite tricky,
but if you manage it, you have really understood traversals!

```haskell id="P05" file="src/P05.hs"
module P05 where

-- >>> set (heading each) "Addis Ababa"'x'
-- "xddis Ababa"
heading :: Traversal' s a -> Traversal' s a
heading = undefined

-- >>> set (tailing each) "Addis Ababa"'x'
-- "Axxxxxxxxxx"
tailing :: Traversal' s a -> Traversal' s a
tailing = undefined

-- >>> set (taking 3 each) "Addis Ababa"'x'
-- "xxxis Ababa"
taking :: Int -> Traversal's a -> Traversal's a
taking = undefined

-- >>> set (dropping 3 each) "Addis Ababa"'x'
-- "Addxxxxxxxx"
dropping :: Int -> Traversal's a -> Traversal's a
dropping = undefined

-- >>> set (filtering (<'d') each) "Addis Ababa"'x'
-- "xddisxxxxxx"
filtering :: (a -> Bool) -> Traversal's a -> Traversal's a
filtering = undefined

-- >>> set (element 1 each) "Addis Ababa"'x'
-- "Axdis Ababa"
element :: Int -> Traversal's a -> Traversal's a
element = undefined

-- Helper functions with the following signatures might be useful
trans1 :: Applicative f => (a -> f a) -> (a -> Compose (State Bool) f a)
trans1 = undefined

trans2 :: Applicative f => (a -> f a) -> (a -> Compose (State Int) f a)
trans2 = undefined

trans3 :: Applicative f => (a -> Bool) -> (a -> f a) -> (a -> f a)
trans3 = undefined
```
