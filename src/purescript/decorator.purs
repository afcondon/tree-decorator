module Decorator where 

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.State          -- from purescript-transformers
import Control.Monad.State.Class    -- from purescript-transformers
import Control.Monad.Maybe.Trans    -- from purescript-transformers
import Data.Foldable
import Data.Traversable
--import Data.Enum
--import Data.Tuple
import Data.Maybe
import Data.Monoid

class SimpleEnum a where
  succ :: a -> a
  pred :: a -> a

instance simpleEnumInt :: SimpleEnum Int where
  succ i = i + 1
  pred i = i - 1

instance simpleEnumChar :: SimpleEnum Char where
  succ c = c
  pred c = c -- dummy implementations just to test typing

{-
postIncrement :: forall s. (SimpleEnum s) => State s s
postIncrement = do val <- get
                   put (succ val)
                   return val

-- Works for any Traversable, not just trees!
tag :: forall s t. (SimpleEnum s, Traversable t) => s -> t a -> t { node :: a, tag :: s }    -- no native tuples in Purescript!
tag init tree = evalState (traverse step tree) init
    where step a = do tag <- postIncrement
                      return { node: a, tag: tag}
-}

-- now let's make our own Tree with no deriving and see how that works
data MyTree a = Empty | Leaf a | Node (MyTree a) a (MyTree a)

instance functorMyTree :: Functor MyTree where        -- no need for fmap distinction in Purescript, just use map
  map f (Empty)  = Empty
  map f (Leaf x) = Leaf (f x)
  map f (Node l k r) = Node (map f l) (f k) (map f r)

instance foldableMyTree :: Foldable MyTree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `append` f k `append` foldMap f r   -- append from Semigroup in PS instead of mappend from monoid in Haskell
-- foldl & foldr required in Purescript but not in Haskell
-- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr                  f               z    Empty = z
  foldr                  f               z   (Leaf x) = f x z
  foldr                  f               z   (Node l k r) = left
    where
      right  = foldr  f z r     -- yields b
      center = f k right        -- a -> b -> b 
      left   = foldr f center l -- yields b
--  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl                  f                z    Empty = z
  foldl                  f                z   (Leaf x) = f z x
  foldl                  f                z   (Node l k r) = right
    where
      left   = foldl f z l       -- yields b
      center = f left k          -- b -> a -> b
      right  = foldl f center r  -- yields b             

instance traversableMyTree :: Traversable MyTree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
  sequence = traverse id
  -- | really not sure how you'd actually define sequence for a tree except in terms of traverse
  --sequence m Empty        = return Empty
  --sequence m (Leaf x)     = return Leaf (f x)
  --sequence m (Node l k r) = something involving monadic bind of the arms of the tree and/or return (Node) 

instance showMyTree :: Show (MyTree String) where
  show Empty = ""
  show (Leaf a) =   show a
  show (Node l k r) = (show l) ++ show k ++ (show r)

fooMyTree :: MyTree String
fooMyTree = Node (Node Empty "left" Empty) "center" (Node Empty "right" Empty)

main = do
  log "hello world"
  --let bar = tag 0 fooMyTree
  print fooMyTree
  let bar = succ 0
  let baz = succ 'a'
  print bar
