module Decorator where 

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.State          -- from purescript-transformers
import Control.Monad.State.Class    -- from purescript-transformers
import Control.Monad.Maybe.Trans    -- from purescript-transformers
import Data.Foldable
import Data.Traversable
import Data.Enum
import Data.Tuple
import Data.Monoid

postIncrement :: forall s. (Enum s) => State s s
postIncrement = do val <- get
                   put $ fromMaybe(succ val)
                   return val

-- Works for any Traversable, not just trees!
tag :: forall s t. (Enum s, Traversable t) => s -> t a -> t (Tuple a s)    -- no native tuples in Purescript!
tag init tree = evalState (traverse step tree) init
    where step a = do tag <- postIncrement
                      return (Tuple a tag)

-- now let's make our own Tree with no deriving and see how that works
data MyTree a = Empty | Leaf a | Node (MyTree a) a (MyTree a)

instance functorMyTree :: Functor MyTree where        -- no need for fmap distinction in Purescript, just use map
  map f (Empty)  = Empty
  map f (Leaf x) = Leaf (f x)
  map f (Node l k r) = Node (map f l) (f k) (map f r)

instance foldableMyTree :: Foldable MyTree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `append` f k `append` foldMap f r   -- append from Semigroup instead of mappend from monoid
  foldr f z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
  foldl f z Empty = z
  foldl f z (Leaf x) = f x z
  foldl f z (Node l k r) = foldl f (f k (foldl f z l)) r                   -- foldl & foldr required in Purescript

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
  print $ tag 0 fooMyTree
