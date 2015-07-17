{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Monad.State
import Data.Foldable
import Data.Traversable
import Data.Map as M
import Data.Set as S

data Tree a = Tree a [Tree a]
  deriving (Show, Functor, Foldable, Traversable)

postIncrement :: Enum s => State s s
postIncrement = do val <- get
                   put (succ val)
                   return val

-- Works for any Traversable, not just trees!
tag :: (Enum s, Traversable t) => s -> t a -> t (a, s)
tag init tree = evalState (traverse step tree) init
    where step a = do tag <- postIncrement
                      return (a, tag)

fooTree :: Tree Integer
fooTree = Tree 1 [Tree 2 [], Tree 3 [Tree 4 [], Tree 5 [] ] ] 

fooMap :: M.Map Integer String
fooMap = M.fromList [(1, "foo"),(2, "bar"),(3, "baz"),(4, "qux")]

fooSet :: S.Set String
fooSet = S.fromList ["blurb", "gubble", "frob"]

-- now let's make our own Tree with no deriving and see how that works
data MyTree a = Empty | Leaf a | Node (MyTree a) a (MyTree a)
  deriving Show

instance Functor MyTree where
  fmap f (Empty)  = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Foldable MyTree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Traversable MyTree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

--instance Show (MyTree a) where
--  show Empty = ""
--  show (Leaf a) =   show a
--  show (Node l k r) = (show l) ++ show k ++ (show r)

fooMyTree :: MyTree Integer
fooMyTree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)


fmap      :: Functor f                      => (a -> b)   -> f a -> f b
                          fn from a -> b      Tree a        gives you   Tree b 
                          so if a & b are the same time you get same type of Tree back

traverse  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
                          fn from a -> b wrapped in f         Tree a          gives you    Tree b wrapped in f
                    ie    fn from Integer -> Maybe Number     Tree Integer    gives        Maybe Tree Number

sequence  :: Monad m                        => t (m a)    -> m (t a)
                        Tree of M a         gives you      Monad Tree a
                        Tree of Maybe a     gives you      Maybe Tree a
