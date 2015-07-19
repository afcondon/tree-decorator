module ShowRecord where

import Prelude
import Control.Monad.Eff.Console

newtype Foo = Foo { content :: String, label :: Int } 

instance showFoo :: Show Foo where
  show (Foo { content = n, label = i })    =  show i ++ " " ++ show n

myFoo :: Foo
myFoo =  Foo { content: "hello", label: 0 }

main = do
	log "hello from showrecord"
	print myFoo