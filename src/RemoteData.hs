module RemoteData where

data RemoteData e a 
  = NotAsked
  | Loading
  | Failure e
  | Success a

instance Functor (RemoteData a) where  
  fmap _ NotAsked     = NotAsked
  fmap _  Loading     = NotAsked
  fmap _ (Failure e)  = Failure e
  fmap f (Success a)  = Success (f a)

instance Applicative (RemoteData a) where 
  pure                        = Success

  Loading     <*> _           = Loading
  NotAsked    <*> _           = NotAsked
  (Failure e) <*> _           = Failure e
  (Success f) <*> (Success x) = Success (f x)

instance Monad (RemoteData a) where  
  return a = Success a

  NotAsked  >>= f = NotAsked  
  Loading   >>= f = Loading  
  Failure e >>= f = Failure e
  Success a >>= f = f a
