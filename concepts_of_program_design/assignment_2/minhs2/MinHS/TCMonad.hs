module MinHS.TCMonad ( TypeError (..)
                     , TC
                     , freshNames
                     , runTC
                     , typeError
                     , fresh
                     ) where

import MinHS.Syntax
import Control.Applicative
import Control.Monad

data TypeError = TypeMismatch Type Type
               | OccursCheckFailed Id Type
               | NoSuchVariable Id
               | NoSuchConstructor Id
               | MalformedAlternatives
               | ForallInRecfun
               deriving (Show)

-- very very important
newtype TC a = TC ([Id] -> Either TypeError ([Id], a)) -- qui sta in succo dell'uso di right, left, frst second, perchè mappa su di un either
-- la nostra monade è una funzione che mette dentro fresh names in un programma
--qua la cosa è stata leggermente modificata perchè ???

instance Monad TC where -- TC is an instance of the monad class
  return x = TC (\s -> Right (s,x))  
  (TC a) >>= f  = TC (\s -> case a s of Left x -> Left x
                                        Right (s',v) -> let TC b = f v
                                                         in b s')
-- f è una funzione che prende un x come parametro e ritorna un TC b ------ okaaaay, adesso si spiegano un po' di cose
--- okaaay, capito
instance Applicative TC where
  pure = return  
  (<*>) = ap

instance Functor TC where
  fmap = ap . pure

freshNames :: [Id]
freshNames = map pure ['a'..'z'] ++ map ((++) "a" . show) [1..]

runTC :: TC a -> Either TypeError a
runTC (TC f) = fmap snd (f freshNames)  -- snd returns the second value of the tuple
-- qui non c'entra nulla la funzione >>= della monade
--- okay, capito

typeError :: TypeError -> TC a
typeError = TC . const . Left -- non ha il parametro nè sulla sinistra nè sulla destra perchè è lo stesso da mettere in fondo

fresh :: TC Type
fresh = TC $ \(x:xs) -> Right (xs,TypeVar x)
