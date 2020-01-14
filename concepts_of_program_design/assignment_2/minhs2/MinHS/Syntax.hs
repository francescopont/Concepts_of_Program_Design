module MinHS.Syntax where

import Data.List

type Id = String

type Program = [Bind]

data Exp
    = Var  Id       -- DONE
    | Prim Op       -- DONE
    | Con  Id       -- DONE               -- con stays for constructor: we have True, False, Cons, NIl, ( just as in assignment 1) Pair, Unitel ( for product types), Inl, Inr ( for sum types)
                                -- NB!!! no cons or NIl because we do not have lists
    | Num  Integer  --DONE
    | App  Exp Exp  -- DONE
    | If   Exp Exp Exp  --DONE
    | Let  [Bind] Exp
    | Recfun Bind
    | Letrec [Bind] Exp
    | Case Exp [Alt]
    deriving (Read,Show,Eq)

data Alt = Alt Id [Id] Exp
    deriving (Read,Show,Eq)


data Bind = Bind Id (Maybe QType) [Id] Exp
  deriving (Read,Show,Eq)

data Op = Add
        | Sub
        | Mul
        | Quot
        | Rem
        | Neg
        | Gt
        | Ge
        | Lt
        | Le
        | Eq
        | Ne
        | Fst
        | Snd
        deriving (Show, Eq, Read)

data QType = Forall Id QType
           | Ty Type
           deriving (Read, Show, Eq, Ord)

data Type = Arrow   Type     Type
          | Prod    Type     Type
          | Sum     Type     Type
          | Base    BaseType
          | TypeVar Id
          deriving (Read, Show, Eq, Ord)

data BaseType = Unit
              | Bool
              | Int
              deriving (Read, Show, Eq, Ord)

binApply :: Exp -> Exp -> Exp -> Exp
binApply e1 e2 e3 = App (App e1 e2) e3


allTypes :: (QType -> QType) -> Exp -> Exp
allTypes f (App e1 e2) = App (allTypes f e1) (allTypes f e2) -- this is very similar to the foldr
allTypes f (If c t e)  = If (allTypes f c) (allTypes f t) (allTypes f e)
allTypes f (Let bs e)  = Let (map (allTypesBind f) bs) (allTypes f e)
allTypes f (Recfun b)  = Recfun (allTypesBind f b)
allTypes f (Letrec bs e)  = Letrec (map (allTypesBind f) bs) (allTypes f e)
allTypes f (Case e alts)  = Case (allTypes f e) (map (allTypesAlt f) alts)
allTypes f x           = x

allTypesBind f (Bind n ty xs e) = Bind n (fmap f ty) xs (allTypes f e)

allTypesAlt f (Alt t ps e) = Alt t ps (allTypes f e)