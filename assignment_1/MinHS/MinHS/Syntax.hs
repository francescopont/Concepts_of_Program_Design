module MinHS.Syntax where

import Data.List

type Id = String

type Program = [Bind]

data Exp
    = Var Id
    | Prim Op
    | Con Id -- con stays for constructor: we have only  True, False, Cons and NIl 
    | Num Integer
    | App Exp Exp
    | If Exp Exp Exp
    | Let [Bind] Exp
    | Recfun Bind
    | Letrec [Bind] Exp 
    deriving (Read,Show,Eq)

data Bind = Bind Id Type [Id] Exp 
-- id is the name of the bind
-- [id] are the independent variables inside Expr ( n-ary functions...)
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
        | Head
        | Tail
        | Null
        deriving (Show, Eq, Read)

data Type = Arrow Type Type
          | TypeApp Type Type -- ? for container types
          | TypeCon TyCon
          deriving (Read, Show, Eq, Ord)

data TyCon = Unit
           | Bool
           | Int
           | List
           deriving (Read, Show, Eq, Ord)

binApply :: Exp -> Exp -> Exp -> Exp
binApply e1 e2 e3 = App (App e1 e2) e3

binTyApp :: Type -> Type -> Type -> Type
binTyApp t1 t2 t3 = TypeApp (TypeApp t1 t2) t3
