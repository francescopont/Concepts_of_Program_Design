module MinHS.Syntax where

import Data.List

type Id = String

type Program = [Bind]

data Exp
    = Var Id
    | Prim Op
    | Con Id -- con stays for constructor: we have only  True, False, Cons and NIl ( id is a string indeed)
    | Num Integer
    | App Exp Exp
    | If Exp Exp Exp
    | Let [Bind] Exp
    | Recfun Bind
    | Letrec [Bind] Exp -- not for the basic assignment ( do not care now)
    deriving (Read,Show,Eq)

data Bind = Bind Id Type [Id] Exp -- very usefulll
-- il primo id è il nome del bind
-- il secondo id è il nome delle variabili indipendenti ( parametri formali in linguaggio informatico) dentro la funzione
-- il codice della funzione è dentro expr
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
          | TypeApp Type Type -- task 5 ?
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