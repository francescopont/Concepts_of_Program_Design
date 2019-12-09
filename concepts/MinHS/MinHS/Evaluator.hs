module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Add other variants as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))

-- Implement the interpreter as specified by the operational semantics.
evalE :: VEnv -> Exp -> Value
--numbers
evalE gamma (Num n)= (I n)
-- booleans
evalE gamma (Con "True") = (B True)
evalE gamma (Con "False") = (B False)
--lists
evalE gamma (Con "Nil") = Nil
evalE gamma (App (App (Con "Cons") (Num n)) expr)   = (Cons n (evalE gamma expr))
--PRIMITIVE OPERATIONS
-- integers operations
evalE gamma (App (App ( Prim Add) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  + (evalI gamma expr2))) -- add  
evalE gamma (App (App ( Prim Sub) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  - (evalI gamma expr2))) -- sub  
evalE gamma (App (App ( Prim Mul) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  * (evalI gamma expr2)))-- mul
evalE gamma (App (App ( Prim Quot) (expr1)) (Num 0)) = error "Cannot divide by 0!" -- complex separated code to handle division by 0
evalE gamma (App (App ( Prim Quot) (expr1)) (Num n)) = (I ( quot (evalI gamma expr1) n ))-- quot
evalE gamma (App (App ( Prim Quot) (expr1)) (expr2)) = evalE gamma (App (App ( Prim Quot) (expr1)) (Num (evalI gamma expr2))) -- quot 
--evalE gamma (App (App ( Prim Rem) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  % (evalI gamma expr2))) -- rem
evalE gamma (App ( Prim Neg) (expr1)) = (I ( (evalI gamma expr1)  * (-1))) -- neg
--booleans operations
evalE gamma (App (App ( Prim Gt) (expr1)) (expr2)) = (B ( (evalI gamma expr1) > (evalI gamma expr2))) --gt
evalE gamma (App (App ( Prim Ge) (expr1)) (expr2)) = (B ( (evalI gamma expr1) >= (evalI gamma expr2))) --ge
evalE gamma (App (App ( Prim Lt) (expr1)) (expr2)) = (B ( (evalI gamma expr1) < (evalI gamma expr2))) --lt
evalE gamma (App (App ( Prim Le) (expr1)) (expr2)) = (B ( (evalI gamma expr1) <= (evalI gamma expr2))) --le
evalE gamma (App (App ( Prim Eq) (expr1)) (expr2)) = (B ( (evalI gamma expr1) == (evalI gamma expr2))) --eq
evalE gamma (App (App ( Prim Ne) (expr1)) (expr2)) = (B ( (evalI gamma expr1) /= (evalI gamma expr2))) --ne


-- head operator
evalE gamma ( App (Prim Head) (App (App (Con "Cons")(Num n)) expr )   ) = (I n) -- we have only one case
evalE gamma (App (Prim Head) (Con "Nil")) = error "List is empty!"
--tail operator
evalE gamma (App(Prim Tail) (Con "Nil")) = error "List is empty!"
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) (Con "Nil") )   ) = (Cons n Nil) -- base case: we are already at the tail
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) expr )   ) = (evalE gamma expr) -- induction case: we are not at the tail
--null operator
evalE gamma (App (Prim Null) (Con "Nil")) = (B True)
evalE gamma (App (Prim Null) expr) = (B False)

evalE gamma exp = error "Implement me!"
-- MISSING PARTS
-- error handling for quotient ( to test)


-- to evaluate into Integers
evalI :: VEnv -> Exp -> Integer
-- case base
evalI gamma (Num n)= n
--primitive operations
evalI gamma (App (App ( Prim Add) (expr1)) (expr2))  = ( (evalI gamma expr1)  + (evalI gamma expr2)) -- add
evalI gamma (App (App ( Prim Sub) (expr1)) (expr2))  = ( (evalI gamma expr1)  - (evalI gamma expr2)) -- sub
evalI gamma (App (App ( Prim Mul) (expr1)) (expr2))  = ( (evalI gamma expr1)  * (evalI gamma expr2)) -- mul
evalI gamma (App (App ( Prim Quot) (expr1)) (Num 0)) = error "Cannot divide by 0" -- quot
evalI gamma (App (App ( Prim Quot) (expr1)) (Num n)) = (quot (evalI gamma expr1) n )-- quot
evalI gamma (App (App ( Prim Quot) (expr1)) (expr2)) = evalI gamma (App (App ( Prim Quot) (expr1)) (Num (evalI gamma expr2))) -- quot --evalI gamma (App (App ( Prim Rem) (expr1)) (expr2)) = ( (evalI gamma expr1)  % (evalI gamma expr1)) -- rem
evalI gamma (App ( Prim Neg) (expr1)) = ( (evalI gamma expr1)  * (-1)) -- neg



-- to evaluate into Booleans (STILL USEFUL??)
evalB :: VEnv -> Exp -> Bool
--case base
evalB gamma (Con "True") = True
evalB gamma (Con "False") = False
-- primitive operations

evalB gamma (App (App ( Prim Gt) (expr1)) (expr2)) = ( (evalB gamma expr1) >  (evalB gamma expr2)) --gt
evalB gamma (App (App ( Prim Ge) (expr1)) (expr2)) = ( (evalB gamma expr1) >= (evalB gamma expr2)) --ge
evalB gamma (App (App ( Prim Lt) (expr1)) (expr2)) = ( (evalB gamma expr1) <  (evalB gamma expr2)) --lt
evalB gamma (App (App ( Prim Le) (expr1)) (expr2)) = ( (evalB gamma expr1) <= (evalB gamma expr2)) --le
evalB gamma (App (App ( Prim Eq) (expr1)) (expr2)) = ( (evalB gamma expr1) == (evalB gamma expr2)) --eq
evalB gamma (App (App ( Prim Ne) (expr1)) (expr2)) = ( (evalB gamma expr1) /= (evalB gamma expr2)) --ne


