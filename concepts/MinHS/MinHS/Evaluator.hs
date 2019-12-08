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
--add operator
evalE gamma (App (App ( Prim Add) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  + (evalI gamma expr2)) -- subcall  
-- sub operation
evalE gamma (App (App ( Prim Sub) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  - (evalI gamma expr2))) -- subcall  
-- mul operation
evalE gamma (App (App ( Prim Mul) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  * (evalI gamma expr2)))-- subcall  
--quot operation
evalE gamma (App (App ( Prim Quot) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  'div' (evalI gamma expr2))) -- subcall  
--rem operation
--evalE gamma (App (App ( Prim Rem) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  % (evalI gamma expr2))) -- subcall  
--neg operation
evalE gamma (App ( Prim Neg) (expr1)) = (I ( (evalI gamma expr1)  * (-1))) -- base case 

-- head operator
evalE gamma ( App (Prim Head) (App (App (Con "Cons")(Num n)) expr )   ) = (I n) -- we have only one case
--tail operator
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) (Con "Nil") )   ) = (Cons n Nil) -- base case: we are already at the tail
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) expr )   ) = (evalE gamma expr) -- induction case: we are not at the tail
--

evalE gamma exp = error "Implement me!"
-- MISSING PARTS
-- ERROR HANDLING FOR HEAD AND TAIL
-- error handling for quotient


-- to evaluate into Integers
evalI :: VEnv -> Exp -> Integer
--numbers
evalI gamma (Num n)= n
--add operator
evalI gamma (App (App ( Prim Add) (expr1)) (expr2)) = ( (evalI gamma expr1)  + (evalI gamma expr1)) -- subcall  
-- sub operation
evalI gamma (App (App ( Prim Sub) (expr1)) (expr2)) = ( (evalI gamma expr1)  - (evalI gamma expr1)) -- subcall  
-- mul operation
evalI gamma (App (App ( Prim Mul) (expr1)) (expr2)) = ( (evalI gamma expr1)  * (evalI gamma expr1)) -- subcall  
--quot operation
evalI gamma (App (App ( Prim Quot) (expr1)) (expr2)) = ( (evalI (gamma) (expr1))  'div' (evalI (gamma) (expr1))) -- subcall  
--rem operation
--evalI gamma (App (App ( Prim Rem) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  % (evalI gamma expr1))) -- subcall  
--neg operation
evalI gamma (App ( Prim Neg) (expr1)) = ( (evalI gamma expr1)  * (-1)) -- base case 
