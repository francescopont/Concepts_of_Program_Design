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
           | Func Exp String -- added: the string represents the identifier of the variablw which is bounded in the function
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
evaluate bs = evalE E.empty (Let bs (Var "main")) -- what's the purpose of this line? useless for now

-- Implement the interpreter as specified by the operational semantics.
-- Con = constructor
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
evalE gamma (App (App ( Prim Add) (expr1)) (expr2)) = sumV (evalE gamma expr1) (evalE gamma expr2)  -- add  
evalE gamma (App (App ( Prim Sub) (expr1)) (expr2)) = subV (evalE gamma expr1) (evalE gamma expr2) -- sub  
evalE gamma (App (App ( Prim Mul) (expr1)) (expr2)) = mulV (evalE gamma expr1) (evalE gamma expr2)-- mul
evalE gamma (App (App ( Prim Quot) (expr1)) (expr2)) = quotV (evalE gamma expr1) (evalE gamma expr2) -- quot 
evalE gamma (App ( Prim Neg) (expr1)) = mulV (evalE gamma expr1) (I (-1)) -- neg
evalE gamma (App (App ( Prim Gt) (expr1)) (expr2)) = gtV (evalE gamma expr1) (evalE gamma expr2) --gt
evalE gamma (App (App ( Prim Ge) (expr1)) (expr2)) = geV (evalE gamma expr1) (evalE gamma expr2)   --ge
evalE gamma (App (App ( Prim Lt) (expr1)) (expr2)) = ltV (evalE gamma expr1) (evalE gamma expr2)   --lt
evalE gamma (App (App ( Prim Le) (expr1)) (expr2)) = leV (evalE gamma expr1) (evalE gamma expr2) --le
evalE gamma (App (App ( Prim Eq) (expr1)) (expr2)) = eqV (evalE gamma expr1) (evalE gamma expr2) -- quot  --eq
evalE gamma (App (App ( Prim Ne) (expr1)) (expr2)) = neV (evalE gamma expr1) (evalE gamma expr2) -- quot  --ne
-- head operator
evalE gamma (App (Prim Head) (Con "Nil")) = error "List is empty!"
evalE gamma ( App (Prim Head) (App (App (Con "Cons")(Num n)) expr ))= evalE gamma ( Num n) -- the first value
--tail operator
evalE gamma (App(Prim Tail) (Con "Nil")) = error "List is empty!"
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) (Con "Nil") )   ) = (Cons n Nil) -- base case: we are already at the tail
evalE gamma ( App (Prim Tail) (App (App (Con "Cons")(Num n)) expr )   ) = (evalE gamma expr) -- induction case: we are not at the tail
--null operator
evalE gamma (App (Prim Null) expr) = nullV (evalE gamma expr)
-- if expression
evalE gamma (If condExpr expr1 expr2) = ifV  gamma (evalE gamma condExpr) expr1 expr2
-- let bindings for variables
evalE gamma (Let [Bind (varId) (TypeCon x) [] varExpr] expr) =  evalE (E.add (gamma) (varId ,( evalE gamma varExpr))) expr
evalE gamma (Var varId) = case E.lookup gamma varId of
                          Just (I n) -> (I n)
                          Just (B bool) -> (B bool)
                          Just Nil -> Nil
                          Just (Cons n val) -> (Cons n val)
                          Nothing  -> error "variable is free in this environment"
                          _ -> error "The environment is not able to provide something"
--- let bindings for functions ( to test ABSOLUTELY)
--START FROM HERE TOMORROW
evalE gamma (Let [Bind (funId1) (Arrow domain range) [] (Recfun (Bind (funId2) (Arrow dom1 range1) [funVar] funExpr ))] expr) = evalE ( E.add (gamma) (funId1, (Func funExpr funVar)))  expr
evalE gamma ( App ( Var funId) (actualParam)) = case E.lookup gamma funId of
                                        Just (Func funExpr funVar) -> evalE (E.add (gamma)( funVar,(evalE gamma actualParam))) funExpr
                                        _ -> error "Your programming is so bad man"
evalE gamma (App (Recfun (Bind (funId) (Arrow dom1 range1) [funVar] funExpr )) actualParam ) = evalE (E.add (gamma)( funVar,(evalE gamma actualParam))) funExpr
evalE gamma exp = error "Implement me!"


sumV :: Value -> Value -> Value
sumV (I n1) (I n2) = (I (n1 + n2))

subV :: Value -> Value -> Value
subV (I n1) (I n2) = (I (n1 - n2))

mulV :: Value -> Value -> Value
mulV (I n1) (I n2) = (I (n1 * n2))

quotV :: Value -> Value -> Value
quotV (I n1) ( I 0) = error "Cannot divide by 0"
quotV (I n1) ( I n2) = (I ( quot n1 n2))

gtV :: Value -> Value -> Value
gtV (I n1) (I n2) = (B ( n1 > n2))

geV :: Value -> Value -> Value
geV (I n1) (I n2) = (B ( n1 >= n2))

ltV :: Value -> Value -> Value
ltV (I n1) (I n2) = (B ( n1 < n2))

leV :: Value -> Value -> Value
leV (I n1) (I n2) = (B ( n1 <= n2))

eqV :: Value -> Value -> Value
eqV (I n1) (I n2) = (B ( n1 == n2))

neV :: Value -> Value -> Value
neV (I n1) (I n2) = (B ( n1 /= n2))

nullV :: Value -> Value
nullV Nil = (B True)
nullV (Cons n val) = (B False)

ifV :: VEnv -> Value -> Exp -> Exp -> Value
ifV gamma (B True) expr1 expr2 = evalE gamma expr1
ifV gamma (B False) expr1 expr2 = evalE gamma expr2

-- still useful
iffound :: Bool -> Value -> Value
iffound True  returnValue = returnValue
iffound False _ = error "the variable is free in this context"

-- MISSING PARTS
--recfun







--integers operations
--evalE gamma (App (App ( Prim Add) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  + (evalI gamma expr2)))-- add  
--evalE gamma (App (App ( Prim Sub) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  - (evalI gamma expr2))) -- sub  
--evalE gamma (App (App ( Prim Mul) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  * (evalI gamma expr2)))-- mul
--evalE gamma (App (App ( Prim Quot) (expr1)) (Num 0)) = error "Cannot divide by 0!" -- complex separated code to handle division by 0
--evalE gamma (App (App ( Prim Quot) (expr1)) (Num n)) = (I ( quot (evalI gamma expr1) n ))-- quot
--evalE gamma (App (App ( Prim Quot) (expr1)) (expr2)) = evalE gamma (App (App ( Prim Quot) (expr1)) (Num (evalI gamma expr2))) -- quot 
--evalE gamma (App (App ( Prim Rem) (expr1)) (expr2)) = (I ( (evalI gamma expr1)  % (evalI gamma expr2))) -- rem
--booleans operations
--evalE gamma (App (App ( Prim Gt) (expr1)) (expr2)) = (B ( (evalI gamma expr1) > (evalI gamma expr2))) --gt
--evalE gamma (App (App ( Prim Ge) (expr1)) (expr2)) = (B ( (evalI gamma expr1) >= (evalI gamma expr2))) --ge
--evalE gamma (App (App ( Prim Lt) (expr1)) (expr2)) = (B ( (evalI gamma expr1) < (evalI gamma expr2))) --lt
--evalE gamma (App (App ( Prim Le) (expr1)) (expr2)) = (B ( (evalI gamma expr1) <= (evalI gamma expr2))) --le
--evalE gamma (App (App ( Prim Eq) (expr1)) (expr2)) = (B ( (evalI gamma expr1) == (evalI gamma expr2))) --eq
--evalE gamma (App (App ( Prim Ne) (expr1)) (expr2)) = (B ( (evalI gamma expr1) /= (evalI gamma expr2))) --ne

-- to evaluate into Integers
--evalI :: VEnv -> Exp -> Integer
-- case base
--evalI gamma (Num n)= n
--evalI gamma (Var id) = E.lookup gamma id
--primitive operations
--evalI gamma (App (App ( Prim Add) (expr1)) (expr2))  = ( (evalI gamma expr1)  + (evalI gamma expr2)) -- add
--evalI gamma (App (App ( Prim Sub) (expr1)) (expr2))  = ( (evalI gamma expr1)  - (evalI gamma expr2)) -- sub
--evalI gamma (App (App ( Prim Mul) (expr1)) (expr2))  = ( (evalI gamma expr1)  * (evalI gamma expr2)) -- mul
--evalI gamma (App (App ( Prim Quot) (expr1)) (Num 0)) = error "Cannot divide by 0" -- quot
--evalI gamma (App (App ( Prim Quot) (expr1)) (Num n)) = (quot (evalI gamma expr1) n )-- quot
--evalI gamma (App (App ( Prim Quot) (expr1)) (expr2)) = evalI gamma (App (App ( Prim Quot) (expr1)) (Num (evalI gamma expr2))) -- quot --evalI gamma (App (App ( Prim Rem) (expr1)) (expr2)) = ( (evalI gamma expr1)  % (evalI gamma expr1)) -- rem
--evalI gamma (App ( Prim Neg) (expr1)) = ( (evalI gamma expr1)  * (-1)) -- neg


-- to evaluate into Booleans for the if-then-else clause
--evalB :: VEnv -> Exp -> Bool
--case base
--evalB gamma (Con "True") = True
--evalB gamma (Con "False") = False
-- primitive operations
--evalB gamma (App (App ( Prim Gt) (expr1)) (expr2)) = ( (evalI gamma expr1) >  (evalI gamma expr2)) --gt
--evalB gamma (App (App ( Prim Ge) (expr1)) (expr2)) = ( (evalI gamma expr1) >= (evalI gamma expr2)) --ge
--evalB gamma (App (App ( Prim Lt) (expr1)) (expr2)) = ( (evalI gamma expr1) <  (evalI gamma expr2)) --lt
--evalB gamma (App (App ( Prim Le) (expr1)) (expr2)) = ( (evalI gamma expr1) <= (evalI gamma expr2)) --le
--evalB gamma (App (App ( Prim Eq) (expr1)) (expr2)) = ( (evalI gamma expr1) == (evalI gamma expr2)) --eq
--evalB gamma (App (App ( Prim Ne) (expr1)) (expr2)) = ( (evalI gamma expr1) /= (evalI gamma expr2)) --ne


