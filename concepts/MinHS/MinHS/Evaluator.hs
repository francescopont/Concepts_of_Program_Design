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
          | FunClosure VEnv Exp   -- added the the exp represents the FunClosure ( the FunClosure contains )
          | PartPrimOp Exp  -- ( task 2)
          | PartFunClosure VEnv Exp  --  (task 3 )(we have added other code in other places)
          | LetClosure VEnv Bind  -- (task 5)
          | LetRecClosure VEnv Bind -- (task 6)
          | Err Integer -- to handle errors
          -- added to represent partially evaluated functions ( task 3)
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
evalE gamma (App (App (Con "Cons") (expr1)) expr2)   = case evalE gamma expr1 of
                                                        I n -> (Cons n (evalE gamma expr2))
                                                        _ -> error "A list ca be composed only of integers"
                                  
--PRIMITIVE OPERATIONS 
-- integers operations
evalE gamma (App (App ( Prim Add) (expr1)) (expr2)) = sumV (evalE gamma expr1) (evalE gamma expr2)  -- add  
evalE gamma (App (App ( Prim Sub) (expr1)) (expr2)) = subV (evalE gamma expr1) (evalE gamma expr2) -- sub  
evalE gamma (App (App ( Prim Mul) (expr1)) (expr2)) = mulV (evalE gamma expr1) (evalE gamma expr2)-- mul
evalE gamma (App (App ( Prim Quot) (expr1)) (expr2)) = quotV (evalE gamma expr1) (evalE gamma expr2) -- quot 
evalE gamma (App (App ( Prim Rem) (expr1)) (expr2)) = remV (evalE gamma expr1) (evalE gamma expr2) -- rem
evalE gamma (App ( Prim Neg) (expr1)) = mulV (evalE gamma expr1) (I (-1)) -- neg
evalE gamma (App (App ( Prim Gt) (expr1)) (expr2)) = gtV (evalE gamma expr1) (evalE gamma expr2) --gt
evalE gamma (App (App ( Prim Ge) (expr1)) (expr2)) = geV (evalE gamma expr1) (evalE gamma expr2)   --ge
evalE gamma (App (App ( Prim Lt) (expr1)) (expr2)) = ltV (evalE gamma expr1) (evalE gamma expr2)   --lt
evalE gamma (App (App ( Prim Le) (expr1)) (expr2)) = leV (evalE gamma expr1) (evalE gamma expr2) --le
evalE gamma (App (App ( Prim Eq) (expr1)) (expr2)) = eqV (evalE gamma expr1) (evalE gamma expr2)  --eq
evalE gamma (App (App ( Prim Ne) (expr1)) (expr2)) = neV (evalE gamma expr1) (evalE gamma expr2) --ne
evalE gamma (App (Prim Head) expr) = headV (evalE gamma expr) --head
evalE gamma (App (Prim Tail) expr) = tailV (evalE gamma expr) --tail
evalE gamma (App (Prim Null) expr) = nullV (evalE gamma expr) --null

-- if expression
evalE gamma (If condExpr expr1 expr2) = ifV  gamma (evalE gamma condExpr) expr1 expr2
-- let bindings for variables
evalE gamma (Var varId) = case E.lookup gamma varId of
                          Just (I n) -> (I n)
                          Just (B bool) -> (B bool)
                          Just (Nil) -> (Nil)
                          Just (Cons n val) -> (Cons n val)
                          Just (FunClosure gamma1 expr) -> (FunClosure gamma1 expr)
                          Just (PartPrimOp expr) -> (PartPrimOp expr)
                          Just (PartFunClosure gamma1 expr) -> (PartFunClosure gamma1 expr)
                          Just (LetClosure gamma1 expr) -> (LetClosure gamma1 expr)
                          Just (LetRecClosure gamma1 (Bind varId ty []  varExpr)) -> evalE ( E.add (gamma) (varId, Err 0)) varExpr
                          Just (Err n) -> error "this variable should not be here"
                          Nothing  -> error "Variable not in this environment"
-- variable bindings with let
evalE gamma (Let    [(Bind varId ty [] varExpr)] expr)             =  evalE (E.add gamma (varId ,( evalE gamma varExpr)))  expr 
evalE gamma (Let    ((Bind varId ty [] varExpr):binds) expr)       =  evalE (E.add gamma (varId ,( evalE gamma varExpr))) (Let binds expr) --(task 4: multiple bindings in let)
evalE gamma (Let    [(Bind varId ty varList varExpr)] expr)         =  evalE (E.add gamma (varId ,( LetClosure gamma (Bind varId ty varList varExpr))))  expr -- 
evalE gamma (Let    ((Bind varId ty varList varExpr): binds) expr)  =  evalE (E.add gamma (varId ,( LetClosure gamma (Bind varId ty varList varExpr)))) (Let binds expr) --(task 5:let bindings declare functions)
evalE gamma (Letrec [(Bind varId ty [] varExpr)] expr)              =  evalE (E.add gamma (varId ,( LetRecClosure gamma (Bind varId ty []  varExpr))))  expr -- 
evalE gamma (Letrec ((Bind varId ty [] varExpr): binds) expr)       =  evalE (E.add gamma (varId ,( LetRecClosure gamma (Bind varId ty [] varExpr)))) (Letrec binds expr)

--task2 
evalE gamma (App (Prim Add) expr) = PartPrimOp  (App (Prim Add) expr)--add
evalE gamma (App (Prim Sub) expr) = PartPrimOp  (App (Prim Sub) expr) --sub
evalE gamma (App (Prim Mul) expr) = PartPrimOp  (App (Prim Mul) expr) --mul
evalE gamma (App (Prim Quot) expr) = PartPrimOp  (App (Prim Quot) expr) --quot
evalE gamma (App (Prim Rem) expr) = PartPrimOp  (App (Prim Rem) expr)  --rem
evalE gamma (Prim Neg)  = PartPrimOp  (Prim Neg)  --neg
evalE gamma (App (Prim Gt) expr) = PartPrimOp  (App (Prim Gt) expr) --gt
evalE gamma (App (Prim Ge) expr) = PartPrimOp  (App (Prim Ge) expr) --ge
evalE gamma (App (Prim Lt) expr) = PartPrimOp  (App (Prim Lt) expr) --lt
evalE gamma (App (Prim Le) expr) = PartPrimOp  (App (Prim Le) expr) --le
evalE gamma (App (Prim Eq) expr) = PartPrimOp  (App (Prim Eq) expr) --eq
evalE gamma (App (Prim Ne) expr) = PartPrimOp  (App (Prim Ne) expr) --ne
evalE gamma (Prim Head)  = PartPrimOp  (Prim Head)  --head
evalE gamma (Prim Tail)  = PartPrimOp  (Prim Tail)  --tail
evalE gamma (Prim Null)  = PartPrimOp  (Prim Null)  --null
evalE gamma (App (Con "Cons") (expr1)) = PartPrimOp (App (Con "Cons") (expr1)) --cons



--recursive
evalE gamma (Recfun (Bind funId typ [] funExpr )) = evalE (E.add gamma (funId, (evalE gamma funExpr))) funExpr -- to introduce the infinite list in the big step semantics 
evalE gamma (Recfun (Bind (funId) typ varList funExpr )) = FunClosure gamma (Recfun (Bind (funId) typ varList funExpr )) -- to introduce funClosures

--function application, partial application, etc
evalE gamma (App (expr1) (expr2)) = case evalE gamma expr1 of
                                      PartPrimOp partExpr -> evalE gamma (App partExpr expr2)
                                      FunClosure gamma1 (Recfun(Bind (funId) typ [funVar] funExpr ))               ->          evalE (E.addAll gamma1 [(funVar, (evalE gamma expr2)), (funId, (FunClosure gamma1 (Recfun(Bind (funId) typ [funVar] funExpr )))) ])          funExpr
                                      FunClosure gamma1 (Recfun(Bind (funId) typ  (funVar :funVars) funExpr ))     -> PartFunClosure (E.addAll gamma1 [(funVar, (evalE gamma expr2)), (funId, (FunClosure gamma1 (Recfun(Bind (funId) typ  (funVar :funVars) funExpr )))) ])  (Recfun (Bind (funId) typ funVars funExpr )) 
                                      PartFunClosure gamma1 (Recfun(Bind (funId) typ [funVar] funExpr ))           ->          evalE (E.add gamma1 (funVar, (evalE gamma expr2)))   funExpr  -- task 3                                       
                                      PartFunClosure gamma1 (Recfun(Bind (funId) typ (funVar : funVars) funExpr )) -> PartFunClosure (E.add gamma1 (funVar, (evalE gamma expr2))) (Recfun (Bind (funId) typ funVars funExpr )) -- task 4
                                      LetClosure gamma1 (Bind (funId) typ [funVar] funExpr )                       ->          evalE (E.add gamma1 (funVar, (evalE gamma expr2)))   funExpr -- task 5
                                      LetClosure gamma1 (Bind funId typ (funVar:funVars) funExpr )                 ->     LetClosure (E.add gamma1 (funVar, (evalE gamma expr2))) (Bind (funId) typ funVars funExpr ) --task 5
                                      _ -> error "unknows parameters of an application"



evalE gamma expr = error "implement me! cio ao"

--functions
sumV :: Value -> Value -> Value
sumV (I n1) (I n2) = (I (n1 + n2))

subV :: Value -> Value -> Value
subV (I n1) (I n2) = (I (n1 - n2))

mulV :: Value -> Value -> Value
mulV (I n1) (I n2) = (I (n1 * n2))

quotV :: Value -> Value -> Value
quotV (I n1) ( I 0) = error "Cannot divide by 0"
quotV (I n1) ( I n2) = (I ( quot n1 n2))

remV :: Value -> Value -> Value
remV (I n1) ( I 0) = error "Cannot divide by 0"
remV (I n1) ( I n2) = (I ( rem n1 n2))

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

headV :: Value -> Value
headV Nil = error "List is empty!"
headV (Cons n value) = (I n)

tailV :: Value -> Value
tailV Nil = error "List is empty!"
tailV (Cons n value) = value

nullV :: Value -> Value
nullV Nil = (B True)
nullV (Cons n value) = (B False)

ifV :: VEnv -> Value -> Exp -> Exp -> Value
ifV gamma (B True) expr1 expr2 = evalE gamma expr1
ifV gamma (B False) expr1 expr2 = evalE gamma expr2





