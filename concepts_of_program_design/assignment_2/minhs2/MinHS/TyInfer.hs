module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))

primOpType :: Op -> QType
primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = Ty $ Base Int `Arrow` Base Int
primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int) -- this is for add and stuff, they all have the same type

constType :: Id -> Maybe QType
constType "True"  = Just $ Ty $ Base Bool
constType "False" = Just $ Ty $ Base Bool
constType "()"    = Just $ Ty $ Base Unit
constType "Pair"  = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
constType "Inl"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType "Inr"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType _       = Nothing

--Inr and Inl are the constructors of the sum tpe
-- () and Pair are the constructors of the Prod type
--True and False are the constructors of the sBool type
type Gamma = E.Env QType

initialGamma :: Gamma
initialGamma = E.empty

tv :: Type -> [Id]   -- tv sono le free variables: a che cosa possono servire??? boh boh
tv = tv'
 where
   tv' (TypeVar x) = [x]
   tv' (Prod  a b) = tv a `union` tv b -- usando queste apici si può usare in forma infissa, molto più leggibile
   tv' (Sum   a b) = tv a `union` tv b
   tv' (Arrow a b) = tv a `union` tv b
   tv' (Base c   ) = []

tvQ :: QType -> [Id]
tvQ (Forall x t) = filter (/= x) $ tvQ t
tvQ (Ty t) = tv t

tvGamma :: Gamma -> [Id]
tvGamma = nub . foldMap tvQ

infer :: Program -> Either TypeError Program
infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
                   return p'


-- remove the quantifiers from a polymrphic type and rename all quantified variables
-- with fresh names.
unquantify :: QType -> TC Type
{-
Normally this implementation would be possible:

unquantify (Ty t) = return t
unquantify (Forall x t) = do x' <- fresh
                             unquantify (substQType (x =:x') t)

However as our "fresh" names are not checked for collisions with names bound in the type
we avoid capture entirely by first replacing each bound
variable with a guaranteed non-colliding variable with a numeric name,
and then substituting those numeric names for our normal fresh variables
-}

unquantify = unquantify' 0 emptySubst -- anche qua mantengo non specificato il parametro e faccio pattern matching
unquantify' :: Int -> Subst -> QType -> TC Type
unquantify' i s (Ty t) = return $ substitute s t
unquantify' i s (Forall x t) = do x' <- fresh
                                  unquantify' (i + 1)
                                              ((show i =: x') <> s)
                                              (substQType (x =:TypeVar (show i)) t)



-- Implement the functions below
-- =============================

unify :: Type -> Type -> TC Subst
unify = error "implement me"


--
generalise :: Gamma -> Type -> QType
generalise g t = quantify (filter ( notpresentIn (tvGamma g)) (tv t)) t -- check gamma now
generalise _ _ = error "implement me better, Mario"

quantify :: [Id] -> Type ->Qtype
quantify [] t = Ty t
quantify x:xs t = quantify'  xs (Forall x (Ty t))

quantify' :: [Id] -> QType ->Qtype
quantify' [] t =  t
quantify' x:xs t = quantify'  xs (Forall x t)


notPresentIn :: Id -> [Id] -> Bool
notPresentIn tauString [] = True
notPresentIn tauString x:xs = case (tauString /= x)of 
                           True -> notPresentIn tauString xs
                           False -> False


inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
-- fix the implementation - it is not correct as it is and will work
-- only for a some cases. The code is just an example on how to use the TC type,
-- and how to add the type information to the expression.
inferProgram env [Bind m _ [] exp]
  = do
      (typedExp, t, subst) <- inferExp env exp
      return ([Bind m  (Just $ Ty t)  []  typedExp], t, subst) -- ricordati che return fa una cosa specifica nelle monadi


-- no need to infer for integers ???
inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
-- constructor type (it was already here)
inferExp g exp@(Con id) = do
  let Just qt = constType id
  t <- unquantify qt
  return (exp, t, emptySubst)

---- prim op type (basically the same)
inferExp g exp@(Prim op) = do
  let qt = primOpType op
  t <- unquantify qt
  return (exp, t, emptySubst)

---- integer base type
inferExp g exp@(Num n) = 
  return (exp,Base Int, emptySubst)

inferExp g expr@(Var varId) = case E.lookup g varId of
                              Just qt -> do 
                                          t <- unquantify qt
                                          return (expr,t, emptySubst)
                              Nothing -> error "this case has not been handled, according to the specs"

inferExp g expr@(App expr1 expr2) =  do (annotatedExpr1, ty1, subst1) <- inferExp g expr1
                                        (annotatedExpr2, ty2, subst2) <- inferExp (substGamma subst1 g)   expr2
                                        (alpha) <- fresh
                                        (finalSub) <- unify (substitute subst2 ty1) (Base Int `Arrow` alpha)
                                        return (expr, (substitute finalSub alpha), (subst1 <> subst2 <> finalSub))

inferExp g expr@(If expr1 expr2 expr3) =  do  (annotatedExpr1, ty1, subst1) <- inferExp g expr1
                                              (substBool) <- unify  ty1 (Base Bool)
                                              (annotatedExpr2, ty2, subst2) <- inferExp (substGamma substBool (substGamma subst1 g))   expr2
                                              (annotatedExpr3, ty3, subst3) <- inferExp (substGamma subst2(substGamma substBool (substGamma subst1 g)))   expr3
                                              (finalSub) <- unify (substitute subst3 ty2) (ty3)
                                              return (expr, (substitute finalSub ty3), (subst1 <> substBool <> subst2 <> subst3 <> finalSub))


-- implement me!!!!
--inferExp g expr@(Let ((Bind varId ty [] varExpr):bs) x)  =  let TC (annotatedExpr1, ty1, subst1) = inferExp g varExpr
inferExp g _ = error "Implement me!"  
-- implement all the missing cases
-- -- Note:  the only case you need to handle for case expressions is:
-- inferExp g (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2])
-- for all others, raise an error:
-- inferExp g (Case e _) = typeError MalformedAlternatives




