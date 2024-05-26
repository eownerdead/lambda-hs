module Lambda where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Relude


type Name = Text


data Expr where
  Unit :: Expr
  Var :: Name -> Expr
  Abs :: Name -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  Let :: Name -> Expr -> Expr -> Expr
  deriving (Eq, Show)


data Ty where
  TUnit :: Ty
  TVar :: Name -> Ty
  TFun :: Ty -> Ty -> Ty
  deriving (Eq, Show)


data Scheme where
  Forall :: [Name] -> Ty -> Scheme
  deriving (Eq, Show)


type Env = Map Name Scheme


type Subst = Map Name Ty


type W = State Int


infer :: Expr -> Ty
infer = snd . evaluatingState 0 . w mempty


newVar :: W Ty
newVar = do
  n <- get
  modify (+ 1)
  pure $ TVar ("_" <> show n)


w :: Env -> Expr -> W (Subst, Ty)
w _ Unit = pure (mempty, TUnit)
w env (Var x) = do
  s' <- inst s
  pure (mempty, s')
  where
    Just s = x `Map.lookup` env -- TODO:
w env (Abs x s) = do
  y <- newVar
  (sub, t) <- w (Map.insert x (Forall [] y) env) s
  pure (sub, apply sub (y `TFun` t))
w env (s `App` t) = do
  (sub1, ts) <- w env s
  (sub2, tt) <- w (applyEnv sub1 env) t
  y <- newVar
  let sub3 = unify (apply sub2 ts) (tt `TFun` y)
  pure (sub3 `composeSubst` sub2 `composeSubst` sub1, apply sub3 y)
w env (Let x s t) = do
  (sub1, t1) <- w env s
  let env' = applyEnv sub1 env
  (sub2, t2) <- w (Map.insert x (gen env' t1) env') t
  pure (sub2 `composeSubst` sub1, t2)


inst :: Scheme -> W Ty
inst (Forall vs s) = do
  vs' <- mapM (const newVar) vs
  pure $ apply (Map.fromList $ zip vs vs') s


gen :: Env -> Ty -> Scheme
gen env s = Forall (Set.toList $ ftv s `Set.difference` ftvEnv env) s


unify :: Ty -> Ty -> Subst
unify (TVar x) (TVar y)
  | x == y = mempty
  | otherwise = Map.singleton y (TVar x)
unify (TFun s ss) (TFun t ts) = sub `composeSubst` subs
  where
    sub = unify s t
    subs = unify ss ts
unify (TVar x) y = Map.singleton x y
unify x (TVar y) = Map.singleton y x
unify TUnit TUnit = mempty
unify x y = error $ "Cannot unify " <> show x <> " and " <> show y


composeSubst :: Subst -> Subst -> Subst
sub1 `composeSubst` sub2 = Map.map (apply sub1) sub2 `Map.union` sub1


applyEnv :: Subst -> Env -> Env
applyEnv = Map.map . applyScheme


applyScheme :: Subst -> Scheme -> Scheme
applyScheme sub (Forall vs s) = Forall vs $ apply (foldr Map.delete sub vs) s


apply :: Subst -> Ty -> Ty
apply _ TUnit = TUnit
apply sub (TVar x) = Map.findWithDefault (TVar x) x sub
apply sub (TFun s t) = TFun (apply sub s) (apply sub t)


tsubst :: Name -> Ty -> Ty -> Ty
tsubst _ _ TUnit = TUnit
tsubst old new (TVar x)
  | x == old = new
  | otherwise = TVar x
tsubst old new (TFun s t) = TFun (tsubst old new s) (tsubst old new t)


ftvEnv :: Env -> Set Name
ftvEnv env = foldr (Set.union . ftvScheme) Set.empty (Map.elems env)


ftvScheme :: Scheme -> Set Name
ftvScheme (Forall vs s) = ftv s `Set.difference` Set.fromList vs


ftv :: Ty -> Set Name
ftv TUnit = mempty
ftv (TVar x) = one x
ftv (TFun s t) = ftv s <> ftv t


fresh :: Name -> Expr -> Name
fresh x s
  | x `Set.member` fv s = fresh (x <> "'") s
  | otherwise = x


fv :: Expr -> Set Name
fv Unit = mempty
fv (Var x) = one x
fv (Abs x s) = x `Set.delete` fv s
fv (App s t) = fv s <> fv t
fv (Let x s t) = fv s <> (x `Set.delete` fv t)


swp :: Name -> Name -> Expr -> Expr
swp _ _ Unit = Unit
swp a b (Var x)
  | x == a = Var b
  | x == b = Var a
  | otherwise = Var x
swp a b (Abs x s) = Abs x (swp a b s)
swp a b (App s t) = App (swp a b s) (swp a b t)
swp a b (Let x s t) = Let x (swp a b s) (swp a b t)


step :: Expr -> Expr
step (App (Abs x s) t) = subst x t s
step (App s t)
  | isVal s = App s (step t)
  | otherwise = App (step s) t
step (Let x s t) = subst x s t
step x = x


isVal :: Expr -> Bool
isVal (Var _) = True
isVal _ = False


subst :: Name -> Expr -> Expr -> Expr
subst _ _ Unit = Unit
subst old new (Var x)
  | x == old = new
  | otherwise = Var x
subst old new (Abs x s) = Abs x' $ subst old new (swp x x' s)
  where
    x' = fresh x new
subst old new (App s t) = App (subst old new s) (subst old new t)
subst old new (Let x s t) = Let x' (subst old new s) (subst old new (swp x x' t))
  where
    x' = fresh x new
