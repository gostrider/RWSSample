module FreeApplicative

import Free

-- public export
-- data FreeAp : (f : Type -> Type) -> (a : Type) -> Type where
--   Pure :   a -> FreeAp f a
--   Ap   : f a -> FreeAp f (a -> b) -> FreeAp f b
--
--
-- public export
-- Functor f => Functor (FreeAp f) where
--   map f (Pure x) = Pure $ f x
--   map f (Ap x g) = Ap x $ (f .) <$> g
--
--
-- public export
-- Functor f => Applicative (FreeAp f) where
--   pure    = Pure
--   m <*> f = assert_total $ case m of
--     Pure x => map x f
--     Ap x g => Ap x $ flip <$> g <*> f
--
--
-- liftAp : Functor f => f a -> FreeAp f a
-- liftAp x = Ap x $ Pure id


record Const (a : Type) (b : Type) where
  constructor MkConst
  getConst : a


runAp_ : Semigroup m => ({a : Type} -> f a -> m) -> Free f b -> m
-- runAp_ u = getConst . lowerFree $ MkConst . u


record App (f : Type -> Type) (a : Type) where
  constructor MkApp
  runApp : f a


var : String -> App (Free (Either String)) Integer
var = MkApp . liftFree . Left


vars : App (Free (Either String)) Integer -> List String
-- vars =
