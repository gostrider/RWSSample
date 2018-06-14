module Free


public export
data Free : (f : Type -> Type) -> (a : Type) -> Type where
  Pure : a -> Free f a
  Bind : f (Free f a) -> Free f a


mutual
  export
  Functor f => Functor (Free f) where
    map f (Pure x) = Pure $ f x
    map f (Bind x) = Bind $ map (map f) x


  Functor f => Applicative (Free f) where
    pure           = Pure
    (Pure f) <*> x = map f x
    (Bind f) <*> x = Bind $ map (<*> x) f


  export
  Functor f => Monad (Free f) where
    (Pure x) >>= f = f x
    (Bind x) >>= f = Bind $ map (>>= f) x


joinFree : Functor f => Free f (Free f a) -> Free f a
joinFree (Pure x) = x
joinFree (Bind x) = Bind $ map joinFree x


export
liftFree : Functor f => f a -> Free f a
liftFree = Bind . map Pure


interface FreeM (m : Type -> Type) (f : Type -> Type) | m where
  wrap : f (m a) -> m a


FreeM (Free f) f where
  wrap = Bind
