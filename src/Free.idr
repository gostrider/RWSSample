module Free

%default total

public export
data Free : (f : Type -> Type) -> (a : Type) -> Type where
  Pure : a -> Free f a
  Bind : f (Free f a) -> Free f a


mutual
  export
  Functor f => Functor (Free f) where
    map f m = assert_total $ case m of
      Pure x => Pure $ f x
      Bind x => Bind $ map (map f) x

  export
  Functor f => Applicative (Free f) where
    pure    = Pure

    m <*> x = assert_total $ case m of
      Pure f => map f x
      Bind f => Bind $ map (<*> x) f

  export
  Functor f => Monad (Free f) where
    m >>= f = assert_total $ case m of
      Pure x => f x
      Bind x => Bind $ map (>>= f) x


joinFree : Functor f => Free f (Free f a) -> Free f a
joinFree m = assert_total $ case m of
  Pure x => x
  Bind x => Bind $ map joinFree x


export
liftFree : Functor f => f a -> Free f a
liftFree = assert_total $ Bind . map Pure


export
lowerFree : Monad f => Free f a -> f a
lowerFree m = assert_total $ case m of
  Pure x => pure x
  Bind f => join (map lowerFree f)


export
hoistFree : Functor g => ({a : Type} -> f a -> g a) -> Free f b -> Free g b
hoistFree f m = assert_total $ case m of
  Pure x => Pure x
  Bind x => Bind $ hoistFree f <$> f x


interface FreeM (m : Type -> Type) (f : Type -> Type) | m where
  wrap : f (m a) -> m a


FreeM (Free f) f where
  wrap = assert_total Bind
