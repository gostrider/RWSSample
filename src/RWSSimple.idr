module RWSTest

import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

execRWST : RWS r w s a -> r -> s -> (s, w)
execRWST rwst env state =
  let (s, r, l) = runIdentity (runRWST rwst env state) in (r, l)


evalRWST : RWS r w s a -> r -> s -> (a, w)
evalRWST rwst env state =
  let (s, r, l) = runIdentity (runRWST rwst env state) in (s, l)


computation : RWS Double (List Double) Double Double
computation = do
  s <- get
  tell [s]
  e <- ask
  tell [e]
  let b = e + s
  put b
  pure b
  -- tell [b]
