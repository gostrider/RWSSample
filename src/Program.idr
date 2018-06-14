module Program

import System as Sys
import Free


data Action x
  = Get (String -> x)
  | Put (List String) x
  -- | Show String x
  | Exit


Functor Action where
  map f (Get g)    = Get $ f . g
  map f (Put x y)  = Put x $ f y
  -- map f (Show x y) = Show x $ f y
  map f Exit       = Exit


get : Free Action String
get = liftFree $ Get id


gets : Free Action (List String)
gets = do
  x <- get
  if x == "END"
    then pure []
    else gets >>= pure . (::) x


puts : List String -> Free Action ()
puts x = liftFree $ Put x ()


-- shows : String -> Free Action ()
-- shows x = liftFree $ Show x ()


exit : Free Action r
exit = liftFree Exit


evalWith : Free Action a -> IO a
evalWith (Pure x)  = pure x
evalWith (Bind fm) =
  case fm of
    (Get f)    => getLine >>= evalWith . f
    -- (Show s t) => do putStrLn s
    --                  evalWith t
    (Put s t)  => do putStrLn (show s)
                     evalWith t
    Exit       => Sys.exitSuccess


program : Free Action ()
program = do
  puts ["Start"]
  content <- gets
  puts ["Content is:"]
  puts content
  exit


-- Almost there...
-- https://github.com/idris-lang/Idris-dev/issues/3752
main : IO ()
main = evalWith program
