module Program

import FreeExample


data Action x
  = Get (String -> x)
  | Put String x
  | Exit


Functor Action where
  map f (Get g) = ?holeFunctor_1
  map f (Put x y) = ?holeFunctor_2
  map f Exit = ?holeFunctor_3


get : Free Action String

puts : String -> Free Action ()

exit : Free Action r

interp : Free Action a -> IO a

runFree : Free Action ()

main : IO ()
main = interp runFree
