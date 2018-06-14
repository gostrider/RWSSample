{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Control.Monad.Free


data Action x
  = Get (String -> x)
  | Put [String] x
  | Exit
  deriving Functor
  -- | Show String x


get :: Free Action String
get = liftF $ Get id


gets :: Free Action [String]
gets = do
  x <- get
  if x == "END"
    then return []
    else gets >>= return . (:) x


puts :: [String] -> Free Action ()
puts x = liftF $ Put x ()


-- printIt :: String -> Free Action ()
-- printIt x = liftF $ Show x ()


exit :: Free Action r
exit = liftF Exit


evalWith :: Free Action a -> IO a
evalWith (Pure x) = return x
evalWith (Free fm) =
  case fm of
    Put s t  -> putStrLn (show s) >> evalWith t
    Get f    -> getLine >>= evalWith . f
    Exit     -> exitSuccess
    -- Show x t -> putStrLn x >> evalWith t


program :: Free Action ()
program = do
  puts ["Start"]
  content <- gets
  puts ["Content is"]
  puts content
  exit


main :: IO ()
main = evalWith program
