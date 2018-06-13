module RWSExample

import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Fuel


data BlogOps
  = Add | Del | Display

data Content
  = Article String

data Blog
  = RWS Content (List Content) Content ()

-- RWS : reader -> writer -> state -> effect
-- computation : RWS Content (List Content) Content ()
-- computation = do
--   e <- ask
--   tell [e]
--   s <- get
--   tell [s]
--   b <- pure $ divison e s
--   put b
--   tell [b]


-- example : Identity ((), Double, List Double)
-- example = runRWST computation

-- runBlog : (blg : Blog) ->

command : String -> Maybe BlogOps
command "" = Nothing
command "add" = Just Add
command "del" = Just Del
command "display" = Just Display


run : Fuel -> List Content -> Blog -> IO ()
run Dry initContent blg = pure ()
run (More f) initContent blg = do
  runRWST ?rwst_computation initContent ?initState
  run f ?content ?result


main : IO ()
main = run forever [] ?next
