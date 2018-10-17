module FreeProgram

import Free

record User where
  constructor MkUser
  name : String
  age : Int


record Store where
  constructor MkStore
  items : List User


data UserOperation x
  = CreateUser (String -> Int -> x)
  | GetUserByID (String -> x)
  | ListUsers Store x


-- Program f a = FreeAp (Free f) a
-- Program f a = Free (FreeAp f) a


-- createUser : String -> Int -> User
-- getUserById : String -> Maybe User
-- listUsers : List User


Functor UserOperation where
  map f (CreateUser g) = CreateUser ?g_rhs
  map f (GetUserByID g) = GetUserByID $ f . g
  map f (ListUsers x y) = ListUsers x $ f y


empty_store : Store
empty_store = MkStore []


add : Store -> User -> Store
add (MkStore items) user = MkStore $ addItem items
  where
    addItem : List User -> List User
    addItem [] = user :: []
    addItem users = user :: users


findUser : Store -> String -> Maybe User
findUser (MkStore []) _ = Nothing
findUser (MkStore (u :: us)) user_id =
  if name u == user_id then Just u else Nothing


show_users : Store -> List User
show_users store = items store


createUser : Store -> String -> Int -> Free UserOperation Store
-- createUser store name age = liftFree $ CreateUser add


getUserById : Store -> String -> Free UserOperation (Maybe User)
-- getUserById store user_id = liftFree $ GetUserByID $ findUser store user_id


listUsers : Store -> Free UserOperation ()
listUsers store = liftFree $ ListUsers store ()


userOps : Store -> Free UserOperation ()
userOps store = do
--   store'    <- createUser store "user1" 20
--   -- one_user  <- getUserById store "user1"
  listUsers store
  pure ()


run : Free UserOperation a -> IO a
run (Pure x) = pure x
run (Bind x) = case x of
  (CreateUser f) => ?run_rhs_1
  (GetUserByID f) => ?run_rhs_2
  (ListUsers users z) => do putStrLn ""
                            run z


all_users : IO ()
all_users = run $ userOps empty_store
