module FreeAp

import Free
import FreeApplicative


record User where
  constructor MkUser
  name : String
  age : Int


data UserOperation x
  = CreateUser (String -> Int -> x)
  | GetUserByID (String -> x)
  | ListUsers x


-- data ExecStrategy (fa : Type -> Type) where
--   seq : Free UserOperation fa
--   par : FreeAp UserOperation fa

-- createUser : String -> Int -> User
-- getUserById : String -> Maybe User
-- listUsers : List User

Functor UserOperation where
  map f (CreateUser g) = CreateUser ?holeFunctor_1
  map f (GetUserByID g) = GetUserByID $ f . g
  map f (ListUsers x) = ListUsers $ f x


createUser : String -> Int -> Free UserOperation User
createUser name age = liftFree $ CreateUser ?createUser_rhs


getUserById : String -> Free UserOperation (Maybe User)
getUserById user_id = liftFree $ GetUserByID ?getUserById_rhs


listUsers : Free UserOperation (List User)
listUsers = liftFree $ ListUsers ?listUsers_rhs
