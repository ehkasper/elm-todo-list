module Messages exposing (..)

type Msg
  = AddTodoList
  | HandleCreateTodoListTitle String
  | HandleCreateTaskTitle Int String
  | AddTask Int
  | CheckTask Int Bool
  | RemoveTask Int
