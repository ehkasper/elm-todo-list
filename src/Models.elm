module Models exposing (..)


type alias TodoGrid =
    { lists: List TodoList
    , title: String }

type alias TodoList =
    { newTask: String
    , title : String
    , listId : Int
    , tasks: List Task }

type alias Task =
    { title : String
    , taskId : Int
    , checked : Bool
    }
