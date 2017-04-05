module Main exposing (..)

import Models exposing (TodoGrid, TodoList, Task)
import Messages exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

model : TodoGrid
model =
  { lists = []
  , title = "" }

initTodoList : String -> Int -> TodoList
initTodoList title listId =
    { title = title
    , newTask = ""
    , listId = listId
    , tasks = [] }

initTask: String -> Int -> Task
initTask title taskId =
  { title = title
  , checked = False
  , taskId = taskId
  }

-- update
update : Msg -> TodoGrid -> TodoGrid
update msg model =
  case msg of
    HandleCreateTodoListTitle title ->
      { model | title = title }

    AddTodoList ->
      let
        getLastTodoList = List.head (List.reverse model.lists)
        newId =
          case getLastTodoList of
            Just todoList -> todoList.listId + 1
            Nothing -> 1
      in
        { model | lists = model.lists ++ [initTodoList model.title newId]
                , title = "" }

    HandleCreateTaskTitle listId newTask ->
      let
        handleCrateTaskTitle todoList =
          if todoList.listId == listId then
            { todoList | newTask = newTask }
          else
            todoList
      in
        { model | lists = List.map handleCrateTaskTitle model.lists }

    AddTask listId ->
      let
        maximumIdInTasks: List TodoList -> Maybe Int
        maximumIdInTasks todoLists =
          List.maximum (allTasksIds todoLists)
 
        allTasksIds : List TodoList -> List Int
        allTasksIds todoLists =
          List.foldl (\todoList allTasks -> List.append (List.map (\x -> x.taskId) todoList.tasks) allTasks) [] todoLists

        nextId : List TodoList -> Int
        nextId todoLists =
          case maximumIdInTasks todoLists of
            Nothing -> 1
            Just currentId -> currentId + 1

        handleAddTask : TodoList -> TodoList
        handleAddTask todoList =
          if todoList.listId == listId then
            { todoList | tasks = todoList.tasks ++ [initTask todoList.newTask (nextId model.lists)]
                       , newTask = "" }
          else
            todoList
      in
        { model | lists = List.map handleAddTask model.lists }

    CheckTask taskId checked ->
      let
        handleCheckTask : Task -> Task
        handleCheckTask task =
          if task.taskId == taskId then
            { task | checked = checked }
          else
            task

        handleCheckTodoList : TodoList -> TodoList
        handleCheckTodoList todoList =
          { todoList | tasks = List.map handleCheckTask todoList.tasks }
      in
        { model | lists = List.map handleCheckTodoList model.lists }

    RemoveTask taskId ->
      let
        isTask : Task -> Bool
        isTask task =
          task.taskId == taskId

        handleFilterTodoList : TodoList -> TodoList
        handleFilterTodoList todoList =
          { todoList | tasks = List.filter (\task -> not (isTask task)) todoList.tasks }
      in
        { model | lists = List.map handleFilterTodoList model.lists }

-- view
view : TodoGrid -> Html Msg
view model =
  let
    todoLists = List.map todoListView model.lists
    isInvalid = model.title == ""
  in
    div []
        [div [ class "new-list" ]
             [ input
                  [ onInput HandleCreateTodoListTitle
                  , onEnter AddTodoList
                  , type_ "text"
                  , value model.title
                  , class "new-list--input"
                  , placeholder "Create your List"
                  ]
                  []
             , button [ onClick AddTodoList, disabled isInvalid, class "new-list--button"] [text "Create Todo List"]
             ]
        , div [class "todo-lists"] todoLists
        ]

todoListView : TodoList -> Html Msg
todoListView model =
  div
    [class "todo-list-view"]
    [
      div
        []
        [
          h1 [] [ text model.title ],
          viewInput model.listId model.newTask
        ],
      div
        []
        [tasksView model.tasks]
    ]

viewInput : Int -> String -> Html Msg
viewInput listId task =
    header
      [ class "header" ]
      [
        input
          [ class "new-todo--input"
          , placeholder "What needs to be done?"
          , autofocus True
          , value task
          , name "newTask"
          , onInput (HandleCreateTaskTitle listId)
          , onEnter (AddTask listId)
          ]
          []
      ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
      isEnter code =
        if code == 13 then
          Json.succeed msg
        else
          Json.fail "not ENTER"
    in
      on "keydown" (Json.andThen isEnter keyCode)


tasksView : List Task -> Html Msg
tasksView tasks =
  ul [class "todo-list"]
     (List.map taskView tasks)

taskView : Task -> Html Msg
taskView task =
  li [] [
    div []
        [ input
              [ class "toggle"
              , type_ "checkbox"
              , checked task.checked
              , onClick (CheckTask task.taskId (not task.checked))
              , id (toString task.taskId)
              ]
              []
              , label [for (toString task.taskId)] [text task.title]
              , button [class "destroy", onClick (RemoveTask task.taskId)] [text "Delete"]
        ]
  ]

main =
  Html.beginnerProgram { model = model, view = view, update = update }
