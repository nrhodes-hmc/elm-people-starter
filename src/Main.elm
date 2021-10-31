module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, button, div, h1, input, p, table, thead, tbody, tr, th, td, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Random

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

type alias Person =  { name : String
                     , email : String
                     , phone : String
                     , selected: Bool
                     , id: Int
                     }

type alias Model =
  { people: List Person,
    nextId: Int
  }


init : () -> (Model, Cmd Msg)
init _ = ( Model  [ Person "neil" "foo" "123kdk3'" False 1234
         , Person "obama" "bar" "990943 3 3 9" True 1235
         ] 1236
       , Cmd.none
       )
  

type Msg =   ChangeName Int String
           | ChangeEmail Int String
           | ChangePhone Int String
           | ChangeSelected Int Bool
           | SelectAll Bool
           | CreatePersonWithId Int
           | CreatePerson 
           | DeleteSelected


update: Msg->Model->(Model, Cmd Msg)

update msg model =
  case msg of
    ChangeName id newName ->
      let
        updatePerson: Person -> Person
        updatePerson t = 
          if t.id == id then
            {t | name=newName}
          else
            t
      in
        ( {model| people = List.map updatePerson model.people}
        , Cmd.none
        )

    ChangeEmail id newEmail ->
      let
        updatePerson: Person -> Person
        updatePerson t = 
          if t.id == id then
            {t | email=newEmail}
          else
            t
      in
        ( {model| people = List.map updatePerson model.people}
        , Cmd.none
        )

    ChangePhone id newPhone ->
      let
        updatePerson: Person -> Person
        updatePerson t = 
          if t.id == id then
            {t | phone=newPhone}
          else
            t
      in
        ( {model| people = List.map updatePerson model.people}
        , Cmd.none
        )

    ChangeSelected id selected ->
      let
        updatePerson: Person -> Person
        updatePerson t = 
          if t.id == id then
            {t | selected=selected}
          else
            t
      in
        ( {model| people = List.map updatePerson model.people}
        , Cmd.none
        )

    SelectAll selected->
      let
        updatePerson: Person -> Person
        updatePerson t = 
          {t | selected=selected}
      in
        ( {model| people = List.map updatePerson model.people}
        , Cmd.none
        )

    CreatePerson ->
      ( model
      , Random.generate CreatePersonWithId (Random.int 1 9999999)
      )

    CreatePersonWithId id ->
      let
        _ = Debug.log "newId" id
      in
      ( {model| 
          people = model.people ++ [Person "" "" "" False id]
        } 
      , Cmd.none
      )

    DeleteSelected ->
      ( {model| people = List.filter (\x -> not x.selected) model.people}
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

taskToRow: Person -> Html Msg
taskToRow task = 
    let
        id = task.id
    in
    tr [] 
      [ td [] [ input [ type_ "checkbox", checked task.selected, onCheck (ChangeSelected id) ] [] ]
      , td [] [ input [ value task.name, onInput (ChangeName id) ] [] ]
      , td [] [ input [ value task.email, onInput (ChangeEmail id) ] [] ]
      , td [] [ input [ value task.phone, onInput (ChangePhone id) ] [] ]
      ]

view: Model -> Html Msg
view model =
    let
        total = List.length model.people
        numSelected = List.length (List.filter (\x -> x.selected) model.people)
        selectAllMsg = SelectAll 
    in
    div []
      ([ h1 [] [text ("People, (" ++ (String.fromInt numSelected) ++ "/" ++ String.fromInt total ++ " selected)")]
        , table [] 
          [ thead []
            [ tr []
              [ th [] [ input [type_ "checkbox", checked (total==numSelected && total > 0), onCheck selectAllMsg ] [] ]
              , th [] [ text "Name" ]
              , th [] [ text "Email" ]
              , th [] [ text "Phone" ]
              ]
            ]
          , tbody []
            (List.map taskToRow model.people)
          ]
        , button [ onClick CreatePerson ] [ text "Add" ]
      ] ++
      if numSelected <= 0 then [] else
        [button [ onClick DeleteSelected ] [ text "🗑" ]])