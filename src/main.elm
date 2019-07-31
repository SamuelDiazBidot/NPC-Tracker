import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, class)

-- MAIN
main =
    Browser.element 
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view 
        }

-- NPC --
type alias Npc = 
    { name : String
    , armor_class : Int
    , hit_points : Int 
    , id : Int
    }

new_npc : String -> Int -> Npc --todo: use Maybe Npc 
new_npc name id =
    case name of
        "Kobold" -> 
            { name = "Kobold", armor_class = 12, hit_points = 5 , id = id }
        "Cultist" -> 
            { name = "Cultist", armor_class = 12, hit_points = 9, id = id }
        "Ambush Drake" -> 
            { name = "Ambush Drake", armor_class = 13, hit_points = 22, id = id }
        "Acolyte" -> 
            { name = "Acolyte", armor_class = 10, hit_points = 9, id = id}
        "Guard" -> 
            { name = "Guard", armor_class = 16, hit_points = 11, id = id}
        _ -> 
            { name = "", armor_class = 0, hit_points = 0, id = id}

-- MODEL --
type alias Model =
    { npc_list : List Npc 
    , selected_npc : String 
    , uid : Int
    }

init : () -> ( Model, Cmd Msg ) 
init _ =
    ( { npc_list = [] 
      , selected_npc = "Kobold" 
      , uid = 0
      }
    , Cmd.none 
    )

-- UPDATE --
type Msg 
    = Add 
    | ChangeSelectedNpc String 
    | Delete Int
    | UpdateHP Int Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Add ->
            ( { model | uid = model.uid + 1
              , npc_list = model.npc_list ++ [ new_npc model.selected_npc model.uid ] 
              }
            , Cmd.none 
            )
        ChangeSelectedNpc name -> 
            ( { model | selected_npc = name }
            , Cmd.none 
            )
        Delete id ->
            ( { model | npc_list = List.filter (\t -> t.id /= id ) model.npc_list } 
            , Cmd.none
            )
        UpdateHP id damage -> 
            let 
                updateHp t = 
                    if t.id == id then
                        { t | hit_points = t.hit_points + damage }
                    else 
                        t
            in 
                ( { model | npc_list = List.map updateHp model.npc_list }
                , Cmd.none 
                )
-- VIEW --
view : Model -> Html Msg
view model =
    div []
        [ selectNpc 
        , renderList model.npc_list 
        ]

selectNpc : Html Msg
selectNpc = 
    div [ class "add-npc" ]
        [ h1 [ class "title" ][ text "NPC Tracker"]
        , button [ onClick Add , class "button" ] [text "Add" ]
        , select [ class "npcSelector"] 
            [ option [ onClick ( ChangeSelectedNpc "Kobold" ) ] [ text "Kobold" ] 
            , option [ onClick ( ChangeSelectedNpc "Cultist" ) ] [ text "Cultist" ] 
            , option [ onClick ( ChangeSelectedNpc "Ambush Drake" ) ] [text "Ambush Drake"]
            , option [ onClick ( ChangeSelectedNpc "Acolyte" ) ] [ text "Acolyte" ]
            , option [ onClick ( ChangeSelectedNpc "Guard" ) ] [ text "Guard"]
            ]
        ]

renderList : List Npc -> Html Msg 
renderList lst =
    lst 
        |> List.map (\l -> div [ class "npc-list"]
                            [ div [] [ text l.name ] 
                            , div [] [text ( "Armor Class: " ++ String.fromInt l.armor_class ) ]
                            , div [] [ text ( "Hit Points: " ++ String.fromInt l.hit_points ) ]
                            , button [ onClick (UpdateHP l.id 1), class "hp-button" ] [ text "▲"]
                            , button [ onClick (UpdateHP l.id -1), class "hp-button" ] [ text "▼"]
                            , button [ onClick (Delete l.id), class "delete-button"] [ text "X" ]
                            ]
                    )
        |> div [ class "npc-div"]
