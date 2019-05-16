import Browser
import Html exposing (Html, button, div, span, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type Symbol = X | O | None

type alias Model =
  { grids : Array (Array Symbol)
  , turn : Symbol
  }

init : Model
init =
  { grids =  Array.repeat 9 (Array.repeat 9 None)
  , turn = X
  }


-- UPDATE

type Msg
  = Increment
  | Decrement
  | Choose Int Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model

    Decrement ->
      model
    Choose n superN ->
      let
        grids = model.grids
        grid = Maybe.withDefault (Array.repeat 9 None) (Array.get superN grids)
        newGrid = Array.set n model.turn grid
        newGrids = Array.set superN newGrid grids
      in
        { model | grids = newGrids, turn = changeTurn model.turn }


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ h1 []
    [ span [] [ text "Current turn: " ]
    , span [] [ (symbolToHtml model.turn) ]
    ]
  , div []
      [ toSuperGrid model.grids
      ]
  ]

toSuperGrid : Array (Array Symbol) -> Html Msg
toSuperGrid grid =
  div [ class "grid" ]
    [ div [ class "node" ] [ toGridNode grid (1 - 1) ]
    , div [ class "line north" ] []
    , div [ class "node" ] [ toGridNode grid (2 - 1) ]
    , div [ class "line north" ] []
    , div [ class "node" ] [ toGridNode grid (3 - 1) ]

    , div [ class "line west" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line east" ] []

    , div [ class "node" ] [ toGridNode grid (4 - 1) ]
    , div [ class "line" ] []
    , div [ class "node" ] [ toGridNode grid (5 - 1) ]
    , div [ class "line" ] []
    , div [ class "node" ] [ toGridNode grid (6 - 1) ]

    , div [ class "line west" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line east" ] []
    
    , div [ class "node" ] [ toGridNode grid (7 - 1) ]
    , div [ class "line south" ] []
    , div [ class "node" ] [ toGridNode grid (8 - 1) ]
    , div [ class "line south" ] []
    , div [ class "node" ] [ toGridNode grid (9 - 1) ]
    ]

toGridNode : Array (Array Symbol) -> Int -> Html Msg
toGridNode grid n =
  case Array.get n grid of
    Just gridNode ->
      case checkWin gridNode of
        None ->
          toGrid gridNode n
        symbol ->
          symbolToHtml symbol
    _ -> text ""

toGrid : Array Symbol -> Int -> Html Msg
toGrid grid n =
  div [ class "grid" ]
    [ div [ class "node" ] [ toNode grid 0 n ]
    , div [ class "line north" ] []
    , div [ class "node" ] [ toNode grid 1 n ]
    , div [ class "line north" ] []
    , div [ class "node" ] [ toNode grid 2 n ]

    , div [ class "line west" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line east" ] []

    , div [ class "node" ] [ toNode grid 3 n ]
    , div [ class "line" ] []
    , div [ class "node" ] [ toNode grid 4 n ]
    , div [ class "line" ] []
    , div [ class "node" ] [ toNode grid 5 n ]

    , div [ class "line west" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line" ] []
    , div [ class "line east" ] []
    
    , div [ class "node" ] [ toNode grid 6 n ]
    , div [ class "line south" ] []
    , div [ class "node" ] [ toNode grid 7 n ]
    , div [ class "line south" ] []
    , div [ class "node" ] [ toNode grid 8 n ]
    ]

toNode : Array Symbol -> Int -> Int -> Html Msg
toNode grid n superN =
  case Array.get n grid of
  Just symbol ->
    div [ class "node", onClick (Choose n superN) ] [ symbolToHtml symbol ]
  _ ->
    text ""

symbolToHtml : Symbol -> Html Msg
symbolToHtml symbol =
  case symbol of
    X -> img [ src "assets/svg/cross.svg" ] []
    O -> img [ src "assets/svg/circle.svg" ] []
    -- None -> text " "
    -- X -> div [ class "cross" ] []
    -- O -> div [ class "circle" ] []
    None -> div [ class "empty" ] []

changeTurn : Symbol -> Symbol
changeTurn symbol =
  case symbol of
    X -> O
    O -> X
    None -> None

checkWin : Array Symbol -> Symbol
checkWin grid =
  let
    e0 = Maybe.withDefault None (Array.get 0 grid)
    e1 = Maybe.withDefault None (Array.get 1 grid)
    e2 = Maybe.withDefault None (Array.get 2 grid)
    e3 = Maybe.withDefault None (Array.get 3 grid)
    e4 = Maybe.withDefault None (Array.get 4 grid)
    e5 = Maybe.withDefault None (Array.get 5 grid)
    e6 = Maybe.withDefault None (Array.get 6 grid)
    e7 = Maybe.withDefault None (Array.get 7 grid)
    e8 = Maybe.withDefault None (Array.get 8 grid)
  in
    if compareSymbols e0 e1 e2 then e0
    else if compareSymbols e3 e4 e5 then e3
    else if compareSymbols e6 e7 e8 then e6

    else if compareSymbols e0 e3 e6 then e0
    else if compareSymbols e1 e4 e7 then e1
    else if compareSymbols e2 e5 e8 then e2

    else if compareSymbols e0 e4 e8 then e0
    else if compareSymbols e2 e4 e6 then e2
    else None

compareSymbols : Symbol -> Symbol -> Symbol -> Bool
compareSymbols x y z =
  ((==) x y) && ((==) x z)