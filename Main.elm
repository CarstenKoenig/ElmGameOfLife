module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Time exposing (Time)
import Array exposing (Array)
import Maybe.Extra exposing (values)
import Random as Rnd exposing (Generator)
import Random.Extra as RndE
import Random.Array as RndArr


type alias Model =
    Array (Array Cell)


type Cell
    = Alive
    | Dead


type Message
    = Tick Time
    | SetModel Model


main : Program Never Model Message
main =
    Html.program
        { init = emptyModel 0 0 ! [ randomModel 100 80 ]
        , update = update
        , view = view
        , subscriptions = always (Time.every (100 * Time.millisecond) Tick)
        }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        SetModel model ->
            model ! []

        Tick _ ->
            iterate model ! []


view : Model -> Html Message
view model =
    let
        pxls =
            800 // Array.length model
    in
        div [] (Array.toList <| Array.map (viewRow pxls) model)


viewRow : Int -> Array Cell -> Html Message
viewRow pxls cells =
    div []
        ((Array.map (viewCell pxls) cells
            |> Array.toList
         )
            ++ [ div [ Attr.style [ ( "clear", "both" ) ] ] [] ]
        )


viewCell : Int -> Cell -> Html Message
viewCell pxls cell =
    let
        pixels =
            toString pxls ++ "px"

        color =
            case cell of
                Dead ->
                    "lightgray"

                Alive ->
                    "slateblue"
    in
        div
            [ Attr.style
                [ ( "background", color )
                , ( "width", pixels )
                , ( "height", pixels )
                , ( "float", "left" )
                ]
            ]
            []


type alias CellStat =
    { aliveNeighbors : Int
    , deadNeighbors : Int
    }


iterate : Model -> Model
iterate model =
    let
        iterateCell ( x, y ) cell =
            let
                cellStat =
                    getCellStat model ( x, y )
            in
                case cell of
                    Dead ->
                        if cellStat.aliveNeighbors == 3 then
                            Alive
                        else
                            Dead

                    Alive ->
                        if cellStat.aliveNeighbors < 2 then
                            Dead
                        else if cellStat.aliveNeighbors <= 3 then
                            Alive
                        else
                            Dead

        iterateRow y =
            Array.indexedMap (\x cell -> iterateCell ( x, y ) cell)
    in
        Array.indexedMap iterateRow model


getCellStat : Model -> ( Int, Int ) -> CellStat
getCellStat cells ( x, y ) =
    let
        alive =
            getNeighbors cells ( x, y )
                |> List.filter (\c -> c == Alive)
                |> List.length

        dead =
            8 - alive
    in
        CellStat alive dead


getNeighbors : Model -> ( Int, Int ) -> List Cell
getNeighbors cells ( x, y ) =
    let
        nPoss =
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x + 1, y + 1 )
            , ( x, y + 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y )
            ]
    in
        List.map (getCell cells) nPoss
            |> values


getCell : Model -> ( Int, Int ) -> Maybe Cell
getCell cells ( x, y ) =
    cells
        |> Array.get y
        |> Maybe.andThen (Array.get x)


emptyModel : Int -> Int -> Model
emptyModel w h =
    let
        row _ =
            Array.initialize w (always Dead)
    in
        Array.initialize h row


randomModel : Int -> Int -> Cmd Message
randomModel w h =
    Rnd.generate SetModel (randomRows w h)


randomRows : Int -> Int -> Generator Model
randomRows w h =
    RndArr.array h (randomRow w)


randomRow : Int -> Generator (Array Cell)
randomRow n =
    RndArr.array n randomCell


randomCell : Generator Cell
randomCell =
    RndE.frequency
        [ ( 0.1, RndE.constant Alive )
        , ( 0.9, RndE.constant Dead )
        ]
