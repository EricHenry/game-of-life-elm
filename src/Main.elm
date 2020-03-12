module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, h1)
import Html.Events exposing (onClick)
import Random
import Svg
import Svg.Attributes



---- CONSTANTS ----


height : Int
height =
    500


width : Int
width =
    500


cell_height : Int
cell_height =
    10


cell_width : Int
cell_width =
    10


rows : Int
rows =
    height // cell_height


columns : Int
columns =
    width // cell_width


total_cells : Int
total_cells =
    rows * columns


seed : Random.Seed
seed =
    Random.initialSeed 31415


liveNeighbors : Int -> Array Bool -> Int
liveNeighbors index cells =
    let
        north_index =
            index - columns

        south_index =
            index + columns

        east_index =
            index + 1

        west_index =
            index - 1

        north_east_index =
            north_index + 1

        north_west_index =
            north_index - 1

        south_east_index =
            south_index + 1

        south_west_index =
            south_index - 1

        neighbors_count =
            [ north_index, south_index, east_index, west_index, north_east_index, north_west_index, south_east_index, south_west_index ]
                |> List.map (\i -> Array.get i cells)
                |> List.filter (Maybe.withDefault False)
                |> List.length
    in
    neighbors_count


update_cell : Array Bool -> Int -> Bool -> Bool
update_cell cells index alive =
    --
    -- Rules for the Game of Life.
    --
    --  1. any live cell with less than 2 or greater than 3 neighbours dies
    --  2. any dead cell with 3 live neighbours lives
    --
    let
        neighbors_count =
            liveNeighbors index cells

        life =
            if alive && (neighbors_count == 2 || neighbors_count == 3) then
                True

            else if not alive && neighbors_count == 3 then
                True

            else
                False
    in
    life



--
--    Random.list 1000 (Random.weighted ( 50, True ) [ ( 50, False ) ])
-- Random.step
--     (Random.weighted ( 50, True ) [ ( 50, False ) ])
--     seed
--     |> List.repeat 1000
--     |> List.map (\( b, _ ) -> b)
-- [ False, True, False, False, False, False, True, True, False ]
---- MODEL ----


type alias Cell =
    ( Bool, Random.Seed )


type alias Model =
    { cells : Array Bool
    , run : Bool
    , count : Float
    }


init_cells : Array Bool
init_cells =
    Array.repeat total_cells False


init : ( Model, Cmd Msg )
init =
    ( { cells = init_cells
      , run = False
      , count = 0
      }
    , Cmd.none
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame



---- UPDATE ----


type Msg
    = Clicked Int
    | Frame Float
    | Run Bool
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked index ->
            let
                new_cells =
                    model.cells
                        -- get the current cell
                        |> Array.get index
                        -- exract the value from the maybe defaulting to False
                        |> Maybe.withDefault False
                        -- flip the value of the boolean
                        |> not
                        -- set the new value partially
                        |> Array.set index
                        -- finiish setting the value
                        |> (\fn -> fn model.cells)
            in
            ( { model
                | cells =
                    if not model.run then
                        new_cells

                    else
                        model.cells
              }
            , Cmd.none
            )

        Frame _ ->
            ( { model
                | count =
                    if model.run then
                        model.count + 1

                    else
                        model.count
                , cells =
                    if model.run && modBy 60 (round model.count) == 0 then
                        Array.indexedMap (update_cell model.cells) model.cells

                    else
                        model.cells
              }
            , Cmd.none
            )

        Run run ->
            ( { model | run = run }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ {--img [ src "/logo.svg" ] [] --}
          h1 []
            [ Html.text <| "Conway's Game of Life" ]
        , Html.p [] [ Html.text "Pause the game and click on cells to bring them to life" ]
        , Html.p [] [ Html.text "Play the game to watch the cell's lives" ]
        , div []
            [ button [ onClick <| Run True ] [ Html.text "Play" ]
            , button [ onClick <| Run False ] [ Html.text "Pause" ]
            ]
        , Html.h2 []
            [ Html.text <|
                if model.run then
                    "Game State: Playing"

                else
                    "Game State: Paused"
            ]
        , Svg.svg
            [ Svg.Attributes.viewBox <| "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height
            , Svg.Attributes.height (String.fromInt height)
            , Svg.Attributes.width (String.fromInt width)
            , Svg.Attributes.style "background-color: antiquewhite"
            ]
            (List.indexedMap viewCell (Array.toList model.cells))
        ]


viewCell : Int -> Bool -> Svg.Svg Msg
viewCell index cell =
    Svg.rect
        [ Svg.Attributes.height (String.fromInt cell_height)
        , Svg.Attributes.width (String.fromInt cell_width)
        , Svg.Attributes.x <| String.fromInt (modBy columns index * cell_width)
        , Svg.Attributes.y <| String.fromInt (index // columns * cell_height)
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.fill <|
            if cell then
                "black"

            else
                "white"
        , onClick (Clicked index)
        ]
        []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update

        -- , subscriptions = always Sub.none
        , subscriptions = subscriptions
        }
