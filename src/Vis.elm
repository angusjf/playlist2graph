module Vis exposing (..)

import Dict exposing (Dict)
import Force
import Html exposing (Html)
import Spotify
import Svg exposing (Svg, circle, g, line, svg, text, text_)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, viewBox, x, x1, x2, y, y1, y2)


type alias Model =
    { nodes : List ArtistEntity, sim : Force.State String, related : List ( String, List String ) }


type alias ArtistEntity =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : String
    , name : String
    , imgUrl : String
    }


init : List Spotify.Artist -> Dict String (List Spotify.Artist) -> Model
init artists related =
    { nodes = List.map asEntity artists
    , sim =
        Force.simulation
            [ links related

            -- , Force.center 0 0
            , Force.collision 30 (List.map .id artists)
            ]
    , related = Dict.toList <| Dict.map (always <| List.map .id) related
    }


asEntity : Spotify.Artist -> ArtistEntity
asEntity { id, name, imgUrl } =
    { x = 0
    , y = 0
    , vx = toFloat <| modBy 1000 <| stringToInt id
    , vy = toFloat <| modBy 1000 <| stringToInt name
    , id = id
    , name = name
    , imgUrl = imgUrl
    }


stringToInt : String -> Int
stringToInt str =
    case String.uncons str of
        Nothing ->
            1

        Just ( x, xs ) ->
            charToInt x * stringToInt xs


charToInt : Char -> Int
charToInt c =
    1 + modBy 3 (Char.toCode c)


update : Model -> Model
update model =
    let
        ( newSim, newNodes ) =
            Force.tick model.sim model.nodes
    in
    { model | nodes = newNodes, sim = newSim }


links : Dict String (List Spotify.Artist) -> Force.Force String
links related =
    related
        |> Dict.map (\k v -> List.map (\artist -> ( k, artist.id )) v)
        |> Dict.values
        |> List.concat
        |> Force.links


view : Model -> Html msg
view model =
    svg [ viewBox "0 0 2000 2000" ] <|
        [ g [] <| List.map (viewLink model.nodes) model.related
        , g [] <| List.map viewNode model.nodes
        ]


viewLink : List ArtistEntity -> ( String, List String ) -> Svg msg
viewLink nodes ( from, tos ) =
    g [] <| List.map (viewLine nodes from) tos


viewLine : List ArtistEntity -> String -> String -> Svg msg
viewLine nodes from to =
    let
        a =
            lookup nodes from

        b =
            lookup nodes to
    in
    line
        [ x1 <| s a.x
        , y1 <| s a.y
        , x2 <| s b.x
        , y2 <| s b.y
        , stroke "coral"
        ]
        []


lookup nodes id =
    case List.filter (\n -> n.id == id) nodes of
        [ x ] ->
            x

        _ ->
            Debug.todo "LOOKUP FAIL - COULD BE TOO MANY?"


s : Float -> String
s =
    String.fromFloat


viewNode : ArtistEntity -> Svg msg
viewNode =
    \node ->
        g
            []
            [ circle [ r "3px", cx (s node.x), cy (s node.y), fill "grey" ] []
            , text_ [ x (s node.x), y (s node.y) ] [ text node.name ]
            ]
