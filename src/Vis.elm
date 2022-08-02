module Vis exposing (..)

import Dict exposing (Dict)
import Force
import Html exposing (Html)
import Spotify
import Svg exposing (Svg, circle, g, image, line, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)
import Svg.Events exposing (onMouseOut, onMouseOver)
import Svg.Keyed
import Svg.Lazy


type alias Model =
    { nodes : List ArtistEntity
    , sim : Force.State String
    , related : List ( String, List String )
    , hovering : String
    }


type Msg
    = MouseOver String


type alias ArtistEntity =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : String
    , name : String
    , genres : List String
    , imgUrl : Maybe String
    , color : String
    }


xRange : Int -> List Float
xRange n =
    let
        sqr =
            ceiling (sqrt (toFloat n))
    in
    sqr
        |> List.range 0
        |> List.map toFloat
        |> List.repeat sqr
        |> List.concat


yRange : Int -> List Float
yRange n =
    let
        sqr =
            ceiling (sqrt (toFloat n))
    in
    0
        |> List.repeat sqr
        |> List.map toFloat
        |> List.repeat sqr
        |> List.map2 (\a -> List.map (\x -> x + toFloat a)) (List.range 0 sqr)
        |> List.concat


init : List Spotify.Artist -> Dict String (List String) -> Model
init artists related =
    let
        model =
            { nodes = List.map3 asEntity (xRange (List.length artists)) (yRange (List.length artists)) artists
            , sim =
                Force.simulation
                    [ links related
                    , Force.center 0 0
                    , Force.collision 40 <| List.map .id artists
                    ]
            , related = Dict.toList related
            , hovering = ""
            }

        newNodes =
            Force.computeSimulation model.sim model.nodes
    in
    { model | nodes = newNodes }


asEntity : Float -> Float -> Spotify.Artist -> ArtistEntity
asEntity x y artist =
    { x = x
    , y = y
    , vx = 0
    , vy = 0
    , id = artist.id
    , name = artist.name
    , imgUrl = artist.imgUrl
    , genres = artist.genres
    , color = genreToHexCode artist.genres
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
    1 + modBy 20 (Char.toCode c)


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseOver id ->
            { model | hovering = id }


tick : Model -> Model
tick model =
    let
        ( newSim, newNodes ) =
            Force.tick model.sim model.nodes
    in
    { model | nodes = newNodes, sim = newSim }


links : Dict String (List String) -> Force.Force String
links related =
    related
        |> Dict.map (\k v -> List.map (\artist -> ( k, artist )) v)
        |> Dict.values
        |> List.concat
        |> Force.links


view : Model -> Html Msg
view model =
    svg [ viewBox "-1000 -1000 2000 2000" ] <|
        [ Svg.Lazy.lazy (g [] << List.map (viewLink model.nodes)) model.related
        , keyedG [] <| List.map (\node -> ( node.id, Svg.Lazy.lazy2 viewNodeHelper model.hovering node )) model.nodes
        , viewHoverNodeOverlay model.hovering model.nodes
        ]


keyedG : List (Svg.Attribute msg) -> List ( String, Svg msg ) -> Svg msg
keyedG =
    Svg.Keyed.node "g"


viewNodeHelper id node =
    viewNode (id == node.id) node


viewLink : List ArtistEntity -> ( String, List String ) -> Svg msg
viewLink nodes ( from, tos ) =
    g [] <| List.map (viewLine nodes from) tos


viewLine : List ArtistEntity -> String -> String -> Svg msg
viewLine nodes from to =
    case ( lookup nodes from, lookup nodes to ) of
        ( Just a, Just b ) ->
            g []
                [ line
                    [ x1 <| s a.x
                    , y1 <| s a.y
                    , x2 <| s b.x
                    , y2 <| s b.y
                    , stroke (mix a.color b.color)
                    , strokeWidth "4px"
                    ]
                    []
                ]

        _ ->
            g [] []


mix : String -> String -> String
mix color1 color2 =
    case ( String.toList color1, String.toList color2 ) of
        ( [ '#', r1a, r1b, g1a, g1b, b1a, b1b ], [ '#', r2a, r2b, g2a, g2b, b2a, b2b ] ) ->
            String.fromList
                [ '#'
                , mixChar r1a r2a
                , mixChar r1b r2b
                , mixChar g1a g2a
                , mixChar g1b g2b
                , mixChar b1a b2a
                , mixChar b1b b2b
                ]

        _ ->
            "#888888"


mixChar : Char -> Char -> Char
mixChar a b =
    fromN <| (toN a + toN b) // 2


fromN : Int -> Char
fromN c =
    case c of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        _ ->
            'f'


toN : Char -> Int
toN c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        _ ->
            15


lookup : List ArtistEntity -> String -> Maybe ArtistEntity
lookup nodes id =
    case List.filter (\n -> n.id == id) nodes of
        [ x ] ->
            Just x

        _ ->
            Nothing


s : Float -> String
s x =
    String.fromFloat <| (toFloat (round (x * 100)) / 100)


viewNode : Bool -> ArtistEntity -> Svg Msg
viewNode hovered node =
    let
        img =
            case node.imgUrl of
                Just href ->
                    image
                        [ x <| s <| node.x
                        , y <| s <| node.y
                        , xlinkHref href
                        , class "nodeImage"
                        ]
                        []

                Nothing ->
                    g [] []

        radius =
            "28.3px"

        circ =
            circle
                [ r radius
                , cx (s node.x)
                , cy (s node.y)
                , fill node.color
                ]
                []

        hoverZone =
            circle
                [ r radius
                , onMouseOver (MouseOver node.id)
                , onMouseOut (MouseOver "")
                , cx (s node.x)
                , cy (s node.y)
                , fill "transparent"
                ]
                []
    in
    g [] <|
        if hovered then
            [ circ, img, hoverZone ]

        else
            [ circ, img, hoverZone ]


viewHoverNodeOverlay : String -> List ArtistEntity -> Svg Msg
viewHoverNodeOverlay id nodes =
    case lookup nodes id of
        Just node ->
            text_
                [ class "hoverText"
                , x (s <| node.x - 25)
                , y (s <| node.y - 20)
                ]
                [ text <|
                    node.name
                        ++ (if List.length node.genres > 0 then
                                " (" ++ String.join ", " node.genres ++ ")"

                            else
                                ""
                           )
                ]

        Nothing ->
            g [] []


genreToHexCode : List String -> String
genreToHexCode genre =
    "#"
        ++ (case genre of
                [] ->
                    "888888"

                [ red ] ->
                    toHexDigits red ++ "ffff"

                [ red, green ] ->
                    toHexDigits red ++ toHexDigits green ++ "ff"

                red :: green :: blue :: _ ->
                    toHexDigits red ++ toHexDigits green ++ toHexDigits blue
           )


toHexDigits : String -> String
toHexDigits str =
    case String.uncons str of
        Just ( x, xs ) ->
            case String.uncons xs of
                Just ( y, _ ) ->
                    toHex x ++ toHex y

                Nothing ->
                    "88"

        Nothing ->
            "88"


toHex : Char -> String
toHex c =
    case modBy 16 (Char.toCode c) of
        15 ->
            "f"

        14 ->
            "e"

        13 ->
            "d"

        12 ->
            "c"

        11 ->
            "b"

        10 ->
            "a"

        n ->
            String.fromInt n
