module Spotify exposing (..)

import Http exposing (Error(..), Response(..))
import Json.Decode
import Task exposing (Task)


type alias Playlist =
    { items : List Item }


type alias Item =
    { artists : List Artist }


type alias Album =
    { imgUrl : String }


type alias Artist =
    { id : String, name : String, imgUrl : String }


playlistsArtists : Playlist -> List Artist
playlistsArtists playlist =
    playlist
        |> .items
        |> List.concatMap .artists


playlistDecoder : Json.Decode.Decoder Playlist
playlistDecoder =
    Json.Decode.map Playlist
        (Json.Decode.field "items" <| Json.Decode.list itemDecoder)


itemDecoder : Json.Decode.Decoder Item
itemDecoder =
    Json.Decode.map (\( a, b ) -> Item (List.map (\artist -> { artist | imgUrl = b.imgUrl }) a))
        (Json.Decode.field "track"
            (Json.Decode.map2 Tuple.pair
                (Json.Decode.field "artists" <| Json.Decode.list artistDecoder)
                (Json.Decode.field "album" albumDecoder)
            )
        )


albumDecoder : Json.Decode.Decoder Album
albumDecoder =
    Json.Decode.map Album
        (Json.Decode.field "images"
            (Json.Decode.map
                (Maybe.withDefault "" << List.head << List.reverse)
                (Json.Decode.list (Json.Decode.field "url" Json.Decode.string))
            )
        )


artistDecoder : Json.Decode.Decoder Artist
artistDecoder =
    Json.Decode.map2 (\id name -> Artist id name "")
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)


getArtists : String -> (Result Http.Error (List Artist) -> msg) -> Int -> Cmd msg
getArtists accessToken toMsg offset =
    let
        id =
            --"6fuUY64Kvu49VhQlYck2PR"
            "7odAmSxIw5X6DrldzMrb0x"
    in
    getRequest
        { accessToken = accessToken
        , path = "playlists/" ++ id ++ "/tracks?offset=" ++ String.fromInt offset
        , toMsg = toMsg
        , decoder = Json.Decode.map playlistsArtists playlistDecoder
        }


getRequest : { accessToken : String, path : String, toMsg : Result Http.Error a -> msg, decoder : Json.Decode.Decoder a } -> Cmd msg
getRequest { accessToken, path, toMsg, decoder } =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            , Http.header "Content-Type" "application/json"
            , Http.header "Authorization" ("Bearer " ++ accessToken)
            ]
        , url = "https://api.spotify.com/v1/" ++ path
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getRelatedArtists : String -> (Result Http.Error (List Artist) -> msg) -> Artist -> Cmd msg
getRelatedArtists accessToken toMsg { id } =
    getRequest
        { accessToken = accessToken
        , path = "artists/" ++ id ++ "/related-artists"
        , toMsg = toMsg
        , decoder = Json.Decode.field "artists" (Json.Decode.list artistDecoder)
        }
