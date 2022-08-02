module Spotify exposing (..)

import Http exposing (Error(..), Response(..))
import Json.Decode


type alias Playlist =
    { items : List Item }


type alias Item =
    { artistIds : List String }


type alias Artist =
    { id : String, name : String, imgUrl : Maybe String, genres : List String }


playlistsArtists : Playlist -> List String
playlistsArtists playlist =
    playlist
        |> .items
        |> List.concatMap .artistIds


playlistDecoder : Json.Decode.Decoder Playlist
playlistDecoder =
    Json.Decode.map Playlist
        (Json.Decode.field "items" <| Json.Decode.list itemDecoder)


itemDecoder : Json.Decode.Decoder Item
itemDecoder =
    Json.Decode.field "track"
        (Json.Decode.map Item
            (Json.Decode.field "artists" <| Json.Decode.list (Json.Decode.field "id" Json.Decode.string))
        )


maybeFlat : Maybe (Maybe a) -> Maybe a
maybeFlat m =
    case m of
        Just (Just a) ->
            Just a

        _ ->
            Nothing


artistDecoder : Json.Decode.Decoder Artist
artistDecoder =
    Json.Decode.map4 Artist
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "images"
            (Json.Decode.map
                (Maybe.map Tuple.second << List.head << List.sortBy (\( height, _ ) -> height))
                (Json.Decode.list
                    (Json.Decode.map2 Tuple.pair
                        (Json.Decode.field "height" Json.Decode.int)
                        (Json.Decode.field "url" Json.Decode.string)
                    )
                )
            )
        )
        (Json.Decode.field "genres" (Json.Decode.list Json.Decode.string))


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


fetchPlaylistData :
    { playlistId : String
    , accessToken : String
    , toMsg : Result Error (List String) -> msg
    , offset : Int
    }
    -> Cmd msg
fetchPlaylistData { playlistId, accessToken, toMsg, offset } =
    getRequest
        { accessToken = accessToken
        , path = "playlists/" ++ playlistId ++ "/tracks?offset=" ++ String.fromInt offset
        , toMsg = toMsg
        , decoder = Json.Decode.map playlistsArtists playlistDecoder
        }


fetchArtistsData :
    { artistIds : List String
    , accessToken : String
    , toMsg : Result Error (List Artist) -> msg
    }
    -> Cmd msg
fetchArtistsData { artistIds, accessToken, toMsg } =
    getRequest
        { accessToken = accessToken
        , path = "artists?ids=" ++ String.join "," artistIds
        , toMsg = toMsg
        , decoder = Json.Decode.field "artists" <| Json.Decode.list artistDecoder
        }


fetchRelatedArtists :
    { accessToken : String
    , toMsg : Result Error (List String) -> msg
    , id : String
    }
    -> Cmd msg
fetchRelatedArtists { accessToken, toMsg, id } =
    getRequest
        { accessToken = accessToken
        , path = "artists/" ++ id ++ "/related-artists"
        , toMsg = toMsg
        , decoder =
            Json.Decode.map (List.map .id) <|
                Json.Decode.field "artists" (Json.Decode.list artistDecoder)
        }
