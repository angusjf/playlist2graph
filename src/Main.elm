port module Main exposing (main, playlistUrlParser, save)

import Browser
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Set
import Spotify
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query
import Vis


port save : Save -> Cmd msg


type alias Model =
    { accessToken : String
    , refreshToken : String
    , state : State
    , playlistId : String
    , oldSave : Maybe Save
    }


type State
    = FetchingPlaylistData (List String)
    | FetchingArtistData { artistIds : List String, artists : List Spotify.Artist }
    | FetchingRelatedArtists { artists : List Spotify.Artist, relations : Dict String (List String) }
    | Completed Vis.Model
    | HttpError Http.Error
    | OtherError String
    | Start
    | ChoosePlaylist String


type Msg
    = GotPlaylistData (Result Http.Error (List String))
    | GotArtistData (List String) (Result Http.Error (List Spotify.Artist))
    | GotRelatedArtistsData String (List String) (Result Http.Error (List String))
    | UrlChange
    | UrlRequest
    | Tick
    | VisMsg Vis.Msg
    | SubmitStartForm
    | ChangeAccessToken String
    | ChangePlaylistId String
    | RedirectToSpotify
    | ChangePlaylistUrl String
    | SelectPlaylist String


type alias Save =
    { accessToken : String
    , refreshToken : String
    , playlistId : String
    , artists : List Spotify.Artist
    , relations : List ( String, List String )
    }


init : Maybe Save -> Url -> Key -> ( Model, Cmd Msg )
init old url _ =
    let
        model =
            case tokens url of
                Just ( access, refresh ) ->
                    { oldSave = old
                    , accessToken = access
                    , refreshToken = refresh
                    , playlistId = Maybe.withDefault "" <| Maybe.map .playlistId old
                    , state = Start
                    }

                Nothing ->
                    { oldSave = old
                    , accessToken = Maybe.withDefault "" <| Maybe.map .accessToken old
                    , refreshToken = Maybe.withDefault "" <| Maybe.map .refreshToken old
                    , playlistId = Maybe.withDefault "" <| Maybe.map .playlistId old
                    , state = Start
                    }
    in
    ( case ( model.accessToken, model.refreshToken ) of
        ( "", "" ) ->
            model

        _ ->
            { model
                | state = ChoosePlaylist ""
            }
    , Cmd.none
    )


tokens : Url -> Maybe ( String, String )
tokens url =
    case
        Url.Parser.parse
            (Url.Parser.s "apps"
                </> Url.Parser.s "playlist2graph.html"
                <?> Url.Parser.Query.map2
                        Tuple.pair
                        (Url.Parser.Query.string "access_token")
                        (Url.Parser.Query.string "refresh_token")
            )
            url
    of
        Just ( Just a, Just r ) ->
            Just ( a, r )

        _ ->
            Nothing


view : Model -> Browser.Document Msg
view model =
    { title = "playlist 2 graph"
    , body =
        [ case model.state of
            FetchingPlaylistData ids ->
                text <| "loaded in " ++ String.fromInt (List.length ids) ++ " artists"

            FetchingArtistData { artistIds, artists } ->
                text <|
                    let
                        done =
                            String.fromInt (List.length artists)

                        total =
                            String.fromInt (List.length artistIds)
                    in
                    "loading artist details, " ++ done ++ "/" ++ total ++ " completed"

            FetchingRelatedArtists { artists, relations } ->
                text <|
                    let
                        done =
                            String.fromInt (Dict.size relations)

                        total =
                            String.fromInt (List.length artists)
                    in
                    "loading artist relationships, " ++ done ++ "/" ++ total ++ " completed"

            Completed vis ->
                Html.map VisMsg <| Vis.view vis

            HttpError e ->
                viewHttpError e

            OtherError e ->
                text <| e

            Start ->
                viewStart model

            ChoosePlaylist url ->
                viewChoosePlaylist url
        ]
    }


viewHttpError : Error -> Html msg
viewHttpError e =
    text <|
        "Spotify API Error ("
            ++ (case e of
                    BadUrl url ->
                        "Bad URL: " ++ url

                    Timeout ->
                        "Timeout"

                    NetworkError ->
                        "Network error"

                    BadStatus code ->
                        "Bad status: "
                            ++ (case code of
                                    429 ->
                                        "I made too many requests and they kicked me off </3"

                                    _ ->
                                        String.fromInt code
                               )

                    BadBody body ->
                        "Bad body :" ++ body
               )
            ++ ")"


viewChoosePlaylist : String -> Html Msg
viewChoosePlaylist url =
    div []
        [ div [] [ text "playlist2graph" ]
        , input
            [ onInput ChangePlaylistUrl
            , placeholder "spotify playlist url"
            , value url
            ]
            []
        , button [ onClick (SelectPlaylist url) ] [ text "select playlist!" ]
        ]


viewStart : Model -> Html Msg
viewStart model =
    div []
        [ div [] [ text "playlist2graph" ]
        , button [ onClick RedirectToSpotify ] [ text "sign in with spotify!" ]
        ]


badMsg : State
badMsg =
    OtherError "Bad Msg"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlaylistData result ->
            case result of
                Ok ids ->
                    case model.state of
                        FetchingPlaylistData soFar ->
                            let
                                all =
                                    ids ++ soFar
                            in
                            case ids of
                                [] ->
                                    case all of
                                        [] ->
                                            ( { model | state = OtherError "Spotify API returned no songs" }, Cmd.none )

                                        _ ->
                                            ( { model | state = FetchingArtistData { artistIds = all, artists = [] } }
                                            , Spotify.fetchArtistsData
                                                { accessToken = model.accessToken
                                                , artistIds = List.take fourtyNine all
                                                , toMsg = GotArtistData (List.drop fourtyNine all)
                                                }
                                            )

                                _ ->
                                    ( { model | state = FetchingPlaylistData all }
                                    , Spotify.fetchPlaylistData
                                        { playlistId = model.playlistId
                                        , accessToken = model.accessToken
                                        , toMsg = GotPlaylistData
                                        , offset = List.length all
                                        }
                                    )

                        _ ->
                            ( { model | state = badMsg }, Cmd.none )

                Err e ->
                    ( { model | state = HttpError e }, Cmd.none )

        GotArtistData nextLot result ->
            case result of
                Ok newArtists ->
                    case model.state of
                        FetchingArtistData { artistIds, artists } ->
                            let
                                all =
                                    artists ++ newArtists
                            in
                            case nextLot of
                                [] ->
                                    case all of
                                        [] ->
                                            ( { model | state = OtherError "API never returned any songs" }, Cmd.none )

                                        next :: more ->
                                            ( { model | state = FetchingRelatedArtists { artists = removeDuplicates all, relations = Dict.empty } }
                                            , Spotify.fetchRelatedArtists
                                                { accessToken = model.accessToken
                                                , toMsg = GotRelatedArtistsData next.id (List.map .id more)
                                                , id = next.id
                                                }
                                            )

                                _ ->
                                    ( { model | state = FetchingArtistData { artistIds = artistIds, artists = all } }
                                    , Spotify.fetchArtistsData
                                        { accessToken = model.accessToken
                                        , toMsg = GotArtistData <| List.drop fourtyNine nextLot
                                        , artistIds = List.take fourtyNine nextLot
                                        }
                                    )

                        _ ->
                            ( { model | state = badMsg }, Cmd.none )

                Err e ->
                    ( { model | state = HttpError e }, Cmd.none )

        GotRelatedArtistsData id remaining result ->
            case result of
                Ok new ->
                    case model.state of
                        FetchingRelatedArtists { artists, relations } ->
                            let
                                all =
                                    Dict.insert id (List.filter (\artist -> List.member artist (List.map .id artists)) new) relations
                            in
                            case remaining of
                                [] ->
                                    ( { model
                                        | state = Completed <| Vis.init artists all
                                      }
                                    , save
                                        { playlistId = model.playlistId
                                        , artists = artists
                                        , accessToken = model.accessToken
                                        , refreshToken = model.refreshToken
                                        , relations = Dict.toList all
                                        }
                                    )

                                next :: more ->
                                    ( { model | state = FetchingRelatedArtists { artists = artists, relations = all } }
                                    , Spotify.fetchRelatedArtists
                                        { accessToken = model.accessToken
                                        , toMsg = GotRelatedArtistsData next more
                                        , id = next
                                        }
                                    )

                        _ ->
                            ( { model | state = badMsg }, Cmd.none )

                Err e ->
                    ( { model | state = HttpError e }, Cmd.none )

        Tick ->
            case model.state of
                Completed vis ->
                    ( { model | state = Completed (Vis.tick vis) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        VisMsg vMsg ->
            case model.state of
                Completed vis ->
                    ( { model | state = Completed (Vis.update vMsg vis) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UrlRequest ->
            ( { model | state = OtherError "url request" }, Cmd.none )

        UrlChange ->
            ( { model | state = OtherError "url change" }, Cmd.none )

        ChangeAccessToken new ->
            ( { model | accessToken = new }, Cmd.none )

        ChangePlaylistId new ->
            ( { model | playlistId = new }, Cmd.none )

        SubmitStartForm ->
            case
                model.oldSave
                    |> Maybe.andThen
                        (\old ->
                            if old.playlistId == model.playlistId then
                                Just old

                            else
                                Nothing
                        )
            of
                Just saveFile ->
                    ( { model
                        | state = Completed <| Vis.init saveFile.artists (Dict.fromList saveFile.relations)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | state = FetchingPlaylistData [] }
                    , Spotify.fetchPlaylistData
                        { playlistId = model.playlistId
                        , accessToken = model.accessToken
                        , toMsg = GotPlaylistData
                        , offset = 0
                        }
                    )

        RedirectToSpotify ->
            ( model, getToken )

        SelectPlaylist url ->
            case parsePlaylistUrl url of
                Just id ->
                    ( { model | playlistId = id, state = FetchingPlaylistData [] }
                    , Spotify.fetchPlaylistData
                        { playlistId = id
                        , accessToken = model.accessToken
                        , toMsg = GotPlaylistData
                        , offset = 0
                        }
                    )

                Nothing ->
                    ( { model | state = OtherError "bad playlist url - no parse :(" }, Cmd.none )

        ChangePlaylistUrl new ->
            ( { model
                | state =
                    case model.state of
                        ChoosePlaylist _ ->
                            ChoosePlaylist new

                        _ ->
                            badMsg
              }
            , Cmd.none
            )



-- "https://open.spotify.com/playlist/7o5522ys8iByNQOtqWgxCx?si=f296ac97d0984ee9"


parsePlaylistUrl : String -> Maybe String
parsePlaylistUrl urlString =
    Url.fromString urlString
        |> Maybe.andThen
            (\url ->
                Url.Parser.parse playlistUrlParser url
            )


playlistUrlParser : Url.Parser.Parser (String -> String) String
playlistUrlParser =
    Url.Parser.s "playlist" </> Url.Parser.string


fourtyNine : Int
fourtyNine =
    49


removeDuplicates : List a -> List a
removeDuplicates =
    List.foldr
        (\artist acc ->
            if List.member artist acc then
                acc

            else
                artist :: acc
        )
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrame (always Tick)


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    UrlRequest


onUrlChange : Url -> Msg
onUrlChange _ =
    UrlChange


getToken : Cmd msg
getToken =
    let
        clientId =
            "f715b26ca7b949cf845e35284242fadc"

        scope =
            "user-read-private user-read-email"

        redirectUri =
            "https://c2d5zl3brn4hov5v5kecbep7dq0btyil.lambda-url.eu-west-2.on.aws"

        state =
            "OnBrowserNavigationLoad"
    in
    Browser.Navigation.load <|
        Url.Builder.crossOrigin "https://accounts.spotify.com"
            [ "authorize" ]
            [ Url.Builder.string "response_type" "code"
            , Url.Builder.string "client_id" clientId
            , Url.Builder.string "scope" scope
            , Url.Builder.string "redirect_uri" redirectUri
            , Url.Builder.string "state" state
            ]


main : Program (Maybe Save) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
