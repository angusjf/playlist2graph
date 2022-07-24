port module Main exposing (..)

import Browser
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (li, text, ul)
import Http
import Remote exposing (Remote(..))
import Spotify
import Url exposing (Url)
import Url.Parser exposing ((<?>))
import Vis


port save : Save -> Cmd msg


type alias Model =
    { accessToken : String
    , artists : Remote (List Spotify.Artist)
    , relatedArtists : Dict String (List Spotify.Artist)
    , debug : Bool
    , vis : Maybe Vis.Model
    }


type Msg
    = GotArtists Int (Result Http.Error (List Spotify.Artist))
    | UrlChange
    | UrlRequest
    | GotRelatedArtists String (List Spotify.Artist) (Result Http.Error (List Spotify.Artist))
    | Tick


type alias Flags =
    Maybe Save


type alias Save =
    ( List Spotify.Artist, List ( String, List Spotify.Artist ) )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init old _ _ =
    case old of
        Just ( artists, relatedArtists ) ->
            ( { accessToken = "ksjdklsafjsdl"
              , artists = Done artists
              , relatedArtists = Dict.fromList relatedArtists
              , debug = False
              , vis = Just <| Vis.init artists (Dict.fromList relatedArtists)
              }
            , Cmd.none
            )

        Nothing ->
            let
                model =
                    { accessToken = "BQBjZzMRcrA2REC1YMOJbqVqilk-GVEBMsDnGa0N_H1Qs9L0f6Wf7EbcG7RZINVaEUC8j3RhkTyEROAvYDr1C7QvF4SxxIp1l3IZsRlOS5DNgsgqhqoz1HFoU6QqtTP9o7wRpRs-EuPtORInJwR2KZslWKtJgIJzJ2Fj0eCyc970WzU"
                    , artists = Fetching
                    , relatedArtists = Dict.empty
                    , debug = False
                    , vis = Nothing
                    }
            in
            ( model
            , Spotify.getArtists model.accessToken (GotArtists 0) 0
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Spotify Visualiser"
    , body =
        [ case model.artists of
            Fetching ->
                text "fetching..."

            Done artists ->
                case ( model.debug, model.vis ) of
                    ( _, Just vis ) ->
                        Vis.view vis

                    _ ->
                        viewArtists artists model.relatedArtists

            Error e ->
                text <| Debug.toString e
        ]
    }


viewArtists : List Spotify.Artist -> Dict String (List Spotify.Artist) -> Html.Html msg
viewArtists artists relatedArtists =
    ul [] <|
        List.map (viewArtist relatedArtists) artists


viewArtist : Dict String (List Spotify.Artist) -> Spotify.Artist -> Html.Html msg
viewArtist relatedArtists artist =
    li
        []
        [ text artist.name
        , ul
            []
          <|
            List.map
                (\{ name } -> li [] [ text name ])
                (Maybe.withDefault [] <| Dict.get artist.id relatedArtists)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotArtists offset result ->
            case ( model.artists, result ) of
                ( Done artists, Ok [] ) ->
                    ( model
                    , fetchArtistsSeries model.accessToken artists
                    )

                ( _, Ok newArtists ) ->
                    ( { model
                        | artists = Done <| nub <| Remote.withDefault [] model.artists ++ newArtists
                      }
                    , Spotify.getArtists model.accessToken (GotArtists (offset + 35)) (offset + 35)
                    )

                ( _, Err e ) ->
                    ( { model | artists = Error e }
                    , Cmd.none
                    )

        UrlRequest ->
            Debug.todo "!"

        UrlChange ->
            Debug.todo "!"

        GotRelatedArtists id more result ->
            case ( result, model.artists ) of
                ( Ok related, Done artists ) ->
                    let
                        new =
                            Dict.insert
                                id
                                (List.filter (\artist -> List.member artist artists) related)
                                model.relatedArtists

                        nextModel =
                            { model | relatedArtists = new }

                        fetchMore =
                            fetchArtistsSeries model.accessToken more
                    in
                    case model.artists of
                        Done done ->
                            if Dict.size new == List.length done then
                                ( { nextModel | vis = Just <| Vis.init done new }
                                , save ( done, Dict.toList nextModel.relatedArtists )
                                )

                            else
                                ( nextModel, fetchMore )

                        _ ->
                            ( nextModel, fetchMore )

                ( e, _ ) ->
                    ( Debug.todo (Debug.toString e), Cmd.none )

        Tick ->
            case model.vis of
                Just vis ->
                    ( { model | vis = Just (Vis.update vis) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


fetchArtistsSeries : String -> List Spotify.Artist -> Cmd Msg
fetchArtistsSeries accessToken artists =
    case artists of
        artist :: more ->
            Spotify.getRelatedArtists accessToken
                (GotRelatedArtists artist.id more)
                artist

        [] ->
            Cmd.none


nub : List a -> List a
nub xs =
    List.foldr
        (\a acc ->
            if List.member a acc then
                acc

            else
                a :: acc
        )
        []
        xs


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrame (always Tick)


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    UrlRequest


onUrlChange : Url -> Msg
onUrlChange _ =
    UrlChange
