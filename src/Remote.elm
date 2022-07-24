module Remote exposing (..)

import Http


type Remote a
    = Fetching
    | Done a
    | Error Http.Error


withDefault : a -> Remote a -> a
withDefault default remote =
    case remote of
        Fetching ->
            default

        Done a ->
            a

        Error _ ->
            default
