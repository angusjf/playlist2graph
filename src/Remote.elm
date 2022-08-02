module Remote exposing (..)

import Http


type Remote a
    = Fetching
    | Done a
    | Partial a
    | Error Http.Error


withDefault : a -> Remote a -> a
withDefault default remote =
    case remote of
        Fetching ->
            default

        Partial a ->
            a

        Done a ->
            a

        Error _ ->
            default


done : Remote a -> Remote a
done remote =
    case remote of
        Fetching ->
            Fetching

        Partial a ->
            Done a

        Done a ->
            Done a

        Error x ->
            Error x
