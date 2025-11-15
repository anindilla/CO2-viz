module Insights exposing
    ( Doc
    , Highlight
    , Payload
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Doc =
    { generatedAt : String
    , model : String
    , status : String
    , insights : Payload
    }


type alias Payload =
    { highlights : List Highlight
    , narrative : String
    , actions : List String
    , questions : List String
    }


type alias Highlight =
    { title : String
    , detail : String
    , evidence : Maybe String
    }


decoder : Decoder Doc
decoder =
    Decode.succeed Doc
        |> required "generatedAt" Decode.string
        |> required "model" Decode.string
        |> required "status" Decode.string
        |> required "insights" insightsDecoder


insightsDecoder : Decoder Payload
insightsDecoder =
    Decode.succeed Payload
        |> required "highlights" (Decode.list highlightDecoder)
        |> required "narrative" Decode.string
        |> required "actions" (Decode.list Decode.string)
        |> required "questions" (Decode.list Decode.string)


highlightDecoder : Decoder Highlight
highlightDecoder =
    Decode.succeed Highlight
        |> required "title" Decode.string
        |> required "detail" Decode.string
        |> required "evidence" (Decode.nullable Decode.string)

