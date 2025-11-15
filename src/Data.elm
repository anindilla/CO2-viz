module Data exposing
    ( Dashboard
    , Series
    , Country
    , Snapshot
    , EmittersForYear
    , Emitter
    , Analytics
    , RankingRow
    , Growth
    , GrowthPoint
    , TopSeries
    , MapData
    , MapEntry
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Dashboard =
    { version : Int
    , generatedAt : String
    , global : Series
    , countries : List Country
    , emitters : List EmittersForYear
    , analytics : Analytics
    }


type alias Series =
    { years : List Int
    , co2 : List (Maybe Float)
    , perCapita : List (Maybe Float)
    , share : List (Maybe Float)
    , population : List (Maybe Float)
    , gdp : List (Maybe Float)
    }


type alias Country =
    { iso : String
    , name : String
    , series : Series
    , latest : Snapshot
    }


type alias Snapshot =
    { year : Int
    , co2 : Maybe Float
    , perCapita : Maybe Float
    , share : Maybe Float
    , population : Maybe Float
    , gdp : Maybe Float
    }


type alias EmittersForYear =
    { year : Int
    , items : List Emitter
    }


type alias Emitter =
    { iso : String
    , name : String
    , co2 : Float
    , share : Maybe Float
    }


type alias Analytics =
    { ranking : List RankingRow
    , growth : Growth
    , map : MapData
    }


type alias RankingRow =
    { iso : String
    , name : String
    , year : Int
    , total : Float
    , yoy : Maybe Float
    , perCapita : Maybe Float
    , share : Maybe Float
    , population : Maybe Float
    }


type alias Growth =
    { global : List GrowthPoint
    , topEmitters : List TopSeries
    }


type alias GrowthPoint =
    { year : Int
    , value : Float
    }


type alias TopSeries =
    { iso : String
    , name : String
    , points : List GrowthPoint
    }


type alias MapData =
    { bins : List Float
    , entries : List MapEntry
    }


type alias MapEntry =
    { iso : String
    , name : String
    , value : Float
    }


decoder : Decoder Dashboard
decoder =
    Decode.succeed Dashboard
        |> required "version" Decode.int
        |> required "generatedAt" Decode.string
        |> required "global" seriesDecoder
        |> required "countries" (Decode.list countryDecoder)
        |> required "emitters" (Decode.list emittersDecoder)
        |> required "analytics" analyticsDecoder


seriesDecoder : Decoder Series
seriesDecoder =
    Decode.succeed Series
        |> required "years" (Decode.list Decode.int)
        |> required "co2" (Decode.list maybeFloat)
        |> required "perCapita" (Decode.list maybeFloat)
        |> required "share" (Decode.list maybeFloat)
        |> required "population" (Decode.list maybeFloat)
        |> required "gdp" (Decode.list maybeFloat)


countryDecoder : Decoder Country
countryDecoder =
    Decode.succeed Country
        |> required "iso" Decode.string
        |> required "name" Decode.string
        |> required "series" seriesDecoder
        |> required "latest" snapshotDecoder


snapshotDecoder : Decoder Snapshot
snapshotDecoder =
    Decode.succeed Snapshot
        |> required "year" Decode.int
        |> required "co2" maybeFloat
        |> required "perCapita" maybeFloat
        |> required "share" maybeFloat
        |> required "population" maybeFloat
        |> required "gdp" maybeFloat


emittersDecoder : Decoder EmittersForYear
emittersDecoder =
    Decode.succeed EmittersForYear
        |> required "year" Decode.int
        |> required "items" (Decode.list emitterDecoder)


emitterDecoder : Decoder Emitter
emitterDecoder =
    Decode.succeed Emitter
        |> required "iso" Decode.string
        |> required "name" Decode.string
        |> required "co2" Decode.float
        |> required "share" maybeFloat


analyticsDecoder : Decoder Analytics
analyticsDecoder =
    Decode.succeed Analytics
        |> required "ranking" (Decode.list rankingDecoder)
        |> required "growth" growthDecoder
        |> required "map" mapDecoder


rankingDecoder : Decoder RankingRow
rankingDecoder =
    Decode.succeed RankingRow
        |> required "iso" Decode.string
        |> required "name" Decode.string
        |> required "year" Decode.int
        |> required "total" Decode.float
        |> required "yoy" maybeFloat
        |> required "perCapita" maybeFloat
        |> required "share" maybeFloat
        |> required "population" maybeFloat


growthDecoder : Decoder Growth
growthDecoder =
    Decode.succeed Growth
        |> required "global" (Decode.list growthPointDecoder)
        |> required "topEmitters" (Decode.list topSeriesDecoder)


growthPointDecoder : Decoder GrowthPoint
growthPointDecoder =
    Decode.succeed GrowthPoint
        |> required "year" Decode.int
        |> required "value" Decode.float


topSeriesDecoder : Decoder TopSeries
topSeriesDecoder =
    Decode.succeed TopSeries
        |> required "iso" Decode.string
        |> required "name" Decode.string
        |> required "points" (Decode.list growthPointDecoder)


mapDecoder : Decoder MapData
mapDecoder =
    Decode.succeed MapData
        |> required "bins" (Decode.list Decode.float)
        |> required "entries" (Decode.list mapEntryDecoder)


mapEntryDecoder : Decoder MapEntry
mapEntryDecoder =
    Decode.succeed MapEntry
        |> required "iso" Decode.string
        |> required "name" Decode.string
        |> required "value" Decode.float


maybeFloat : Decoder (Maybe Float)
maybeFloat =
    Decode.nullable Decode.float

