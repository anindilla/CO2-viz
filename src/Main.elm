module Main exposing (main)

import Browser
import Data
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import List.Extra as ListExtra
import Svg exposing (..)
import Svg.Attributes as SvgAttr


-- MODEL


type Remote data
    = Loading
    | Failure String
    | Loaded data


type alias Model =
    { remote : Remote Data.Dashboard
    , focusIso : Maybe String
    , compareIso : Maybe String
    , selectedYear : Maybe Int
    }


type Msg
    = GotDashboard (Result Http.Error Data.Dashboard)
    | SelectFocus String
    | SelectCompare String
    | SelectYear String


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { remote = Loading, focusIso = Nothing, compareIso = Nothing, selectedYear = Nothing }, fetchDashboard )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


fetchDashboard : Cmd Msg
fetchDashboard =
    Http.get
        { url = "/data/dashboard.json"
        , expect = Http.expectJson GotDashboard Data.decoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDashboard (Ok dashboard) ->
            let
                defaultFocus =
                    dashboard.countries |> List.head |> Maybe.map .iso

                defaultCompare =
                    dashboard.countries
                        |> List.filter (\country -> Just country.iso /= defaultFocus)
                        |> List.head
                        |> Maybe.map .iso

                defaultYear =
                    dashboard.emitters
                        |> ListExtra.last
                        |> Maybe.map .year
            in
            ( { model
                | remote = Loaded dashboard
                , focusIso = defaultFocus
                , compareIso = defaultCompare
                , selectedYear = defaultYear
              }
            , Cmd.none
            )

        GotDashboard (Err err) ->
            ( { model | remote = Failure (httpErrorToString err) }, Cmd.none )

        SelectFocus iso ->
            ( { model | focusIso = Just iso }, Cmd.none )

        SelectCompare iso ->
            ( { model | compareIso = Just iso }, Cmd.none )

        SelectYear value ->
            case String.toInt value of
                Just yr ->
                    ( { model | selectedYear = Just yr }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl _ ->
            "The dashboard URL looks incorrect."

        Http.Timeout ->
            "The request timed out. Please retry."

        Http.NetworkError ->
            "Network error when fetching dashboard data."

        Http.BadStatus status ->
            "Server responded with HTTP " ++ String.fromInt status

        Http.BadBody msg ->
            "Could not decode dashboard JSON (" ++ msg ++ ")."


-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.remote of
        Loading ->
            viewDocument "Carbon Pulse" [ viewLoading ]

        Failure message ->
            viewDocument "Carbon Pulse" [ viewError message ]

        Loaded dashboard ->
            viewDocument "Carbon Pulse · Elm CO₂ Explorer"
                [ viewDashboard dashboard model ]


viewDocument : String -> List (Html Msg) -> Browser.Document Msg
viewDocument title nodes =
    { title = title
    , body = nodes
    }


viewLoading : Html Msg
viewLoading =
    Html.main_
        [ Attr.class "layout" ]
        [ Html.section [ Attr.class "panel skeleton" ] [ Html.div [ Attr.class "skeleton-bar" ] [] ]
        , Html.section [ Attr.class "panel skeleton" ] [ Html.div [ Attr.class "skeleton-bar" ] [] ]
        , Html.section [ Attr.class "panel skeleton" ] [ Html.div [ Attr.class "skeleton-bar" ] [] ]
        ]


viewError : String -> Html Msg
viewError message =
    Html.main_
        [ Attr.class "layout" ]
        [ Html.section [ Attr.class "panel error" ]
            [ Html.h2 [] [ Html.text "Something went wrong" ]
            , Html.p [] [ Html.text message ]
            ]
        ]


viewDashboard : Data.Dashboard -> Model -> Html Msg
viewDashboard dashboard model =
    let
        countryDict =
            dashboard.countries
                |> List.foldl (\country acc -> Dict.insert country.iso country acc) Dict.empty

        focusIso =
            model.focusIso
                |> Maybe.withDefault (dashboard.countries |> List.head |> Maybe.map .iso |> Maybe.withDefault "")

        compareIso =
            model.compareIso
                |> Maybe.withDefault
                    (dashboard.countries
                        |> List.filter (\country -> country.iso /= focusIso)
                        |> List.head
                        |> Maybe.map .iso
                        |> Maybe.withDefault focusIso
                    )

        selectedYear =
            model.selectedYear
                |> Maybe.withDefault
                    (dashboard.emitters
                        |> ListExtra.last
                        |> Maybe.map .year
                        |> Maybe.withDefault (dashboard.global.years |> List.reverse |> List.head |> Maybe.withDefault 0)
                    )

        focusCountry =
            Dict.get focusIso countryDict

        compareCountry =
            Dict.get compareIso countryDict
    in
    Html.main_
        [ Attr.class "layout" ]
        [ viewHero dashboard
        , viewGlobalChart dashboard
        , viewCountryComparison dashboard focusIso compareIso focusCountry compareCountry
        , viewEmitters dashboard selectedYear
        ]


viewHero : Data.Dashboard -> Html Msg
viewHero dashboard =
    let
        totals =
            latestValue dashboard.global.years dashboard.global.co2

        perCapita =
            latestValue dashboard.global.years dashboard.global.perCapita

        heroYear =
            totals |> Maybe.map Tuple.first |> Maybe.withDefault 0
    in
    Html.section [ Attr.class "panel hero" ]
        [ Html.div []
            [ Html.p [ Attr.class "eyebrow" ] [ Html.text ("Updated " ++ humanizeDate dashboard.generatedAt) ]
            , Html.h1 [] [ Html.text "Carbon Pulse" ]
            , Html.p []
                [ Html.text "Lightweight Elm dashboard for exploring global and national CO₂ trends from 1900 onwards." ]
            ]
        , Html.div [ Attr.class "metric-row" ]
            [ viewMetric "Global CO₂ (Mt)" (formatMetric totals)
            , viewMetric "Per Capita (t)" (formatMetric perCapita)
            , viewMetric "Countries" (String.fromInt (List.length dashboard.countries))
            , viewMetric "Latest Year" (String.fromInt heroYear)
            ]
        ]


viewMetric : String -> String -> Html Msg
viewMetric label value =
    Html.div [ Attr.class "metric" ]
        [ Html.span [] [ Html.text label ]
        , Html.strong [] [ Html.text value ]
        ]


viewGlobalChart : Data.Dashboard -> Html Msg
viewGlobalChart dashboard =
    let
        points =
            seriesToPoints dashboard.global
                |> List.filterMap
                    (\p ->
                        p.co2 |> Maybe.map (\value -> ( toFloat p.year, value ))
                    )
                |> takeRecent 160
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Global momentum" ]
            , Html.span [] [ Html.text "1900 – present" ]
            ]
        , lineChart
            { width = 900
            , height = 260
            , color = "#4f46e5"
            , fill = "rgba(79,70,229,0.15)"
            }
            points
        ]


viewCountryComparison :
    Data.Dashboard
    -> String
    -> String
    -> Maybe Data.Country
    -> Maybe Data.Country
    -> Html Msg
viewCountryComparison dashboard focusIso compareIso focusCountry compareCountry =
    let
        options =
            dashboard.countries
                |> List.map (\country -> ( country.iso, country.name ))
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Country comparison" ]
            , Html.span [] [ Html.text "Pick any two countries" ]
            ]
        , Html.div [ Attr.class "country-grid" ]
            [ viewCountryPanel "Focus" focusIso options focusCountry SelectFocus
            , viewCountryPanel "Compare" compareIso options compareCountry SelectCompare
            ]
        ]


viewCountryPanel :
    String
    -> String
    -> List ( String, String )
    -> Maybe Data.Country
    -> (String -> Msg)
    -> Html Msg
viewCountryPanel title selectedIso options maybeCountry onChangeMsg =
    let
        selectNode =
            Html.select
                [ Attr.value selectedIso
                , Events.onInput onChangeMsg
                ]
                (options
                    |> List.map (\( iso, name ) -> Html.option [ Attr.value iso ] [ Html.text name ])
                )
    in
    Html.div [ Attr.class "country-panel" ]
        [ Html.div [ Attr.class "country-panel__header" ]
            [ Html.span [ Attr.class "eyebrow" ] [ Html.text title ]
            , selectNode
            ]
        , maybeCountry
            |> Maybe.map viewCountryDetails
            |> Maybe.withDefault (Html.p [ Attr.class "muted" ] [ Html.text "No data for this selection." ])
        ]


viewCountryDetails : Data.Country -> Html Msg
viewCountryDetails country =
    let
        co2Metric =
            country.latest.co2
                |> Maybe.map (formatNumber 1)
                |> Maybe.withDefault "–"

        perCapitaMetric =
            country.latest.perCapita
                |> Maybe.map (formatNumber 2)
                |> Maybe.withDefault "–"

        shareMetric =
            country.latest.share
                |> Maybe.map (\val -> formatNumber 2 val ++ "%")
                |> Maybe.withDefault "–"

        populationMetric =
            country.latest.population
                |> Maybe.map (\val -> formatPopulation val)
                |> Maybe.withDefault "–"

        sparkPoints =
            seriesToPoints country.series
                |> List.filterMap
                    (\point ->
                        point.perCapita
                            |> Maybe.map (\value -> ( toFloat point.year, value ))
                    )
                |> takeRecent 80
    in
    Html.div []
        [ Html.div [ Attr.class "comparison-metrics" ]
            [ viewMetric "CO₂ (Mt)" co2Metric
            , viewMetric "Per Capita (t)" perCapitaMetric
            , viewMetric "Share (%)" shareMetric
            , viewMetric "Population" populationMetric
            ]
        , lineChart
            { width = 420
            , height = 140
            , color = "#0ea5e9"
            , fill = "rgba(14,165,233,0.15)"
            }
            sparkPoints
        ]


viewEmitters : Data.Dashboard -> Int -> Html Msg
viewEmitters dashboard selectedYear =
    let
        emittersYears =
            dashboard.emitters |> List.map .year

        minYear =
            emittersYears |> List.minimum |> Maybe.withDefault selectedYear

        maxYear =
            emittersYears |> List.maximum |> Maybe.withDefault selectedYear

        selected =
            dashboard.emitters
                |> List.filter (\entry -> entry.year == selectedYear)
                |> List.head
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Top emitters" ]
            , Html.span [] [ Html.text ("Year " ++ String.fromInt selectedYear) ]
            ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min (String.fromInt minYear)
            , Attr.max (String.fromInt maxYear)
            , Attr.value (String.fromInt selectedYear)
            , Attr.step "1"
            , Events.onInput SelectYear
            ]
            []
        , selected
            |> Maybe.map viewEmitterList
            |> Maybe.withDefault (Html.p [ Attr.class "muted" ] [ Html.text "No data for the selected year." ])
        ]


viewEmitterList : Data.EmittersForYear -> Html Msg
viewEmitterList entry =
    Html.ul [ Attr.class "emitters" ]
        (entry.items
            |> List.indexedMap
                (\idx emitter ->
                    let
                        pct =
                            emitter.share
                                |> Maybe.map (\val -> clamp 0 100 val)
                                |> Maybe.withDefault 0
                    in
                    Html.li []
                        [ Html.div []
                            [ Html.span [ Attr.class "rank" ] [ Html.text (String.fromInt (idx + 1)) ]
                            , Html.div [ Attr.class "emitter-info" ]
                                [ Html.strong [] [ Html.text emitter.name ]
                                , Html.span [] [ Html.text (formatNumber 1 emitter.co2 ++ " Mt") ]
                                ]
                            ]
                        , Html.div [ Attr.class "bar-track" ]
                            [ Html.span
                                [ Attr.class "bar-fill"
                                , Attr.style "width" (String.fromFloat pct ++ "%")
                                ]
                                []
                            ]
                        ]
                )
        )


-- HELPERS


seriesToPoints :
    Data.Series
    -> List
            { year : Int
            , co2 : Maybe Float
            , perCapita : Maybe Float
            , share : Maybe Float
            }
seriesToPoints series =
    List.map4
        (\year co2 perCapita share ->
            { year = year
            , co2 = co2
            , perCapita = perCapita
            , share = share
            }
        )
        series.years
        series.co2
        series.perCapita
        series.share


takeRecent : Int -> List a -> List a
takeRecent count list =
    let
        len =
            List.length list
    in
    if len <= count then
        list

    else
        List.drop (len - count) list


latestValue : List Int -> List (Maybe Float) -> Maybe ( Int, Float )
latestValue years values =
    List.map2
        (\year maybeVal ->
            Maybe.map (\val -> ( year, val )) maybeVal
        )
        years
        values
        |> List.filterMap identity
        |> ListExtra.last


formatNumber : Int -> Float -> String
formatNumber decimals value =
    let
        factor =
            10 ^ decimals
    in
    ((toFloat (round (value * toFloat factor))) / toFloat factor)
        |> String.fromFloat


formatMetric : Maybe ( Int, Float ) -> String
formatMetric maybeTuple =
    maybeTuple
        |> Maybe.map Tuple.second
        |> Maybe.map (formatNumber 1)
        |> Maybe.withDefault "–"


formatPopulation : Float -> String
formatPopulation value =
    if value >= 1000000 then
        formatNumber 1 (value / 1000000) ++ " M"

    else if value >= 1000 then
        formatNumber 1 (value / 1000) ++ " K"

    else
        formatNumber 0 value


humanizeDate : String -> String
humanizeDate isoString =
    isoString
        |> String.left 10


lineChart :
    { width : Float
    , height : Float
    , color : String
    , fill : String
    }
    -> List ( Float, Float )
    -> Html msg
lineChart config points =
    case points of
        [] ->
            Html.p [ Attr.class "muted" ] [ Html.text "Not enough data to render this chart." ]

        _ ->
            let
                minX =
                    points |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0

                maxX =
                    points |> List.map Tuple.first |> List.maximum |> Maybe.withDefault (minX + 1)

                minY =
                    points |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0

                maxY =
                    points |> List.map Tuple.second |> List.maximum |> Maybe.withDefault (minY + 1)

                margin =
                    16

                width =
                    config.width

                height =
                    config.height

                scaleX value =
                    margin + ((value - minX) / (maxX - minX)) * (width - (2 * margin))

                scaleY value =
                    height - margin - ((value - minY) / (maxY - minY)) * (height - (2 * margin))

                scaled =
                    points |> List.map (\( x, y ) -> ( scaleX x, scaleY y ))

                pathData =
                    toPath scaled
            in
            Svg.svg
                [ SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
                , Attr.class "chart"
                ]
                [ Svg.path
                    [ SvgAttr.d (pathData ++ " L " ++ String.fromFloat (scaleX maxX) ++ " " ++ String.fromFloat (height - margin) ++ " L " ++ String.fromFloat (scaleX minX) ++ " " ++ String.fromFloat (height - margin) ++ " Z")
                    , SvgAttr.fill config.fill
                    , SvgAttr.stroke "none"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d pathData
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke config.color
                    , SvgAttr.strokeWidth "3"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []
                ]


toPath : List ( Float, Float ) -> String
toPath points =
    case points of
        [] ->
            ""

        ( x0, y0 ) :: rest ->
            let
                start =
                    "M " ++ String.fromFloat x0 ++ " " ++ String.fromFloat y0

                tail =
                    rest
                        |> List.map (\( x, y ) -> "L " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                        |> String.join " "
            in
            start ++ " " ++ tail

