port module Main exposing (main)

import Browser
import Data
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Encode as Encode
import List.Extra as ListExtra
import String
import Svg exposing (..)
import Svg.Attributes as SvgAttr


-- MODEL


type TrendMode
    = Total
    | PerCapita


type EmitterView
    = TopCountries
    | IncomeGroups


type alias Model =
    { remote : Remote Data.Dashboard
    , focusIso : Maybe String
    , compareIso : Maybe String
    , selectedYear : Int
    , trendMode : TrendMode
    , emitterView : EmitterView
    }


type Remote data
    = Loading
    | Failure String
    | Loaded data


type Msg
    = GotDashboard (Result Http.Error Data.Dashboard)
    | SelectFocus String
    | SelectCompare String
    | ChangeYear String
    | SetTrend TrendMode
    | SetEmitterView EmitterView


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initModel, fetchDashboard )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


initModel : Model
initModel =
    { remote = Loading
    , focusIso = Nothing
    , compareIso = Nothing
    , selectedYear = 2021
    , trendMode = Total
    , emitterView = TopCountries
    }


fetchDashboard : Cmd Msg
fetchDashboard =
    Http.get
        { url = "/data/dashboard.json"
        , expect = Http.expectJson GotDashboard Data.decoder
        }


port mapUpdate : Encode.Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDashboard (Ok dashboard) ->
            let
                latestYear =
                    dashboard.global.years
                        |> List.maximum
                        |> Maybe.withDefault model.selectedYear

                focusIso =
                    dashboard.countries
                        |> List.head
                        |> Maybe.map .iso

                compareIso =
                    dashboard.countries
                        |> List.filter (\country -> Just country.iso /= focusIso)
                        |> List.head
                        |> Maybe.map .iso

                updatedModel =
                    { model
                        | remote = Loaded dashboard
                        , focusIso = focusIso
                        , compareIso = compareIso
                        , selectedYear = latestYear
                    }
            in
            ( updatedModel, mapUpdate (encodeMapPayload dashboard latestYear) )

        GotDashboard (Err err) ->
            ( { model | remote = Failure (httpErrorToString err) }, Cmd.none )

        SelectFocus iso ->
            ( { model | focusIso = Just iso }, Cmd.none )

        SelectCompare iso ->
            ( { model | compareIso = Just iso }, Cmd.none )

        ChangeYear value ->
            case ( String.toInt value, model.remote ) of
                ( Just year, Loaded dashboard ) ->
                    let
                        clamped = clampYear dashboard year
                    in
                    ( { model | selectedYear = clamped }, mapUpdate (encodeMapPayload dashboard clamped) )

                _ ->
                    ( model, Cmd.none )

        SetTrend mode ->
            ( { model | trendMode = mode }, Cmd.none )

        SetEmitterView viewMode ->
            ( { model | emitterView = viewMode }, Cmd.none )


clampYear : Data.Dashboard -> Int -> Int
clampYear dashboard value =
    let
        minYear =
            dashboard.global.years |> List.minimum |> Maybe.withDefault value

        maxYear =
            dashboard.global.years |> List.maximum |> Maybe.withDefault value
    in
    value |> clamp minYear maxYear


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
        year = model.selectedYear

        topCountries =
            topCountriesByCo2 year 12 dashboard
    in
    Html.main_
        [ Attr.class "layout" ]
        [ viewHero dashboard year
        , viewGlobalTimeline dashboard model.trendMode
        , viewMapSection dashboard year topCountries
        , viewComposition dashboard year model.emitterView topCountries
        , viewPareto year topCountries
        , viewScatter dashboard year
        , viewCountryComparison dashboard model year
        ]


viewHero : Data.Dashboard -> Int -> Html Msg
viewHero dashboard year =
    let
        latestGlobal =
            globalPointForYear year dashboard.global

        total =
            latestGlobal |> Maybe.andThen .co2 |> Maybe.map (formatNumber 1)

        perCapita =
            latestGlobal |> Maybe.andThen .co2PerCapita |> Maybe.map (formatNumber 2)

        countryCount =
            String.fromInt (List.length dashboard.countries)
    in
    Html.section [ Attr.class "panel hero" ]
        [ Html.div []
            [ Html.p [ Attr.class "eyebrow" ] [ Html.text ("Updated " ++ humanizeDate dashboard.generatedAt) ]
            , Html.h1 [] [ Html.text "Carbon Pulse" ]
            , Html.p []
                [ Html.text "Lightweight Elm dashboard for exploring global and national CO₂ stories." ]
            ]
        , Html.div [ Attr.class "metric-row" ]
            [ viewMetric "Global CO₂ (Mt)" (Maybe.withDefault "–" total)
            , viewMetric "Per capita (t)" (Maybe.withDefault "–" perCapita)
            , viewMetric "Countries" countryCount
            , viewMetric "Active year" (String.fromInt year)
            ]
        , Html.div [ Attr.class "year-slider" ]
            [ Html.label [] [ Html.text "Choose year" ]
            , Html.input
                [ Attr.type_ "range"
                , Attr.min (String.fromInt (sliderMinYear dashboard))
                , Attr.max (String.fromInt (sliderMaxYear dashboard))
                , Attr.value (String.fromInt year)
                , Attr.step "1"
                , Events.onInput ChangeYear
                ]
                []
            ]
        ]


viewGlobalTimeline : Data.Dashboard -> TrendMode -> Html Msg
viewGlobalTimeline dashboard trendMode =
    let
        series =
            seriesToPoints dashboard.global
                |> List.filterMap
                    (\point ->
                        case trendMode of
                            Total ->
                                point.co2 |> Maybe.map (\value -> ( toFloat point.year, value ))

                            PerCapita ->
                                point.co2PerCapita |> Maybe.map (\value -> ( toFloat point.year, value ))
                    )
                |> takeRecent 160

        titleSuffix =
            case trendMode of
                Total ->
                    "Mt" 

                PerCapita ->
                    "t per capita"
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.div []
                [ Html.h2 [] [ Html.text "Global momentum" ]
                , Html.span [] [ Html.text ("1900 – present · " ++ titleSuffix) ]
                ]
            , Html.div [ Attr.class "toggle-group" ]
                [ viewToggle trendMode Total "Total"
                , viewToggle trendMode PerCapita "Per capita"
                ]
            ]
        , lineChart
            { width = 900
            , height = 260
            , color = "#2563eb"
            , fill = "rgba(37,99,235,0.15)"
            }
            series
        ]


viewToggle : TrendMode -> TrendMode -> String -> Html Msg
viewToggle current target label =
    Html.button
        [ Attr.class ("toggle " ++ if current == target then "toggle--active" else "")
        , Events.onClick (SetTrend target)
        ]
        [ Html.text label ]


viewMapSection : Data.Dashboard -> Int -> List CountrySnapshot -> Html Msg
viewMapSection dashboard year countries =
    Html.section [ Attr.class "panel map-panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Map of major emitters" ]
            , Html.span [] [ Html.text ("Highlighting top countries · " ++ String.fromInt year) ]
            ]
        , Html.div [ Attr.class "map-panel__body" ]
            [ Html.div [ Attr.id "emissions-map", Attr.class "map-panel__map" ] []
            , Html.div [ Attr.class "map-panel__legend" ]
                (countries
                    |> List.take 6
                    |> List.map (
                        \snapshot ->
                            Html.div [ Attr.class "map-panel__legend-row" ]
                                [ Html.span [] [ Html.text snapshot.name ]
                                , Html.span [] [ Html.text (formatNumber 1 snapshot.co2 ++ " Mt") ]
                                ]
                       )
                )
            ]
        ]


viewComposition : Data.Dashboard -> Int -> EmitterView -> List CountrySnapshot -> Html Msg
viewComposition dashboard year emitterView countries =
    let
        ( label, dataset ) =
            case emitterView of
                TopCountries ->
                    ( "Top countries"
                    , countries
                        |> List.take 8
                        |> List.map (\c -> ( c.name, c.co2 ))
                    )

                IncomeGroups ->
                    ( "Income groups"
                    , incomeGroupSeries dashboard year
                    )
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Composition" ]
            , Html.div [ Attr.class "toggle-group" ]
                [ viewEmitterToggle emitterView TopCountries "Countries"
                , viewEmitterToggle emitterView IncomeGroups "Income"
                ]
            ]
        , Html.p [ Attr.class "muted" ] [ Html.text (label ++ " ordered by total CO₂ in " ++ String.fromInt year) ]
        , barList dataset
        ]


viewEmitterToggle : EmitterView -> EmitterView -> String -> Html Msg
viewEmitterToggle current viewMode label =
    Html.button
        [ Attr.class ("toggle " ++ if current == viewMode then "toggle--active" else "")
        , Events.onClick (SetEmitterView viewMode)
        ]
        [ Html.text label ]


viewPareto : Int -> List CountrySnapshot -> Html Msg
viewPareto year countries =
    let
        globalShare =
            cumulativeShare countries
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "How concentrated are emissions?" ]
            , Html.span [] [ Html.text ("Top " ++ String.fromInt (List.length globalShare) ++ " countries") ]
            ]
        , Html.div [ Attr.class "pareto" ]
            (globalShare
                |> List.map (
                    \entry ->
                        Html.div [ Attr.class "pareto-row" ]
                            [ Html.span [] [ Html.text (Tuple.first entry) ]
                            , Html.div [ Attr.class "pareto-bar" ]
                                [ Html.div
                                    [ Attr.style "width" (String.fromFloat (Tuple.second entry) ++ "%")
                                    ]
                                    []
                                ]
                            , Html.span [] [ Html.text (formatNumber 1 (Tuple.second entry) ++ "%") ]
                            ]
                   )
            )
        ]


viewScatter : Data.Dashboard -> Int -> Html Msg
viewScatter dashboard year =
    let
        points = scatterPoints dashboard year

        ( minX, maxX ) = scatterDomain .gdpPerCapita points
        ( minY, maxY ) = scatterDomain .co2PerCapita points

        width = 520
        height = 320
        padding = 48

        scaleX value =
            if maxX == minX then
                padding
            else
                padding + ((value - minX) / (maxX - minX)) * (toFloat width - (2 * padding))

        scaleY value =
            if maxY == minY then
                toFloat height - padding
            else
                toFloat height - padding - ((value - minY) / (maxY - minY)) * (toFloat height - (2 * padding))
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "GDP vs. CO₂ intensity" ]
            , Html.span [] [ Html.text (String.fromInt year ++ " · GDP per capita vs CO₂ per capita") ]
            ]
        , Svg.svg
            [ SvgAttr.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
            , Attr.class "scatter"
            ]
            ([ Svg.line
                [ SvgAttr.x1 (String.fromFloat padding)
                , SvgAttr.y1 (String.fromFloat (toFloat height - padding))
                , SvgAttr.x2 (String.fromFloat (toFloat width - padding))
                , SvgAttr.y2 (String.fromFloat (toFloat height - padding))
                , SvgAttr.stroke "#cbd5f5"
                , SvgAttr.strokeWidth "1"
                ]
                []
            , Svg.line
                [ SvgAttr.x1 (String.fromFloat padding)
                , SvgAttr.y1 (String.fromFloat padding)
                , SvgAttr.x2 (String.fromFloat padding)
                , SvgAttr.y2 (String.fromFloat (toFloat height - padding))
                , SvgAttr.stroke "#cbd5f5"
                , SvgAttr.strokeWidth "1"
                ]
                []
            ]
                ++ (points
                        |> List.map
                            (\point ->
                                Svg.circle
                                    [ SvgAttr.cx (String.fromFloat (scaleX point.gdpPerCapita))
                                    , SvgAttr.cy (String.fromFloat (scaleY point.co2PerCapita))
                                    , SvgAttr.r (String.fromFloat (circleRadius point.population))
                                    , SvgAttr.fill "rgba(79,70,229,0.45)"
                                    ]
                                    [ Svg.title [] [ Svg.text (point.name ++ " · " ++ formatNumber 2 point.co2PerCapita ++ " t/person") ] ]
                            )
                   )
            )
        ]


viewCountryComparison : Data.Dashboard -> Model -> Int -> Html Msg
viewCountryComparison dashboard model year =
    let
        options =
            dashboard.countries |> List.map (\country -> ( country.iso, country.name ))

        focusCountry =
            model.focusIso |> Maybe.andThen (\iso -> Dict.get iso (countryDict dashboard))

        compareCountry =
            model.compareIso |> Maybe.andThen (\iso -> Dict.get iso (countryDict dashboard))
    in
    Html.section [ Attr.class "panel" ]
        [ Html.div [ Attr.class "section-header" ]
            [ Html.h2 [] [ Html.text "Country comparison" ]
            , Html.span [] [ Html.text "Select any two countries" ]
            ]
        , Html.div [ Attr.class "country-grid" ]
            [ viewCountryPanel "Focus" model.focusIso options focusCountry year SelectFocus
            , viewCountryPanel "Compare" model.compareIso options compareCountry year SelectCompare
            ]
        ]


viewCountryPanel : String -> Maybe String -> List ( String, String ) -> Maybe Data.Country -> Int -> (String -> Msg) -> Html Msg
viewCountryPanel label currentIso options maybeCountry year onSelectMsg =
    let
        selectNode =
            Html.select
                [ Attr.value (Maybe.withDefault "" currentIso)
                , Events.onInput onSelectMsg
                ]
                (options |> List.map (\( iso, name ) -> Html.option [ Attr.value iso ] [ Html.text name ]))
    in
    Html.div [ Attr.class "country-panel" ]
        [ Html.div [ Attr.class "country-panel__header" ]
            [ Html.span [ Attr.class "eyebrow" ] [ Html.text label ]
            , selectNode
            ]
        , case maybeCountry of
            Nothing ->
                Html.p [ Attr.class "muted" ] [ Html.text "No data for this selection." ]

            Just country ->
                let
                    point =
                        countryPointForYear year country

                    latestCo2 =
                        point |> Maybe.andThen .co2 |> Maybe.map (formatNumber 2) |> Maybe.withDefault "–"

                    perCapita =
                        point |> Maybe.andThen .co2PerCapita |> Maybe.map (formatNumber 2) |> Maybe.withDefault "–"

                    share =
                        point |> Maybe.andThen .share |> Maybe.map (\value -> formatNumber 2 value ++ "%") |> Maybe.withDefault "–"

                    populationText =
                        point |> Maybe.andThen .population |> Maybe.map formatPopulation |> Maybe.withDefault "–"

                    sparkPoints =
                        seriesToPoints country.series
                            |> List.filterMap (\p -> p.co2 |> Maybe.map (\value -> ( toFloat p.year, value )))
                            |> takeRecent 80
                in
                Html.div []
                    [ Html.div [ Attr.class "comparison-metrics" ]
                        [ viewMetric "CO₂ (Mt)" latestCo2
                        , viewMetric "Per capita (t)" perCapita
                        , viewMetric "Share (%)" share
                        , viewMetric "Population" populationText
                        ]
                    , lineChart
                        { width = 420
                        , height = 140
                        , color = "#0ea5e9"
                        , fill = "rgba(14,165,233,0.15)"
                        }
                        sparkPoints
                    ]
        ]


viewMetric : String -> String -> Html Msg
viewMetric label value =
    Html.div [ Attr.class "metric" ]
        [ Html.span [] [ Html.text label ]
        , Html.strong [] [ Html.text value ]
        ]


barList : List ( String, Float ) -> Html Msg
barList dataset =
    Html.div [ Attr.class "bar-list" ]
        (dataset
            |> List.map (
                \( label, value ) ->
                    let
                        width =
                            String.fromFloat (normalizeValue dataset value) ++ "%"
                    in
                    Html.div [ Attr.class "bar-row" ]
                        [ Html.span [ Attr.class "bar-row__label" ] [ Html.text label ]
                        , Html.div [ Attr.class "bar-row__track" ]
                            [ Html.div [ Attr.class "bar-row__value", Attr.style "width" width ] []
                            , Html.span [] [ Html.text (formatNumber 1 value ++ " Mt") ]
                            ]
                        ]
               )
        )


normalizeValue : List ( String, Float ) -> Float -> Float
normalizeValue dataset value =
    let
        maxValue =
            dataset |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 1
    in
    if maxValue == 0 then
        0
    else
        (value / maxValue) * 100


sortByDesc : (a -> comparable) -> List a -> List a
sortByDesc accessor list =
    List.sortWith (\a b -> compare (accessor b) (accessor a)) list


-- DATA HELPERS


type alias CountrySnapshot =
    { iso : String
    , name : String
    , co2 : Float
    , perCapita : Maybe Float
    , share : Maybe Float
    , population : Maybe Float
    }


countryDict : Data.Dashboard -> Dict String Data.Country
countryDict dashboard =
    dashboard.countries
        |> List.map (\country -> ( country.iso, country ))
        |> Dict.fromList


topCountriesByCo2 : Int -> Int -> Data.Dashboard -> List CountrySnapshot
topCountriesByCo2 year limit dashboard =
    let
        globalTotal =
            globalPointForYear year dashboard.global |> Maybe.andThen .co2 |> Maybe.withDefault 0
    in
    dashboard.countries
        |> List.filter
            (\country ->
                String.length country.iso == 3 && not (isAggregator country.name)
            )
        |> List.filterMap
            (\country ->
                countryPointForYear year country
                    |> Maybe.andThen
                        (\point ->
                            point.co2
                                |> Maybe.map
                                    (\value ->
                                        { iso = country.iso
                                        , name = country.name
                                        , co2 = value
                                        , perCapita = point.co2PerCapita
                                        , share =
                                            case ( point.share, globalTotal ) of
                                                ( Just s, _ ) ->
                                                    Just s

                                                ( Nothing, total ) ->
                                                    if total > 0 then
                                                        Just ((value / total) * 100)
                                                    else
                                                        Nothing
                                        , population = point.population
                                        }
                                    )
                        )
            )
        |> sortByDesc .co2
        |> List.take limit


isAggregator : String -> Bool
isAggregator name =
    String.contains "income" (String.toLower name)
        || String.contains "(GCP" name
        || name == "World"


countryPointForYear : Int -> Data.Country -> Maybe SeriesPoint
countryPointForYear year country =
    country.series
        |> seriesToPoints
        |> ListExtra.find (\point -> point.year == year)


globalPointForYear : Int -> Data.Series -> Maybe SeriesPoint
globalPointForYear year series =
    series
        |> seriesToPoints
        |> ListExtra.find (\point -> point.year == year)


type alias SeriesPoint =
    { year : Int
    , co2 : Maybe Float
    , co2PerCapita : Maybe Float
    , share : Maybe Float
    , population : Maybe Float
    , gdp : Maybe Float
    }


seriesToPoints : Data.Series -> List SeriesPoint
seriesToPoints series =
    let
        partial =
            List.map5
                (\year co2 perCapita share population ->
                    { year = year
                    , co2 = co2
                    , co2PerCapita = perCapita
                    , share = share
                    , population = population
                    , gdp = Nothing
                    }
                )
                series.years
                series.co2
                series.perCapita
                series.share
                series.population
    in
    List.map2 (\point gdp -> { point | gdp = gdp }) partial series.gdp


takeRecent : Int -> List a -> List a
takeRecent count list =
    let
        total = List.length list
    in
    if total <= count then
        list
    else
        list |> List.drop (total - count)


sliderMinYear : Data.Dashboard -> Int
sliderMinYear dashboard =
    dashboard.global.years |> List.minimum |> Maybe.withDefault 1900


sliderMaxYear : Data.Dashboard -> Int
sliderMaxYear dashboard =
    dashboard.global.years |> List.maximum |> Maybe.withDefault 2021


incomeGroupSeries : Data.Dashboard -> Int -> List ( String, Float )
incomeGroupSeries dashboard year =
    dashboard.emitters
        |> ListExtra.find (\bucket -> bucket.year == year)
        |> Maybe.map
            (\bucket ->
                bucket.items
                    |> List.filter (\item -> String.contains "income" (String.toLower item.name))
                    |> List.map (\item -> ( item.name, item.co2 ))
            )
        |> Maybe.withDefault []


cumulativeShare : List CountrySnapshot -> List ( String, Float )
cumulativeShare countries =
    let
        total = countries |> List.map .co2 |> List.sum

        folder country ( running, rows ) =
            let
                share =
                    if total == 0 then
                        0
                    else
                        (country.co2 / total) * 100

                newTotal = running + share
            in
            ( newTotal, rows ++ [ ( country.name, clamp 0 100 newTotal ) ] )
    in
    List.foldl folder ( 0, [] ) (List.take 10 countries) |> Tuple.second


type alias ScatterPoint =
    { name : String
    , gdpPerCapita : Float
    , co2PerCapita : Float
    , population : Float
    }


scatterPoints : Data.Dashboard -> Int -> List ScatterPoint
scatterPoints dashboard year =
    dashboard.countries
        |> List.filterMap (
            \country ->
                countryPointForYear year country
                    |> Maybe.andThen
                        (\point ->
                            case ( point.co2PerCapita, point.population, point.gdp ) of
                                ( Just co2pc, Just population, Just gdp ) ->
                                    if population > 0 then
                                        Just
                                            { name = country.name
                                            , gdpPerCapita = gdp / population
                                            , co2PerCapita = co2pc
                                            , population = population
                                            }
                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
           )
        |> sortByDesc .population
        |> List.take 60


scatterDomain : (ScatterPoint -> Float) -> List ScatterPoint -> ( Float, Float )
scatterDomain accessor points =
    let
        values = points |> List.map accessor
    in
    ( List.minimum values |> Maybe.withDefault 0
    , List.maximum values |> Maybe.withDefault 0
    )


circleRadius : Float -> Float
circleRadius population =
    let
        normalized =
            population |> sqrt |> (
                \n -> clamp 4 16 (n / 250)
            )
    in
    normalized


formatNumber : Int -> Float -> String
formatNumber decimals value =
    let
        factor = 10 ^ decimals
    in
    ((toFloat (round (value * toFloat factor))) / toFloat factor)
        |> String.fromFloat


formatPopulation : Float -> String
formatPopulation pop =
    if pop >= 1.0e9 then
        formatNumber 1 (pop / 1.0e9) ++ " B"
    else if pop >= 1.0e6 then
        formatNumber 1 (pop / 1.0e6) ++ " M"
    else if pop >= 1.0e3 then
        formatNumber 1 (pop / 1.0e3) ++ " K"
    else
        formatNumber 0 pop


humanizeDate : String -> String
humanizeDate value =
    value |> String.left 10


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "The data URL looks incorrect."

        Http.Timeout ->
            "The request timed out. Try again."

        Http.NetworkError ->
            "Network error while fetching data."

        Http.BadStatus status ->
            "Server returned HTTP " ++ String.fromInt status

        Http.BadBody body ->
            "Could not decode data: " ++ body


-- MAP PAYLOAD


encodeMapPayload : Data.Dashboard -> Int -> Encode.Value
encodeMapPayload dashboard year =
    topCountriesByCo2 year 15 dashboard
        |> List.filterMap
            (\country ->
                Dict.get country.iso centroids
                    |> Maybe.map (
                        \( lon, lat ) ->
                            Encode.object
                                [ ( "iso", Encode.string country.iso )
                                , ( "name", Encode.string country.name )
                                , ( "lon", Encode.float lon )
                                , ( "lat", Encode.float lat )
                                , ( "co2", Encode.float country.co2 )
                                , ( "share", country.share |> Maybe.map Encode.float |> Maybe.withDefault Encode.null )
                                ]
                       )
           )
        |> Encode.list identity


centroids : Dict String ( Float, Float )
centroids =
    Dict.fromList
        [ ( "USA", ( -98.5, 39.8 ) )
        , ( "CHN", ( 103.8, 35.3 ) )
        , ( "IND", ( 78.0, 22.0 ) )
        , ( "RUS", ( 37.6, 61.7 ) )
        , ( "JPN", ( 138.0, 36.2 ) )
        , ( "DEU", ( 10.4, 51.0 ) )
        , ( "FRA", ( 2.2, 46.2 ) )
        , ( "GBR", ( -1.5, 54.0 ) )
        , ( "BRA", ( -52.9, -10.8 ) )
        , ( "CAN", ( -106.3, 56.1 ) )
        , ( "IDN", ( 117.3, -2.2 ) )
        , ( "MEX", ( -102.5, 23.7 ) )
        , ( "IRN", ( 53.7, 32.4 ) )
        , ( "SAU", ( 44.5, 23.8 ) )
        , ( "AUS", ( 134.5, -25.7 ) )
        , ( "ZAF", ( 24.7, -29.0 ) )
        , ( "KOR", ( 127.8, 35.9 ) )
        , ( "TUR", ( 34.8, 39.1 ) )
        , ( "ITA", ( 12.6, 42.7 ) )
        , ( "ESP", ( -3.7, 40.2 ) )
        , ( "POL", ( 19.1, 52.1 ) )
        , ( "UKR", ( 31.4, 48.9 ) )
        , ( "ARG", ( -64.2, -34.2 ) )
        , ( "NGA", ( 8.5, 9.1 ) )
        , ( "EGY", ( 30.0, 26.8 ) )
        , ( "VNM", ( 108.3, 16.0 ) )
        , ( "THA", ( 101.0, 15.0 ) )
        , ( "PAK", ( 69.3, 29.6 ) )
        , ( "KAZ", ( 66.9, 48.0 ) )
        , ( "COL", ( -74.3, 4.5 ) )
        , ( "PER", ( -75.0, -9.2 ) )
        , ( "CHL", ( -71.0, -30.0 ) )
        ]


-- CHART HELPERS (reusing previous implementation)


lineChart : { width : Float, height : Float, color : String, fill : String } -> List ( Float, Float ) -> Html msg
lineChart config points =
    case points of
        [] ->
            Html.p [ Attr.class "muted" ] [ Html.text "Not enough data." ]

        _ ->
            let
                minX = points |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0
                maxX = points |> List.map Tuple.first |> List.maximum |> Maybe.withDefault (minX + 1)
                minY = points |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0
                maxY = points |> List.map Tuple.second |> List.maximum |> Maybe.withDefault (minY + 1)

                width = config.width
                height = config.height
                margin = 18

                usableWidth = width - (2 * margin)
                usableHeight = height - (2 * margin)

                scaleX x =
                    if maxX == minX then
                        margin
                    else
                        margin + ((x - minX) / (maxX - minX)) * usableWidth

                scaleY y =
                    if maxY == minY then
                        height - margin
                    else
                        height - margin - ((y - minY) / (maxY - minY)) * usableHeight

                scaledPoints = points |> List.map (\( x, y ) -> ( scaleX x, scaleY y ))

                pathData = toPath scaledPoints
            in
            Svg.svg
                [ SvgAttr.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
                , Attr.class "sparkline"
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
                    , SvgAttr.strokeLinejoin "round"
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
                start = "M " ++ String.fromFloat x0 ++ " " ++ String.fromFloat y0

                tail =
                    rest
                        |> List.map (\( x, y ) -> "L " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                        |> String.join " "
            in
            start ++ " " ++ tail
