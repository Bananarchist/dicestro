module Main exposing (main, spreadForListOfDice)

import Axis
import Browser
import Html as Tag
import Html.Attributes as Hat
import Html.Events as Emit
import List exposing (concat, filter, foldl, length, map, range, repeat, sort, sortBy, sum, unzip)
import List.Extra exposing (allDifferentBy, cartesianProduct, gatherEquals, zip)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Tuple exposing (first, mapBoth, mapSecond, second)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


type alias Model =
    { dice : List ( Int, Int ) }


type Msg
    = AddDie
    | RemoveDie Int
    | SetCountForFace Int Int
    | ChangeFaceForFace Int Int
    | SimplifySet


type DuplicatableTagIndex
    = Only
    | Last
    | Middle


subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ ( 1, 6 ) ], Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDie : Int -> (( Int, Int ) -> ( Int, Int )) -> List ( Int, Int )
        updateDie face fn =
            map
                (\( c, f ) ->
                    if f == face then
                        fn ( c, f )

                    else
                        ( c, f )
                )
                model.dice

        flattenDice dice =
            dice
                |> sortBy second
                |> List.Extra.groupWhile (\( _, f1 ) ( _, f2 ) -> f1 == f2)
                |> map (\( k, v ) -> foldl (\( an, f ) ( bn, _ ) -> ( an + bn, f )) ( 0, 0 ) (k :: v))
    in
    case msg of
        AddDie ->
            ( { model | dice = model.dice |> (::) ( 1, 1 ) |> sortBy second }, Cmd.none )

        RemoveDie face ->
            ( { model | dice = model.dice |> filter (second >> (/=) face) }, Cmd.none )

        SetCountForFace face count ->
            ( { model
                | dice = updateDie face (\_ -> ( count, face ))
              }
            , Cmd.none
            )

        ChangeFaceForFace original new ->
            ( { model
                | dice = updateDie original (\( c, _ ) -> ( c, new ))
              }
            , Cmd.none
            )

        SimplifySet ->
            ( { model | dice = flattenDice model.dice }, Cmd.none )


w : Float
w =
    900


h : Float
h =
    450


padding =
    30


xScale model =
    model
        |> data
        |> unzip
        |> first
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : Model -> ContinuousScale Float
yScale model =
    model
    |> data
    |> unzip
    |> second
    |> List.maximum
    |> Maybe.withDefault 1.0
    |> Tuple.pair 0
    |> Scale.linear (h - 2 * padding, 0 )
    --|> Scale.band { defaultLinearScale | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, h - 2 * padding )
    --Scale.linear ( h - 2 * padding, 0 ) ( 0, 1.0 )


xAxis model =
    Axis.bottom [] (Scale.toRenderable String.fromFloat (xScale model))


yAxis model =
    --Axis.left [] (Scale.toRenderable String.fromFloat (yScale model))
    Axis.left [ Axis.tickCount 5 ] (yScale model)


column xsc ysc ( sum, p ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert xsc sum
            , y <| Scale.convert ysc p
            , width <| Scale.bandwidth xsc
            , height <| h - Scale.convert ysc p - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable String.fromFloat xsc) sum
            , y <| Scale.convert ysc p - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat p ]
        ]


view : Model -> Tag.Html Msg
view model =
    Tag.main_ []
        [ Tag.section []
            [ svg [ viewBox 0 0 w h ]
                [ style [] [ text """
              .column rect { fill: rgba(118, 214, 78, 0.8); }
              .column text { display: none }
              .column:hover rect { fill: rgb(118, 214, 78); }
              .column:hover text { display: inline; }
              """ ]
                , g [ transform [ Translate (padding - 1) (h - padding) ] ] [ xAxis model ]
                , g [ transform [ Translate (padding - 1) padding ] ] [ yAxis model ]
                , g [ transform [ Translate padding padding ], class [ "series" ] ] <| map (column (xScale model) (yScale model)) (data model)
                ]
            ]
        , form model
        ]


anyDuplicateDiceIn =
    allDifferentBy second
        >> not


form : Model -> Tag.Html Msg
form model =
    let
        totalDice =
            List.length model.dice

        tagIndex idx =
            if totalDice == 1 then
                Only

            else if idx + 1 < totalDice then
                Middle

            else
                Last

        notInSampleSet =
            True

        fieldSets =
            List.indexedMap (\i d -> tagIndex i |> fieldSetForDie d) model.dice
    in
    Tag.section []
        (if anyDuplicateDiceIn model.dice then
            fieldSets ++ [ mergeDuplicatesButton ]

         else
            fieldSets
        )


mergeDuplicatesButton =
    Tag.button [ Emit.onClick SimplifySet ] [ Tag.text "Simplify by merging duplicate dice" ]


fieldSetForDie : ( Int, Int ) -> DuplicatableTagIndex -> Tag.Html Msg
fieldSetForDie ( count, faces ) tagIndex =
    let
        id f =
            "inputFor" ++ String.fromInt f ++ "SidedDie"

        inputAttrs =
            [ Hat.type_ "number"
            , Hat.id (id faces)
            , Hat.value (String.fromInt count)
            , Hat.min "1"
            , Emit.onInput (String.toInt >> Maybe.withDefault count >> SetCountForFace faces)
            ]

        optAttrs f =
            [ Hat.value f
            , Hat.selected (f |> String.toInt |> Maybe.withDefault 0 |> (==) faces)
            ]

        optValue f =
            [ Tag.text f ]

        optTag f =
            Tag.option (optAttrs f) (optValue f)

        addButton =
            Tag.button [ Emit.onClick AddDie ] [ Tag.text "+" ]

        delButton =
            Tag.button [ Emit.onClick (RemoveDie faces) ] [ Tag.text "-" ]

        buttons =
            case tagIndex of
                Only ->
                    [ addButton ]

                Last ->
                    [ delButton, addButton ]

                Middle ->
                    [ delButton ]
    in
    Tag.fieldset [] <|
        List.append
            [ Tag.input inputAttrs []
            , Tag.text "d"
            , Tag.select [ Emit.onInput (String.toInt >> Maybe.withDefault 6 >> ChangeFaceForFace faces) ] <|
                List.map (String.fromInt >> optTag) (List.range 1 20)
            ]
            buttons



data model =
    model.dice
        |> map (\( n, d ) -> List.repeat n d)
        |> List.concat
        |> spreadForListOfDice


spreadForListOfDice : List Int -> List ( Float, Float )
spreadForListOfDice =
    map (range 1)
        >> cartesianProduct
        >> map sum
        >> sort
        >> gatherEquals
        >> map (mapBoth toFloat (length >> toFloat >> (+) 1.0))
        >> unzip
        >> mapSecond (\l -> map (\x -> x / sum l) l)
        >> (\t -> zip (first t) (second t))
