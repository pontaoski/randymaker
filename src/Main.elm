module Main exposing (..)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File.Download as Download
import Image
import Json.Encode as E
import QRCode
import Svg.Attributes as SvgA
import Url
import Url.Builder as Builder


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model Array.empty, Cmd.none )


type alias Model =
    { urls : Array String
    }


type Msg
    = AddURL
    | FieldChange Int String
    | Delete Int
    | Download


view : Model -> Browser.Document Msg
view model =
    { title = "orwell"
    , body = [ Element.layout [] (viewEl model) ]
    }


scale : Int -> Float
scale =
    modular 20 1.25


intScale : Int -> Int
intScale =
    scale >> round


fontScale : Int -> Attr decorative msg
fontScale =
    scale >> round >> Font.size


indexedMap : (Int -> a -> b) -> Array a -> List b
indexedMap fn arr =
    Array.indexedMap fn arr
        |> Array.toList


button : { onPress : Maybe msg, label : Element msg } -> Element msg
button =
    Input.button
        [ Background.color <| rgb255 0xBB 0xF7 0xD0
        , Border.width 2
        , Border.shadow
            { offset = ( 0, 1 )
            , size = 1
            , blur = 5
            , color = rgba255 0 0 0 0.1
            }
        , Border.color <| rgb255 0x22 0xC5 0x5E
        , paddingXY (intScale -1) (intScale -4)
        , Border.rounded 3
        ]


viewEl : Model -> Element Msg
viewEl model =
    column [ padding (intScale 1), spacing (intScale -1) ]
        ([ paragraph
            [ fontScale 1 ]
            [ text "enter the list of URLs (they should all start with https://site.com, e.g. https://www.google.com/)" ]
         , none
         ]
            ++ indexedMap viewItem model.urls
            ++ [ button
                    { onPress = Just AddURL
                    , label = text "add a url"
                    }
               , button
                    { onPress = Just Download
                    , label = text "download qr code"
                    }
               , qrCodeView model
               ]
        )


viewItem : Int -> String -> Element Msg
viewItem idx url =
    row [ spacing (intScale -3) ]
        [ Input.text
            []
            { onChange = FieldChange idx
            , text = url
            , placeholder = Nothing
            , label = Input.labelHidden "a field"
            }
        , button
            { onPress = Just <| Delete idx
            , label = text "delete"
            }
        ]


qrCode : Array String -> ( String, Result QRCode.Error QRCode.QRCode )
qrCode urls =
    let
        val =
            E.array E.string urls

        obj =
            E.object [ ( "urls", val ) ]

        str =
            E.encode 0 obj

        uri =
            Builder.crossOrigin "https://randy.blackquill.cc" [ Url.percentEncode str ] []
    in
    ( uri, QRCode.fromString uri )


qrCodeView : Model -> Element Msg
qrCodeView model =
    let
        rend : QRCode.QRCode -> Element Msg
        rend qrc =
            QRCode.toSvg [ SvgA.width "200px", SvgA.height "200px" ] qrc
                |> html

        ( _, qr ) =
            qrCode model.urls

        qrcode =
            qr
                |> Result.map rend
                |> Result.withDefault (text "error rendering")
    in
    column []
        [ qrcode
        ]


remove : Int -> Array a -> Array a
remove i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddURL ->
            ( { model
                | urls = Array.append model.urls (Array.fromList [ "" ])
              }
            , Cmd.none
            )

        FieldChange idx url ->
            ( { model
                | urls = Array.set idx url model.urls
              }
            , Cmd.none
            )

        Delete idx ->
            ( { model | urls = remove idx model.urls }, Cmd.none )

        Download ->
            let
                ( _, qr ) =
                    qrCode model.urls

                prepare qrc =
                    QRCode.toImageWithOptions
                        { moduleSize = 10
                        , darkColor = 0xFF
                        , lightColor = 0xFFFFFFFF
                        , quietZoneSize = 4
                        }
                        qrc
                        |> Image.toPng
                        |> Download.bytes "yourcode.png" "image/png"

                cmd =
                    qr
                        |> Result.map prepare
                        |> Result.withDefault Cmd.none
            in
            ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
