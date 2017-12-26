module Main exposing (..)

import Html exposing (Html, text, div, a, span, textarea)
import Html.Attributes exposing (type_, class, value, readonly, wrap)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { displayText : List Char, lastKey : Maybe Key, numOfPush : Int }


init : ( Model, Cmd Msg )
init =
    ( { displayText = [], lastKey = Nothing, numOfPush = 0 }, Cmd.none )


type Key
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Zero


selectChars : Key -> List Char
selectChars key =
    case key of
        One ->
            [ 'あ', 'い', 'う', 'え', 'お' ]

        Two ->
            [ 'か', 'き', 'く', 'け', 'こ' ]

        Three ->
            [ 'さ', 'し', 'す', 'せ', 'そ' ]

        Four ->
            [ 'た', 'ち', 'つ', 'て', 'と' ]

        Five ->
            [ 'な', 'に', 'ぬ', 'ね', 'の' ]

        Six ->
            [ 'は', 'ひ', 'ふ', 'へ', 'ほ' ]

        Seven ->
            [ 'ま', 'み', 'む', 'め', 'も' ]

        Eight ->
            [ 'や', 'ゆ', 'よ' ]

        Nine ->
            [ 'ら', 'り', 'る', 'れ', 'ろ' ]

        Zero ->
            [ 'わ', 'を', 'ん' ]



---- UPDATE ----


type Msg
    = PushKey Key
    | PushEnter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ displayText, lastKey, numOfPush } as model) =
    case msg of
        PushKey key ->
            if Just key == lastKey then
                ( { model | numOfPush = numOfPush + 1 }, Cmd.none )
            else
                case lastKey of
                    Just lk ->
                        ( { model | displayText = (lastChar lk numOfPush) :: displayText, lastKey = Just key, numOfPush = 0 }, Cmd.none )

                    Nothing ->
                        ( { model | lastKey = Just key, numOfPush = 0 }, Cmd.none )

        PushEnter ->
            case lastKey of
                Just lk ->
                    ( { model | displayText = (lastChar lk numOfPush) :: displayText, lastKey = Nothing, numOfPush = 0 }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


lastChar : Key -> Int -> Char
lastChar key numOfPush =
    case List.head <| List.drop (numOfPush % (List.length <| selectChars key)) (selectChars key) of
        Just c ->
            c

        Nothing ->
            Debug.crash "Illegal key"



---- VIEW ----


view : Model -> Html Msg
view { displayText, lastKey, numOfPush } =
    let
        dt =
            case lastKey of
                Just key ->
                    (lastChar key numOfPush) :: displayText |> List.reverse |> String.fromList

                Nothing ->
                    displayText |> List.reverse |> String.fromList
    in
        div []
            [ div [ class "display" ]
                [ textarea [ class "display-text", readonly True, wrap "hard", value dt ] []
                ]
            , div [ class "keyboard" ]
                [ a [ class "return button", type_ "button", onClick PushEnter ] [ span [ class "icon is-medium" ] [ text "□" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey One ] [ span [ class "icon" ] [ text "1 あ" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Two ] [ span [ class "icon" ] [ text "2 か" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Three ] [ span [ class "icon" ] [ text "3 さ" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Four ] [ span [ class "icon" ] [ text "4 た" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Five ] [ span [ class "icon" ] [ text "5 な" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Six ] [ span [ class "icon" ] [ text "6 は" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Seven ] [ span [ class "icon" ] [ text "7 ま" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Eight ] [ span [ class "icon" ] [ text "8 や" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Nine ] [ span [ class "icon" ] [ text "9 ら" ] ]
                , a [ class "button", type_ "button" ] [ span [ class "icon" ] [ text "*" ] ]
                , a [ class "button", type_ "button", onClick <| PushKey Zero ] [ span [ class "icon" ] [ text "0 わ" ] ]
                , a [ class "button", type_ "button" ] [ span [ class "icon" ] [ text "#" ] ]
                ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
