module Format exposing (main)

import Browser
import Form
import Form.Field as Field exposing (EventInfo(..), Selection(..))
import Form.FieldView as FieldView
import Form.Validation as Validation
import Html exposing (Html, div, text)
import Html.Attributes


type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit
        { fields : List ( String, String )
        , method : Form.Method
        , action : String
        , parsed : Form.Validated String FormData
        }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { formState : Form.Model
    , submitting : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { formState = Form.init
      , submitting = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit parsed ->
            let
                _ =
                    Debug.log "OnSubmit!" parsed
            in
            ( model, Cmd.none )

        FormMsg formMsg ->
            let
                ( updatedFormModel, cmd ) =
                    Form.update formMsg model.formState
            in
            ( { model | formState = updatedFormModel }, cmd )


view : Model -> Browser.Document Msg
view model =
    { title = "elm-form formatOnEvent demo"
    , body =
        [ div []
            [ formatExampleForm
                |> Form.renderHtml
                    { submitting = model.submitting
                    , state = model.formState
                    , toMsg = FormMsg
                    }
                    (Form.options "format-example"
                        |> Form.withOnSubmit OnSubmit
                    )
                    []
            ]
        ]
    }


type alias FormData =
    { name : String
    , phone : String
    , username : String
    }


formatExampleForm : Form.HtmlForm String FormData input msg
formatExampleForm =
    (\name phone username ->
        { combine =
            Validation.succeed FormData
                |> Validation.andMap name
                |> Validation.andMap phone
                |> Validation.andMap username
        , view =
            \context ->
                [ fieldView context "Name (trimmed on blur)" name
                , fieldView context "Phone (formatted on input)" phone
                , fieldView context "Username (uppercase on input, trimmed on blur)" username
                , Html.button []
                    [ if context.submitting then
                        text "Submitting..."

                      else
                        text "Submit"
                    ]
                ]
        }
    )
        |> Form.form
        |> Form.field "name"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Blur { value } ->
                                Just (String.trim value)

                            _ ->
                                Nothing
                    )
                |> Field.required "Name is required"
            )
        |> Form.field "phone"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Input { value, selection } ->
                                case selection of
                                    Cursor { after } ->
                                        if after == "" then
                                            Just (formatPhoneNumber value)

                                        else
                                            Nothing

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Field.required "Phone is required"
            )
        |> Form.field "username"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Input { value } ->
                                Just (String.toUpper value)

                            Blur { value } ->
                                Just (String.trim value)

                            _ ->
                                Nothing
                    )
                |> Field.required "Username is required"
            )


formatPhoneNumber : String -> String
formatPhoneNumber value =
    let
        -- Remove all non-digits
        digits =
            String.filter Char.isDigit value

        -- Format as (XXX) XXX-XXXX
        formatted =
            case String.length digits of
                0 ->
                    ""

                1 ->
                    digits

                2 ->
                    digits

                3 ->
                    digits

                4 ->
                    "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

                5 ->
                    "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

                6 ->
                    "(" ++ String.left 3 digits ++ ") " ++ String.dropLeft 3 digits

                7 ->
                    "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.dropLeft 6 digits

                _ ->
                    "(" ++ String.left 3 digits ++ ") " ++ String.slice 3 6 digits ++ "-" ++ String.slice 6 10 digits
    in
    formatted


fieldView :
    Form.Context String input
    -> String
    -> Validation.Field String parsed FieldView.Input
    -> Html msg
fieldView context label field =
    div []
        [ Html.label []
            [ text (label ++ " ")
            , FieldView.input [] field
            , errorsView context field
            ]
        ]


errorsView :
    Form.Context String input
    -> Validation.Field String parsed kind
    -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Validation.statusAtLeast Validation.Blurred field then
        errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [ Html.Attributes.style "color" "red" ] [ text error ])
            |> Html.ul []

    else
        Html.ul [] []
