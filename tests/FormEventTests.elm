module FormEventTests exposing (all)

import Dict
import Expect
import Form
import Form.Field as Field
import Form.Validation as Validation
import Html
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query


type Msg
    = FormMsg (Form.Msg Msg)
    | Submitted


formWithHiddenField : String -> Form.HtmlForm String ( String, String ) input Msg
formWithHiddenField hiddenName =
    Form.form
        (\hidden name ->
            { combine =
                Validation.succeed Tuple.pair
                    |> Validation.andMap hidden
                    |> Validation.andMap name
            , view = \_ -> []
            }
        )
        |> Form.hiddenField hiddenName (Field.text |> Field.required "Required")
        |> Form.field "name" (Field.text |> Field.required "Required")


renderInputForm : String -> Html.Html Msg
renderInputForm hiddenName =
    formWithHiddenField hiddenName
        |> Form.renderHtml
            { submitting = False
            , state = Form.init
            , toMsg = FormMsg
            }
            (Form.options "myForm")
            []


renderSubmitForm : String -> Html.Html Msg
renderSubmitForm hiddenName =
    formWithHiddenField hiddenName
        |> Form.renderHtml
            { submitting = False
            , state = Form.init
            , toMsg = FormMsg
            }
            (Form.options "myForm"
                |> Form.withAction "/submit"
                |> Form.withOnSubmit (\_ -> Submitted)
            )
            []


shadowedElement : String -> Encode.Value
shadowedElement name =
    Encode.object
        [ ( "tagName", Encode.string "INPUT" )
        , ( "name", Encode.string name )
        ]


dataset : Encode.Value
dataset =
    Encode.object [ ( "elmFormId", Encode.string "myForm" ) ]


inputEvent : Encode.Value -> ( String, Encode.Value )
inputEvent idValue =
    ( "input"
    , Encode.object
        [ ( "type", Encode.string "input" )
        , ( "target"
          , Encode.object
                [ ( "type", Encode.string "text" )
                , ( "value", Encode.string "hello" )
                , ( "name", Encode.string "name" )
                ]
          )
        , ( "currentTarget"
          , Encode.object
                [ ( "id", idValue )
                , ( "dataset", dataset )
                ]
          )
        ]
    )


submitEvent : Encode.Value -> ( String, Encode.Value )
submitEvent idValue =
    ( "submit"
    , Encode.object
        [ ( "type", Encode.string "submit" )
        , ( "currentTarget"
          , Encode.object
                [ ( "id", idValue )
                , ( "method", Encode.string "post" )
                , ( "action", Encode.string "/submit" )
                , ( "dataset", dataset )
                ]
          )
        ]
    )


fieldValue : String -> Form.Model -> Maybe String
fieldValue fieldName model =
    model
        |> Dict.get "myForm"
        |> Maybe.andThen (.fields >> Dict.get fieldName)
        |> Maybe.map .value


updatedNameValue : Result String Msg -> Maybe String
updatedNameValue result =
    case result of
        Ok (FormMsg formMsg) ->
            Form.updateWithMsg formMsg Form.init
                |> Tuple.first
                |> fieldValue "name"

        _ ->
            Nothing


dispatchedOnSubmit : Result String Msg -> Maybe Msg
dispatchedOnSubmit result =
    case result of
        Ok (FormMsg formMsg) ->
            Form.updateWithMsg formMsg Form.init
                |> Tuple.second

        _ ->
            Nothing


all : Test
all =
    describe "a hidden field named \"id\" shadows form.id via [LegacyOverrideBuiltIns]"
        [ test "input event on a form with a non-reserved hidden field name works" <|
            \() ->
                renderInputForm "record-id"
                    |> Query.fromHtml
                    |> Event.simulate (inputEvent (Encode.string "myForm"))
                    |> Event.toResult
                    |> updatedNameValue
                    |> Expect.equal (Just "hello")
        , test "submit event on a form with a non-reserved hidden field name fires onSubmit" <|
            \() ->
                renderSubmitForm "record-id"
                    |> Query.fromHtml
                    |> Event.simulate (submitEvent (Encode.string "myForm"))
                    |> Event.toResult
                    |> dispatchedOnSubmit
                    |> Expect.equal (Just Submitted)
        , test "input event on a form with a hidden field named \"id\" still updates form state" <|
            \() ->
                renderInputForm "id"
                    |> Query.fromHtml
                    |> Event.simulate (inputEvent (shadowedElement "id"))
                    |> Event.toResult
                    |> updatedNameValue
                    |> Expect.equal (Just "hello")
        , test "submit event on a form with a hidden field named \"id\" still fires onSubmit" <|
            \() ->
                renderSubmitForm "id"
                    |> Query.fromHtml
                    |> Event.simulate (submitEvent (shadowedElement "id"))
                    |> Event.toResult
                    |> dispatchedOnSubmit
                    |> Expect.equal (Just Submitted)
        ]
