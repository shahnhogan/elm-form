module Internal.Field exposing (EventInfo(..), Field(..), FieldInfo, Selection(..))

{-| -}

import Json.Encode as Encode


{-| Information about a field event, including the value and selection state.
-}
type EventInfo
    = Input { value : String, selection : Selection }
    | Blur { value : String }
    | Focus { value : String }


{-| Represents the current selection/cursor position in a text input.
-}
type Selection
    = Cursor { before : String, after : String }
    | Range { before : String, selected : String, after : String }


type Field error parsed input initial kind constraints
    = Field (FieldInfo error parsed input initial) kind


{-| -}
type alias FieldInfo error parsed input initial =
    { initialValue : input -> Maybe String
    , decode : Maybe String -> ( Maybe parsed, List error )
    , properties : List ( String, Encode.Value )
    , initialToString : initial -> String
    , compare : String -> initial -> Order
    , formatOnEvent : Maybe (EventInfo -> Maybe String)
    }

