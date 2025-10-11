module Internal.Form exposing (FieldDefinition(..), Form(..))

{-| -}

import Dict exposing (Dict)
import Internal.Field
import Pages.FormState exposing (FormState)


type Form error combineAndView parsed input
    = Form
        (List ( String, FieldDefinition ))
        (Maybe input
         -> FormState
         ->
            { result : Dict String (List error)
            , isMatchCandidate : Bool
            , combineAndView : combineAndView
            }
        )
        (input -> List ( String, Maybe String ))
        (Dict.Dict String (Internal.Field.EventInfo -> Maybe String))


{-| -}
type FieldDefinition
    = RegularField
    | HiddenField
