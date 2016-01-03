
import SuperTimer exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = ""
    , update = update
    , view = view
    }
