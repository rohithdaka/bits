import Packet exposing (..)
import StartApp.Simple exposing (start)


main =
  start
    { model = emptyPacket
    , update = updatePacket
    , view = viewPacket
    }