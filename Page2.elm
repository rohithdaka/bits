import Packet
import StartApp.Simple exposing (start)


main =
  start
    { model = Packet.emptyPacket
    , update = Packet.update
    , view = Packet.view
    }