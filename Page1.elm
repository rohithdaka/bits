import Bit exposing(updateBit, viewBit)
import StartApp.Simple as StartApp


defaultBit = { value = 0
            , location = {x=0,y=0}
            , category ="data"
            }

main = StartApp.start { model = defaultBit, view = viewBit, update = updateBit }