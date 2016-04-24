import Bit exposing(..)
import StartApp.Simple exposing(start)


main = 
    start { 
        model = (defaultBit 0 {x=0,y=0} "data")
    ,   view = viewBit
    ,   update = updateBit 
    }