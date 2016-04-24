import Bit 
import Header 
import StartApp.Simple exposing(start)

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model 
    }



main = 
    start { 
        model = (Bit.defaultBit 0 {x=0,y=0} "data")
    ,   view = Bit.view
    ,   update = Bit.update 
    }