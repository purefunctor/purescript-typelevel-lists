module Items where

import Type.Data.List


-- Create your kind and constructors
foreign import kind YesNo
foreign import data Yes :: YesNo
foreign import data No :: YesNo


-- Create an adapter for your kind
foreign import data YesNoItem :: YesNo -> Item'


type YesNoList = ( YesNoItem Yes :> YesNoItem No :> Nil' )
