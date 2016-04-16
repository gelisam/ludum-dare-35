module Vec where


type alias Vec a =
  { x : a
  , y : a
  }

init : Vec number
init =
  { x = 0
  , y = 0
  }
