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


plus : Vec number -> Vec number -> Vec number
plus v1 v2  =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }

minus : Vec number -> Vec number -> Vec number
minus v1 v2  =
  { x = v1.x - v2.x
  , y = v1.y - v2.y
  }

scale : number -> Vec number -> Vec number
scale s v =
  { x = s * v.x
  , y = s * v.y
  }


-- units are blocks
type alias Coord = Vec Int

-- units are pixels
type alias Pixels = Vec Int

pixels : Coord -> Pixels
pixels = scale 28
