module Tagged.Extra exposing (is)

import Tagged exposing (Tagged)


is : (value -> Bool) -> Tagged tag value -> Bool
is f =
    Tagged.map f >> Tagged.untag
