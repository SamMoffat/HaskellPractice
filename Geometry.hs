--File related to modules lesson

module Geometry
( sphereVolume
,	sphereArea
,	cubeVolume
,	cubeArea
,	cuboidVolume
,	cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: Float -> Float
cubeVolume h = cuboidVolume h h h

cubeArea = Float -> Float
cubeArea h = cuboidArea h h h

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b --reactangle area is a private function, more or less; because it isn't included in "module Geometry", it is not exported.