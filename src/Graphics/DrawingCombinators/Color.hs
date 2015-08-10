-- | Color datatype

module Graphics.DrawingCombinators.Color (Color(..), white, modulate) where

import Graphics.DrawingCombinators.Affine

-- | Color is defined in the usual computer graphics sense:
-- a 4 vector containing red, green, blue, and alpha.
--
-- The Monoid instance is given by alpha composition, described
-- at @http:\/\/lukepalmer.wordpress.com\/2010\/02\/05\/associative-alpha-blending\/@
--
-- In the semantcs the values @zero@ and @one@ are used, which are defined as:
--
-- > zero = Color 0 0 0 0
-- > one = Color 1 1 1 1
data Color = Color !R !R !R !R
    deriving (Eq,Show)

instance Monoid Color where
    mempty = Color 0 0 0 0
    mappend (Color r g b a) (Color r' g' b' a') = Color (i r r') (i g g') (i b b') γ
        where
        γ = a + a' - a * a'
        i | γ == 0    = \_ _ -> 0  -- imples a = a' = 0
          | otherwise = \x y -> (a*x + (1-a)*a'*y)/γ

white :: Color
white = Color 1 1 1 1

-- | Modulate two colors by each other.
--
-- > modulate (Color r g b a) (Color r' g' b' a')
-- >           = Color (r*r') (g*g') (b*b') (a*a')
modulate :: Color -> Color -> Color
modulate (Color r g b a) (Color r' g' b' a') = Color (r*r') (g*g') (b*b') (a*a')

