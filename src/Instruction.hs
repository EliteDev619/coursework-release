module Instruction (Instruction (..)) where
import Graphics.Gloss.Data.Color

-- Instructions: steps to be performed by the renderer.
-- Unlike expressions, these do not contain any computations (e.g., addition, function calls).
data Instruction =
      IMove Int
    | IRotate Int
    | IPenUp
    | IPenDown
    | IChangeColor Color
    deriving (Eq, Show)
