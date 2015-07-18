import Diagrams.Prelude
import Diagrams.Backend.Cairo (Cairo)
import Yesod.Media.Simple

-- | The main entry point.
main :: IO ()
main = serveDiagram $ sierpinski 3

sierpinski :: Int -> Diagram Cairo R2
sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where
    s = sierpinski (n-1)
