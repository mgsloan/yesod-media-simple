import Diagrams.Prelude
import Yesod.Media

-- | The main entry point.
main :: IO ()
main = serveDiagram $ sierpinski 3

sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where
    s = sierpinski (n-1)
