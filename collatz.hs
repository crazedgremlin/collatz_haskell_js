import Haste
import Haste.Graphics.Canvas
import Haste.App

{-
 - Compute the stopping time for the input n, where n is an integer and n > 0.
 -}
collatzCount :: Int -> Int
collatzCount n = length $ takeWhile (>1) $ iterate collatz n
  where
    collatz :: Int -> Int
    collatz m
      | mod m 2 == 0    =   div m 2
      | otherwise       =   3*m + 1


drawPoint :: MonadIO m => Canvas -> Point -> m ()
drawPoint c (x,y) = renderOnTop c $ fill $ rect (x,y) (x+1,y+1)

createPoint :: (Int,Int) -> Point
createPoint (a,b) = (fromIntegral a, fromIntegral b)

{- 
 - Plot the stopping times of the values along the x axis.
 -}
main :: IO ()
main = do
  Just ce <- elemById "canvas"
  Just c <- getCanvas ce

  -- render c $ text (110, 120) "Loading, please wait..."
  --render c $ fill $ rect (20, 20) (21, 21)

  mapM (\x -> drawPoint c (createPoint (x, collatzCount x))) [0..639]

  return ()
