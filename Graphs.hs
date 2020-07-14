import Graphics.Gloss
import InterComp      hiding (scale)
import Fuck

pi2 = pi/2
x = [-33.0,-32.999..33.0]
fx = map (midpoint . f) x
fx' = map (midpoint . f' . singleton) x

path = zip x fx
path' = zip x fx'

graph = color blue $ line path
graph' = color blue $ line path'

hLine x = line $ zip [-100,100] [x,x]
vLine x = line $ zip [x,x] [-100,100]

zeroHLine = hLine 0
zeroVLine = vLine 0

piVLines = pictures $ map vLine [-10*pi2,-9*pi2..10*pi2]
piHLines = pictures $ map hLine [-10*pi2,-9*pi2..10*pi2]

disconts = color red $ pictures $ map (vLine . discoPoint) [-30..30]

dispath = display FullScreen white $ scale 25 25 $ pictures [zeroVLine, zeroHLine, graph, disconts]
dispath' = display FullScreen white $ scale 25 25 $ pictures [piVLines, piHLines, zeroVLine, zeroHLine, graph']

g r w i = map (\x -> let aux = i * ((-w...w) + singleton x) in (inf aux, sup aux)) [-r,-r+0.1..r]

mkG r w = scale 100 100 . line . Main.g r w

p r w = pictures $ map (line . Main.g r w) $ take 6 $ iterate (+1) (0...1)

pln = pictures [zeroHLine, zeroVLine]

pln45 = rotate 45 pln

pln30 = rotate 30 pln

pln60 = rotate 60 pln

main :: IO ()
main = display FullScreen white $ pictures [pln, pln45, pln30, pln60, p 5 1]
