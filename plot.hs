import Graphics.EasyPlot
import Heateqreg
--import heateqpar



reg = Data3D [Style Lines] [] (fdm 1 0 1 0 100 0.1 0.01 (\x->((x^2) + (5*x) +5)))

main = plot X11 reg