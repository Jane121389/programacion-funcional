import Codec.Picture

type Point = (Float, Float)

next::Point->Point->Point

next (u,v) (x,y)=(x*x-y*y+u,2*x*y+v)--Dado dos puntos, aplica una operación sobre estos y devuelve otros dos puntos.

mandelbrot ::Point->[Point]

mandelbrot p=iterate (next p) (0,0)--crea todos los puntos iterando la función next en cada punto

cerca::Point->Bool

cerca (u,v)=(u*u+v*v)<100--Verifica si un punto esta lo suficientemente cerca, si es así entonces se encuentra en el conjunto de MandelBrot

estaEnMandelbrot ::Int-> Point ->Bool

estaEnMandelbrot n  p= all cerca (take n(mandelbrot p))--all acepta un predicado ,una lista y devuelve verdadero si todos los elementos de la lista cumplen el predicado

chooseColor :: [color]->[Point]->color

chooseColor paleta =(paleta!!).length.take n.takeWhile cerca  where n=length paleta-1--Toma los puntos mientas la función cerca sea verdadera , puede ocurir que la lista se haga de tamaño infinito, por lo tanto tomaremos solo n puntos (que son los elementos de la peleta), mide la longitud de la lista y toma el color que se encuentre en la posición de la longitud de la paleta

type ImageFunction color=Point ->color

fracImage::(Point->[Point])->[color]->ImageFunction color

fracImage fractal paleta= chooseColor paleta.fractal--chooseColor paleta se aplica a fractal (función) , en este caso se aplicara la función mandelbrot, y nos devuelve una función donde evalua un punto y devuele un color usando el chooseColor

type Grid a = [[a]]

for :: Int->Float-> Float ->[Float]

for n min max =take n [min, min + delta ..] where delta = (max - min)/fromIntegral (n - 1)--Divide  un tramo de un rango minimo a un rango maximo en n celdas

grid :: Int -> Int ->Point -> Point-> Grid Point

grid c r (xmin , ymin ) (xmax , ymax )= [[(x , y) | x<- for c xmin xmax ] | y<- for r ymin ymax ]--Crea una malla con 'c' número de columnas y 'r' número de renglones

--grid 5 5 (0,0) (10,10)

sample :: Grid Point-> ImageFunction color->Grid color--Dado un Grid point

sample points image = map (map image) points-- Image es una función que va de puntos a colores , como points es Grid Point, es decir, una lista de lista entonces aplicamos dos veces map

draw ::Grid Point-> (Point ->[Point])-> [color]->(Grid color ->image)->image

draw points fractal palette render = render (sample points (fracImage fractal palette))--fracImage regresa image(que convierte los puntos a  colores) y sample regresa la malla de colores a partir de una malla de puntos y del image color y render convierte la malla de colores a un objeto (imagen donde los colores pueden ser caracteres o colores , etc)

charPalette :: [Char]

charPalette= "     ,o-!|?/<>X+={^O#%&@8*$"

--charPalette= " ,o-!|?/<>X+={^O#%&@8*$"

charRender :: Grid Char->IO()

charRender=putStr.unlines--putStr imprime en pantalla un salto de línea cada que encuentra un \n y unlines devuelve un String que concatena los elementos de una lista de String separados por un \n

figure1 = draw points mandelbrot charPalette charRender where points = grid 79 37 (-2.25, -1.5) (0.75, 1.5)

julia::Point->Point->[Point]

julia c= iterate (next c)

figure2 = draw points (julia (0.32, 0.043)) charPalette charRender where points = grid 79 37 (-1.5, -1.5) (1.5, 1.5)

imageCreator :: String -> Int -> Int -> (Int->Int->PixelRGB8) -> IO ()
imageCreator path width height pixelRenderer= writePng path $ generateImage pixelRenderer width height--PixelRender , dadas dos coordenadas te da un color, ejemplo (1,3) verde

degradado :: PixelRGB8 -> PixelRGB8 -> Int -> [PixelRGB8]
{--degradado (PixelRGB8 x y z) (PixelRGB8 x2 y2 z2) pasos =
  [PixelRGB8 (x+(division x2 x pasos)*(fromIntegral i))
             (y+(division y2 y pasos)*(fromIntegral i))
             (z+(division z2 z pasos)*(fromIntegral i)) | i <- [0..pasos]]--}
             
degradado (PixelRGB8 x y z) (PixelRGB8 x2 y2 z2) pasos =
  [PixelRGB8 (x+(division x2 x pasos)*(fromIntegral j))
             (y+(division y2 y pasos)*(fromIntegral i))
             (z+(division z2 z pasos)*(fromIntegral j)) | i <- [0..pasos],j <- [0,2..pasos*2]]
             
division :: Pixel8 -> Pixel8 -> Int -> Pixel8

division x y pasos= round ((fromIntegral x-fromIntegral y) / (fromIntegral pasos))

paletaRGB :: [PixelRGB8]
--paletaRGB = degradado (PixelRGB8 0 0 0) (PixelRGB8 19 158 178) 200
paletaRGB = degradado (PixelRGB8 0 0 0) (PixelRGB8 170 80 140) 200
--paletaRGB = degradado (PixelRGB8 20 20 50) (PixelRGB8 100 100  200) 100
--paletaRGB = degradado (PixelRGB8 0 0 0) (PixelRGB8  70 200 250) 200

--paletaRGB = degradado (PixelRGB8 100 100 205) (PixelRGB8 0 0 0) 100

rgbRender :: Grid PixelRGB8 -> IO()
rgbRender grid = imageCreator "fractal_j9.png" (length (grid!!0)) (length grid) pixelRenderer
               where pixelRenderer x y = (grid!!y)!!x

figure3 = draw points mandelbrot paletaRGB rgbRender where points = grid 500 250 (-2.25, -1.5) (0.75, 1.5)

figure4 = draw points (julia (-0.194, 0.6557)) paletaRGB rgbRender where points = grid 500 250 (-1.5, -1.5) (1.5, 1.5)

