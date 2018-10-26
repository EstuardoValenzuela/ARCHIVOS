import Graphics.Gloss
import System.IO

main :: IO () 
main = do     
  hSetBuffering stdout NoBuffering    
  putStr "_______________________________________________  _______________________________________ \n|Bienvido al programa generador de fractales   | |¿Quien es Arturito? Arturito es un    |\n|Mi nombre es arturito y sere tu guia,         | |asistente que se creo para ayudarte   |\n|aqui algunos consejos:                        | |a utilizar el programa, si te ayudo   |\n|abajo de este cuadro de texo, con presionar   | |puedes darnos puntos extra,           |\n|la tecla 0, seguido de la tecla enter, podras | |eso haria a Arturito muy feliz :)     |\n|visualizar el fractal llamado Triangulo de    | | Este proyecto fue elaborado por      |\n|Sierpinski, luego tendras opcion de visualizar| | Estuardo Valenzuela y Sliven Carranza|\n|el número de recursiones que quieras, siempre | | Ambos estuadiantes de ingenieria     |\n|y cuando sean menores a 6. Al proyectarse el  | | si te gusto el programa, no olivides |\n|fractal, podras hacer zoom con el scroll del  | | darnos un 40/30, nos costo mucho :(  |\n|Mouse, si haces clic en el, ¡PUEDES MOVERLO!  | |______________________________________|\n|Espero y te guste, ¡Nos vemos mas abajo!      |\n|______________________________________________|\n \n \n \n [0] Triangulo de Sierpinski. \n ¿Deseas visualizar el siguiente fractal? [0] No [1] Si:"
  frac <- readLn       
  if frac == 0 then do 
    putStr "Ingrese el numero de recursion que quiere ver en el Triangulo de Sierpinski [0..6]: "
    paso <- readLn
    display (InWindow "Fractal: Triangulo de Sierpinki" (700,800) (20,20)) yellow (triangulo paso)
    else do
        putStr "[1] Fractal: Copo de nieve \n Ya solo queda un fractal :( \n ¿Quieres ver el mejor de todos?\n ¿o quieres ver el copo de nieve?\n [0] Quiero ver el mejor de todos! (si lo miras tienes que poner la nota completa jeje)\n [1] No, quiero ver el copo de nieve (igual te pongo la nota completa): "
        fracb <- readLn
        if fracb == 1 then do
            putStr "Ingrese el numero de recursiones a visualizar en el fractal copo de nieve [0...6]: "
            copo <- readLn
            display (InWindow "Fractal copo de nieve" (500,500) (20,20)) black (nieve copo)
            else do
                putStr "Muy buena eleccion... ahora Ingresa el numero de recursiones que quieres visualizar en el fractal Árbol en invierno [0..6]: "
                pasod <- readLn
                display (InWindow "Arbol Invierno" (700,800) (20,20)) black (tercero pasod)



                
                      
--------------------------------------------------------------------------------------------------------
triangulo :: Int -> Picture
triangulo paso = Color red (Translate (-150) (-125) (primero paso))
        
longitud = 300 :: Float
         
primero :: Int -> Picture
primero 0 =                                  ---Trianglu equilatero
    Polygon [(0,0),                             ---
            (longitud/2, longitud * sqrt 3 /2),
            (longitud, 0)]
primero n
    = Pictures [nuevoprimero,
                Translate (longitud/2) 0 nuevoprimero,
                Translate (longitud/4) (longitud * sqrt 3 /4) nuevoprimero]
        where nuevoprimero = Scale 0.5 0.5 (primero (n-1))
  --------------------------------------------------------------------------------      ---------------- Inicia declaracon del otro fractal

nieve :: Int -> Picture
nieve copo =
    Color blue $                              
    Translate (-longitud/2) ((longitud * sqrt 3)/6) $   
    fractter copo  
    
linea :: Int -> Picture
linea 0 = Line [(0,0), (longitud, 0)]
linea n =
    Pictures [nuevalinea,
              Translate (longitud/3)     0                        (Rotate (-60) nuevalinea),
              Translate (longitud/2)     ((longitud * sqrt 3)/6)  (Rotate   60  nuevalinea),
              Translate (2 * longitud/3) 0                        nuevalinea]
    where nuevalinea = Scale (1/3) (1/3) (linea (n-1))

fractter :: Int -> Picture
fractter n = linea 1
    --Pictures [unalinea,
    --          Translate longitud     0                          (Rotate   120  unalinea),
    --          Translate (longitud/2) (-((longitud * sqrt 3)/2)) (Rotate (-120) unalinea)]
    --where unalinea = linea n


---------------------------------------------------------------------------------------------------
tercero :: Int -> Picture
tercero pasod = Translate 0 (-300) (arbol pasod cafe)

tronco :: Color -> Picture
tronco color = Color color (Polygon [(30,0), (15,300), (-15,300), (-30,0)])

arbol :: Int -> Color -> Picture
arbol 0 color = Pictures [
    tronco color,
    Scale 0.5 0.5 $ tronco (masVerde color)]
arbol n color = Pictures [ tronco color,
                          Translate 0 300 arbolMenor,
                          Translate 0 240 (Rotate   20  arbolMenor),
                          Translate 0 180 (Rotate (-20) arbolMenor),
                          Translate 0 120 (Rotate   40  arbolMenor),
                          Translate 0  60 (Rotate (-40) arbolMenor) ]
    where arbolMenor = Scale 0.5 0.5 (arbol (n-1) (masVerde color))

cafe :: Color
cafe = makeColorI 148 97  37 260

masVerde :: Color -> Color
masVerde color = mixColors 1.0 0.1 color white
------------------------------------------------------------------------------------------------------
