module Render where
import AddrSpace
import Cpu
import Data.Array
import Data.Array.IO
import qualified Data.Array.Unboxed as UA
import Data.IORef
import Data.Word
import Data.Bits
import Data.Int

type Tile = UA.UArray (Word8, Word8) Word8
type Background = Array (Word8, Word8) Tile
type Tileset = Array Int Tile
type Screen = IOUArray (Word8, Word8) Word32
data Sprite = Sprite {
                       arriba :: Bool, 
                       pixelarray :: Array (Int, Int) Word32
                      }
listaT = (listArray (0,383) [0 .. 383])
data Paleta = Paleta { 
                       color0 :: Word8, 
                       color1 :: Word8, 
                       color2 :: Word8, 
                       color3 :: Word8
                     }

--FUNCIONES AUXILIARES:
--Para aplicar los pixels

hacerMonada :: Monad m => (a -> m b) -> [a] -> m ()
hacerMonada f [] = return ()
hacerMonada f (x:xs) = f x >> (hacerMonada f xs)

--Para transformar numeros de color, en colores RBGA
darColor :: Word8 -> Word32
darColor 0 = 0xFFFFFFFF
darColor 1 = 0xC0C0C0FF
darColor 2 = 0x606060FF
darColor 3 = 0x000000FF

--Dado un registro de paleta, construye una paleta de colores de GameBoy
hacerPaleta :: Word8 -> Paleta
hacerPaleta paleta = Paleta {
                              color0 = (paleta .&. 0x3),
                              color1 = (shift (paleta .&. 0xC) (-2)),
                              color2 = (shift (paleta .&. 0x30) (-4)),
                              color3 = (shift (paleta .&. 0xC0) (-6))
                          }

--Pinta un pixel segun la paleta dada
pintar :: Paleta -> Word8 -> Word8
pintar paleta 0 = color0 paleta
pintar paleta 1 = color1 paleta
pintar paleta 2 = color2 paleta
pintar paleta 3 = color3 paleta

--Transforma los pixels, del formato al que se guardan en memoria, a pixels con coloroes de gameboy
hacerPixels :: [Word8] -> [Word8]
hacerPixels [] = []
hacerPixels (x:y: xs) = ( (shiftR (x .&. 0x80) 7 ) .|. (shiftR (y .&. 0x80) 6) ) : 
                        ( (shiftR (x .&. 0x40) 6) .|. (shiftR (y .&. 0x40) 5) ) : 
                        ( (shiftR (x .&. 0x20) 5) .|. (shiftR (y .&. 0x20) 4) ) : 
                        ( (shiftR (x .&. 0x10) 4) .|. (shiftR (y .&. 0x10) 3) ) : 
                        ( (shiftR (x .&. 0x8) 3) .|. (shiftR (y .&. 0x8) 2) ) : 
                        ( (shiftR (x .&. 0x4) 2) .|. (shiftR (y .&. 0x4) 1) ) : 
                        ( (shiftR (x .&. 0x2) 1) .|. (y .&. 0x2) ):
                        ( (x .&. 0x1) .|. (shiftL (y .&. 0x1) 1) ) : 
                        (hacerPixels xs)
hacerPixels _ = error "lista mala"
                
--Las siguentes funciones son para obtener el numero de tile (entre 0 y 383) 
--segun el tileset al que pertenecen
tilenumber1 :: Word8 -> Int
tilenumber1 n = fromIntegral n

tilenumber0 :: Word8 -> Int
tilenumber0 n = 256 + (fromIntegral ( (fromIntegral n) :: Int8) )

--Lee un tile de la memoria y lo guarda como un array de pixels
leerTile :: AddrSpace -> Int -> Tile
leerTile space n = UA.listArray ((0,0),(7,7)) (hacerPixels lista)
                where base = fromIntegral n * 16 + 0x8000
                      lista = leerEntre space base (base + 15)

--Lee todos los tiles.
tiles :: AddrSpace -> Tileset
tiles space =  fmap (leerTile space) listaT



--Toma la memoria y los tiles ya leidos, y arma el background, como un arreglo de tiles.
armarBackground :: AddrSpace -> Tileset -> Background
armarBackground space tileset  = listArray 
                                  ((0,0),(31,31)) 
                                  (map 
                                    (\n -> tileset ! (tilen n)) 
                                    (leerEntre space mapa (mapa + 0x3FF))
                                  )
                               where tileset = tiles space
                                     mapa = if (ff40 space .&. 0x8 == 0x8) then 0x9C00 else 0x9800
                                     tilen = if (ff40 space .&. 0x10 == 0x10)
                                                 then tilenumber1
                                                 else tilenumber0

--La siguiente funcion lee los pixels del Background
getPixel :: Word8 ->  Word8 -> Background -> Word8
getPixel x y arreglo = (arreglo ! (tiley,tilex)) UA.! (iny,inx)
                    where tilex = div x 8
                          tiley = div y 8
                          inx = mod x 8
                          iny = mod y 8

--Esta funcion arma el Background, como un array pixels, ya pintados con el color RGBA que corresponde
renderBackground :: AddrSpace -> Tileset -> IO Screen
renderBackground space tileset =  newListArray
                                   ((0,0),(143,159))
                                   (map
                                      (darColor  . (pintar paleta) )
                                      [getPixel x y pantalla |y <- yindices, x<-xindices]
                                   )
                             where xindices = map (+ff43 space) [0 .. 159]
                                   yindices = map (+ff42 space) [0 .. 143]
                                   paleta = hacerPaleta (ff47 space)
                                   pantalla = armarBackground space tileset

--Esta funcion pone un sprite en pantalla, o deja la pantalla igual si el argumento es Nothing
ponerSprite :: Screen -> Maybe Sprite -> IO ()
ponerSprite screen Nothing = return ()
ponerSprite screen (Just sprite)  =  hacerMonada
                                         (\i -> do bgpix <- readArray screen i 
                                                   writeArray screen 
                                                              i
                                                              (mezclar 
                                                                       bgpix
                                                                       (pixels ! (\(x,y) -> (fromIntegral x, fromIntegral y) )  i )
                                                              )
                                          )
                                          losindices
                                         
                                 where mezclar = if arriba sprite
                                                   then mezclarArriba
                                                   else mezclarAbajo
                                       pixels = pixelarray sprite
                                       losindices = map (\(x,y) -> (fromIntegral x, fromIntegral y))
                                                        (filter 
                                                              (\(y,x) -> (x>= 0) && (y>=0) && (y < 144) && (x < 160)) 
                                                              (indices pixels)
                                                        )

--Funciones para mezclar sprites con el background, arriba o abajo del mismo
mezclarArriba :: Word32 -> Word32 -> Word32
mezclarArriba bgpix spritepix | spritepix == 0 = bgpix
                              | otherwise = spritepix

mezclarAbajo :: Word32 -> Word32 -> Word32
mezclarAbajo bgpix spritepix | bgpix == 0xFFFFFFFF && spritepix == 0 = 0xFFFFFFFF
                             | bgpix == 0xFFFFFFFF = spritepix
                             | otherwise = bgpix

--Funcion analoga a pintar, pero para sprites, en este caso, el 0 se mantiene como 0 y delimita la zona transparente del sprite
pintarSprite :: Paleta -> Word8 -> Word32
pintarSprite paleta 0 = 0
pintarSprite paleta 1 = darColor (color1 paleta)
pintarSprite paleta 2 = darColor (color2 paleta)
pintarSprite paleta 3 = darColor (color3 paleta)

--Esta funcion se usa para reordenar los pixeles del tile segun lo indiquen las propiedades del sprite.
flipPix :: Bool -> Bool -> [a] -> [a]
flipPix _ _ [] = []
flipPix False False xs = xs
flipPix True True xs = reverse xs
flipPix True False (a:b:c:d:e:f:g:h:xs) = [h,g,f,e,d,c,b,a] ++ flipPix True False xs
flipPix False True (a:b:c:d:e:f:g:h:xs) = flipPix False True xs ++ [a,b,c,d,e,f,g,h]


--Dado el addrspace, el tileset, y un numero, esta funcion devuelve el sprite correspondiente a ese numero
hacerSprite8 :: AddrSpace -> Tileset -> Word8 -> Maybe Sprite
hacerSprite8 space tileset n | n >= 40  = Nothing
                             | ypos >= 144 = Nothing
                             | xpos >= 160 = Nothing
                             | ypos <= -16 = Nothing
                             | xpos <= -8 = Nothing
                             | otherwise = Just (Sprite { arriba = above, 
                                                          pixelarray = listArray ((ypos, xpos),(ypos + 7, xpos + 7)) pixels
                                                        }
                                                )
                         where 
                              addr = (0xFE00 + fromIntegral n * 4) :: Word16
                              ypos = fromIntegral (readMem8 space addr) - 16 :: Int
                              xpos = fromIntegral (readMem8 space  (addr + 1)) - 8 :: Int
                              tilenum = fromIntegral $ readMem8 space (addr + 2)
                              atributos = readMem8 space (addr + 3)
                              above = (atributos .&. 0x80 == 0)
                              yflip = (atributos .&. 0x40 == 0x40)
                              xflip = (atributos .&. 0x20 == 0x20)
                              paleta = if (atributos .&. 0x10 == 0x10)
                                          then hacerPaleta (readMem8 space 0xff49)
                                          else hacerPaleta (readMem8 space 0xff48)
                              pixels = flipPix xflip yflip (map (pintarSprite paleta) (UA.elems (tileset ! tilenum)))

--Construye todos los sprites en la OAM
hacerSprites8 :: AddrSpace -> Tileset -> [Maybe Sprite]
hacerSprites8 space tiles = map (hacerSprite8 space tiles) [ 0 .. 39]



makeScreen :: IORef Estado -> IO [Word32]
makeScreen state = do estado <- readIORef state
                      let space = addrspace estado
                      let tileset = tiles space
                      let sprites = hacerSprites8 space tileset 
                      pantalla <- renderBackground space tileset
                      if (ff40 space .&. 2 == 2) 
                         then hacerMonada (ponerSprite pantalla) sprites
                         else return ()
                      lista <- getElems pantalla
                      return lista
