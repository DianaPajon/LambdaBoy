import Cpu
import AddrSpace
import Interrupts
import Data.Int
import Rom
import Render
import Data.Word
import Data.Array.Diff
import Data.Char
import Foreign.Marshal.Array
import Graphics.UI.GLUT
import qualified Numeric as Num

ceros :: [Word8]
ceros = 0:ceros
showHex n = "0x" ++ (map toUpper) (Num.showHex n "")
archivo = "roms/drmario.gb"


newcpu = CPU { 
              a = 0x01, f = 0xB0, b = 0x00, c = 0x13, d = 0x00, e = 0xD8,
              h = 0x01, l = 0x4D, sp = 0xFFFE, pc = 0x100, ime = True, 
              halted = False, freq = 4194304, ciclos = 0
            }


open :: String -> IO Estado
open str = do rom_s <- openRom str
              let iram_a = listArray (0xC000, 0xDFFF) ceros
                  zram_a = listArray (0xFF80, 0xFFFE) ceros
                  tiles = listArray (0x8000,0x97FF) ceros
                  maps = listArray (0x9800, 0x9FFF) ceros
                  oam_a = listArray (0xFE00, 0xFE9F) ceros
                   in return (
                              Estado {
                                      cpu = newcpu,
                                      addrspace = AddrSpace {
                                                              rom = rom_s,
                                                              iram = iram_a,
                                                              zram = zram_a,
                                                              tilesets = tiles,
                                                              tilemaps = maps,
                                                              oam = oam_a,
                                                              arrowpad = 0xF,
                                                              buttonpad = 0xF,
                                                              ff00 = 0,
                                                              ff0f = 0,ff04 = 0,ff05 = 2,ff06 = 0,ff07 = 4,ff40 = 0,
                                                              ff41 = 2,ff42 = 0,ff43 = 0,ff44 = 0,ff45 = 0,ff46 = 0,
                                                              ff47 = 0,ff48 = 0,ff49 = 0,ff4a = 0,ff4b = 0,ffff = 0,
                                                              lcdTicks = 80,
                                                              timerTicks = 1024,
                                                              divTicks = 256
                                                           }
                                    }
                              )




time :: Estado -> Int -> Int -> Int -> IO ()
time est n a b | (n >= 4194304) = do putStrLn ("segundo ")
                                     time est 0 (a+1) b
               | (a >= b) = do putStrLn (
                                          show b ++ " segundos!, ultima instruccion:" ++
                                          showHex (readMem8 (addrspace est) (pc (cpu est) ) ) ++ " pc:" ++
                                          showHex (pc (cpu est))
                                        )
               | otherwise = time est' (n + cicl) a b
                             where
                               est' = run est
                               cicl = (ciclos $ (cpu est'))



traza :: Estado -> Int -> IO ()
traza est n = let pc_r = pc (cpu est)
                  a_r = a (cpu est)
                  b_r = b (cpu est)
                  c_r = c (cpu est)
                  d_r = d (cpu est)
                  e_r = e (cpu est)
                  h_r = h (cpu est)
                  l_r = l (cpu est)
                  f_r = f (cpu est)
                  inst = readMem8 (addrspace est) pc_r
                  est' = run est
                  sp_r = sp (cpu est)
                  tope = readMem16 (addrspace est) sp_r
                  y = ff44 (addrspace est)
                    in do putStrLn ("Inst: " ++ show n)
                          putStrLn ( "a: " ++ showHex a_r ++
                                     " b: " ++ showHex b_r ++
                                     " c: " ++ showHex c_r ++
                                     " d: " ++ showHex d_r ++
                                     " e: " ++ showHex e_r ++
                                     " h: " ++ showHex h_r ++
                                     " l: " ++ showHex l_r ++
                                     " f: " ++ showHex f_r)
                          putStrLn ( " pc: " ++ showHex pc_r ++
                                     " inst: " ++ showHex inst ++ 
                                     " sp: " ++ showHex sp_r ++ 
                                     " top: " ++ showHex tope ++
                                     " y: " ++ show y 
                                    )
                          putStrLn ""
                          traza (lcdTick est') (n + 1)






main :: IO ()
main = do est <- open archivo
          traza est 15125
