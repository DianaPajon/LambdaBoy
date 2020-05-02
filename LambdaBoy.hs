module Main where
import AddrSpace
import Interrupts
import Cpu
import Data.Int
import Rom
import Render
import Data.Word
import Data.Array.Unboxed
import Data.IORef
import Data.Bits
import Foreign.Marshal.Array
import Graphics.UI.GLUT

fps = 40
clocksporframe = div 4194304 fps
frametime = div 1000 fps
archivo = "roms/drmario.gb"
ceros = 0:ceros
newcpu = CPU {
              a_reg = 0x01, f_reg = 0xB0, b_reg = 0x00, c_reg = 0x13, d_reg = 0x00, e_reg = 0xD8,
              h_reg = 0x01, l_reg = 0x4D, sp_reg = 0xFFFE, pc_reg = 0x100, ime = True,
              halted = False, freq = 4194304, ciclos = 0
             }

main :: IO ()
main = do (programname, romname:xs) <-getArgsAndInitialize
          rom <- open romname
          estado <- newIORef rom
          pad <- newIORef ((0xF, 0xF)::(Word8, Word8))
          rendimiento <- newIORef ((0,0)::(Int, Int))
          createWindow "proyectoBoy"
          windowSize $= Size 640 580
          viewport $= (Position 0 0, Size 640 580)
          loadIdentity
          ortho2D 0 640 0 580
          shadeModel $= Flat
          depthFunc $= Nothing
          cullFace $= Nothing
          dither $= Disabled
          blend $= Disabled
          displayCallback $= display estado
          keyboardMouseCallback $= Just (actualizarPad pad)
          addTimerCallback frametime (ciclar estado pad rendimiento)
          addTimerCallback 1000 (bench rendimiento)
          mainLoop

bench :: IORef (Int, Int) -> IO ()
bench rendimiento = do addTimerCallback 1000 (bench rendimiento)
                       (a,b) <- readIORef rendimiento
                       putStrLn ( (show (fromIntegral a / 4194304 * 100)) ++ "% ," ++ (show (b)) ++ "fps")
                       writeIORef rendimiento (0,0)


display :: IORef Estado -> IO ()
display estado   = do clear [ColorBuffer,DepthBuffer]
                      loadIdentity
                      rasterPos (Vertex2 (-1::GLfloat) 1)
                      pixelZoom $= (4, -4)
                      lista <- makeScreen estado
                      withArray lista $ 
                                 (\ptr-> drawPixels
                                         (Size 160 144)
                                         (PixelData RGBA UnsignedInt8888 ptr)
                                       )
                      flush

actualizarPad pad (Char 'x') Down a b = modifyIORef pad (\(x,y) -> (clearBit x 3,y))
actualizarPad pad (Char 'x') Up a b = modifyIORef pad (\(x,y) -> (setBit x 3,y))
actualizarPad pad (Char 'z') Down a b = modifyIORef pad (\(x,y) -> (clearBit x 2,y))
actualizarPad pad (Char 'z') Up a b = modifyIORef pad (\(x,y) -> (setBit x 2,y))
actualizarPad pad (Char 's') Down a b = modifyIORef pad (\(x,y) -> (clearBit x 1,y))
actualizarPad pad (Char 's') Up a b = modifyIORef pad (\(x,y) -> (setBit x 1,y))
actualizarPad pad (Char 'a') Down a b = modifyIORef pad (\(x,y) -> (clearBit x 0,y))
actualizarPad pad (Char 'a') Up a b = modifyIORef pad (\(x,y) -> (setBit x 0,y))
actualizarPad pad (Char 'q') Up a b = leaveMainLoop
actualizarPad pad (SpecialKey KeyRight) Down a b = modifyIORef pad (\(x,y) -> (x, clearBit y 0))
actualizarPad pad (SpecialKey KeyRight) Up a b = modifyIORef pad (\(x,y) -> (x, setBit y 0))
actualizarPad pad (SpecialKey KeyLeft) Down a b = modifyIORef pad (\(x,y) -> (x, clearBit y 1))
actualizarPad pad (SpecialKey KeyLeft) Up a b = modifyIORef pad (\(x,y) -> (x, setBit y 1))
actualizarPad pad (SpecialKey KeyUp) Down a b = modifyIORef pad (\(x,y) -> (x, clearBit y 2))
actualizarPad pad (SpecialKey KeyUp) Up a b = modifyIORef pad (\(x,y) -> (x, setBit y 2))
actualizarPad pad (SpecialKey KeyDown) Down a b = modifyIORef pad (\(x,y) -> (x, clearBit y 3))
actualizarPad pad (SpecialKey KeyDown) Up a b = modifyIORef pad (\(x,y) -> (x, setBit y 3))
actualizarPad a b c d e = return ()

ciclar :: IORef Estado -> IORef (Word8, Word8) -> IORef (Int, Int) -> TimerCallback
ciclar estado  pad rendimiento = do botones <- readIORef pad
                                    addTimerCallback frametime (ciclar estado pad rendimiento)
                                    modifyIORef estado (step clocksporframe botones True)
                                    modifyIORef rendimiento (\(x,y) -> (x + clocksporframe, y + 1))
                                    postRedisplay Nothing

open :: String -> IO Estado
open str = do rom_s <- openRom str
              let iram_a = listArray (0xC000, 0xDFFF) ceros
                  zram_a = listArray (0xFF80, 0xFFFE) ceros
                  tilemem = listArray (0x8000,0x97FF) ceros
                  maps = listArray (0x9800, 0x9FFF) ceros
                  oam_a = listArray (0xFE00, 0xFE9F) ceros
                   in return (
                              Estado {
                                      cpu = newcpu,
                                      addrspace = AddrSpace {
                                                              rom = rom_s,
                                                              iram = iram_a,
                                                              zram = zram_a,
                                                              tilesets = tilemem,
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

