module Interrupts where
import Cpu
import AddrSpace
import Rom
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.IORef


updateTimers :: AddrSpace -> Int -> AddrSpace
updateTimers space tick 
                 | (enabled == False) = space
                 | (timercd > 0 && divcd > 0) = space{timerTicks = timercd, divTicks = divcd}
                 | (timercd <= 0 && divcd <= 0) = space{
                                                        timerTicks = 1024,
                                                        divTicks = 256,
                                                        ff04 = newdiv,
                                                        ff05 = newtimer,
                                                        ff0f = newif
                                                       }
                 | timercd <= 0 = space{
                                        timerTicks = 1024,
                                        divTicks = divcd,
                                        ff05 = newtimer,
                                        ff0f =  newif
                                       }
                 | divcd <= 0 = space{
                                      divTicks = 256,
                                      timerTicks = timercd,
                                      ff04 = newdiv
                                     }
               where
                  divcd = divTicks space - tick
                  newif = ff0f space .|. if ff05 space == 255 then 0x4 else 0
                  enabled = (ff07 space .&. 4) == 4
                  newdiv = ff04 space + 1
                  newtimer = if ff05 space == 255 then ff06 space else ff05 space + 1
                  timercd = case ff07 space .&. 3 of
                                       0 -> timerTicks space - tick
                                       1 -> timerTicks space - 64 * tick


timerTick :: Estado -> Estado
timerTick estado = let tick = ciclos (cpu estado)
                        in estado{addrspace = id $ updateTimers (addrspace estado) tick}
{-
timerTick estado | (enabled == False) = estado
                 | (timercd > 0 && divcd > 0) = estado{addrspace= space{timerTicks = timercd, divTicks = divcd}}
                 | (timercd <= 0 && divcd <= 0) = estado{addrspace=space{timerTicks = 1024, 
                                                                         divTicks = 256,
                                                                         ff04 = newdiv, 
                                                                         ff05 = newtimer,
                                                                         ff0f = newif
                                                                        }
                                                        }
                 | timercd <= 0 = estado{addrspace=space{timerTicks = 1024,
                                                         divTicks = divcd,
                                                         ff05 = newtimer,
                                                         ff0f =  newif
                                                        }
                                        }
                 | divcd <= 0 = estado{addrspace=space{divTicks = 256,
                                                       timerTicks = timercd,
                                                       ff04 = newdiv
                                                      }
                                      }
               where
                  space = addrspace estado
                  cpu' = cpu estado
                  divcd = divTicks space - ciclos cpu'
                  newif = ff0f space .|. if ff05 space == 255 then 0x4 else 0
                  enabled = (ff07 space .&. 4) == 4
                  newdiv = ff04 space + 1
                  newtimer = if ff05 space == 255 then ff06 space else ff05 space + 1
                  timercd = case ff07 space .&. 3 of
                                       0 -> timerTicks space - ciclos cpu'
                                       1 -> timerTicks space - 64 * ciclos cpu'
                                       2 -> timerTicks space - 16 * ciclos cpu'
                                       3 -> timerTicks space - 4 * ciclos cpu'

-}
--tengo que actualizar el m clock
lcdTick :: Estado -> Estado
lcdTick estado = if (ticks > 0)
                    then estado{addrspace = space {lcdTicks = ticks}}
                    else estado{addrspace = space{ 
                                                    ff41 = nextStatusReg, 
                                                    ff44 = nextLine, 
                                                    ff0f = nextIntReg, 
                                                    lcdTicks = nextTimer
                                                   }
                        }
              where  modo =  ff41 space  .&. 0x3
                     linea = ff44 space
                     intline = ff45 space
                     ticks = lcdTicks (addrspace estado) - ciclos (cpu estado)
                     intReg = ff0f space
                     space = addrspace estado
                     nextMode = case modo of
                                       0 -> if (linea >= 143) then 1 else 2
                                       1 -> if (linea >= 153) then 2 else 1
                                       2 -> 3
                                       3 -> 0
                     nextTimer = case nextMode of
                                       0 -> 204
                                       1 -> 456
                                       2 -> 80
                                       3 -> 172
                     nextLine = case modo of
                                       0 -> linea + 1
                                       1 -> if(linea >= 153) then 0 else linea + 1
                                       2 -> linea
                                       3 -> linea
                     nextIntReg = intReg .|.
                                      if (modo == 0 && nextMode == 1) then 0x1 else 0 .|.
                                      if (linea == intline && nextLine /= linea && ff41 space .&. 40 == 40) then 0x2 else 0 .|.
                                      if (modo /= 0 && nextMode == 0 && ff41 space .&. 8 == 8) then 0x2 else 0 .|.
                                      if (modo /= 1 && nextMode == 1 && ff41 space .&. 10==10) then 0x2 else 0 .|.
                                      if (modo /= 2 && nextMode == 2 && ff41 space .&. 20==20) then 0x2 else 0

                     nextStatusReg = (ff41 space .&. 0xF8) .|.  nextMode .|. 
                                     if linea == intline && nextLine /=linea then 0x4 else 0


interrupt :: Estado -> Estado
interrupt estado | (master .&. ieflag .&. iflag .&. 0x1 == 1) = let (cpu', ad') = rst_n 0x40 (cpu estado, addrspace estado)
                                                                    addrspace' = ad'{ff0f = iflag .&. 0xFE}
                                                                     in estado{ 
                                                                                 cpu = cpu'{ime = False, halted = False}, 
                                                                                 addrspace = addrspace'
                                                                              }
                 | (master .&. ieflag .&. iflag .&. 0x2 == 2) = let (cpu', ad') = rst_n 0x48 (cpu estado, addrspace estado)
                                                                    addrspace' = ad'{ff0f = ff0f ad' .&. 0xFD}
                                                                     in estado { 
                                                                                 cpu = cpu'{ime = False, halted = False},
                                                                                 addrspace = addrspace'
                                                                               }
                 | (master .&. ieflag .&. iflag .&. 0x4 == 4) = let (cpu', ad') = rst_n 0x50 (cpu estado, addrspace estado)
                                                                    addrspace' = ad'{ff0f = iflag .&. 0xFB}
                                                                     in estado {
                                                                                 cpu = cpu'{ime = False, halted = False},
                                                                                 addrspace = addrspace'
                                                                               }

                 | (master .&. ieflag .&. iflag .&. 0x8 == 8) = estado
                 | (master .&. ieflag .&. iflag .&. 0x10 == 10) = let (cpu', ad') = rst_n 0x60 (cpu estado, addrspace estado)
                                                                      addrspace' = ad'{ff0f = iflag .&. 0xEF}
                                                                       in estado{ 
                                                                                 cpu = cpu'{ime = False, halted = False}, 
                                                                                 addrspace = addrspace'
                                                                                }
                 | True = estado
                where ieflag = ffff (addrspace estado)
                      iflag = ff0f (addrspace estado)
                      master = if ime (cpu estado) then 0xFF else 0x00



oneTick =  lcdTick . timerTick

--Cualquier similitud con NACHOS es pura coincidencia
run estado = if halted (cpu estado)
                then interrupt $ oneTick estado
                else interrupt $ oneTick $ oneInstruction estado

step :: Int -> (Word8, Word8) -> Bool ->  Estado -> Estado
step n pad int est | (n <= 0) = est
                   | otherwise = step (n - cicl) pad False est'
                 where
                    space = addrspace est
                    est' =  run est{addrspace = space{arrowpad = fst pad, 
                                                      buttonpad = snd pad, 
                                                      ff0f = if int then ff0f space .|. 0x10 else ff0f space
                                                     }
                                        }
                    cicl = (ciclos $ (cpu est'))


