module AddrSpace where
import Rom
import Data.Array.Unboxed
import Data.Word
import Data.Bits

data AddrSpace = AddrSpace {
                            --Rams:
                             rom :: !ROM,
                             iram :: !(UArray Word16 Word8),
                             zram :: !(UArray Word16 Word8),
                             tilesets :: !(UArray Word16 Word8),
                             tilemaps :: !(UArray Word16 Word8), 
                             oam :: !(UArray Word16 Word8),
                             arrowpad :: !Word8,
                             buttonpad :: !Word8,
--Puertos de IO implementados:
                             ff00 :: !Word8,
                             ff0f :: !Word8,
                             ff04 :: !Word8,
                             ff05 :: !Word8,
                             ff06 :: !Word8,
                             ff07 :: !Word8,
                             ff40 :: !Word8,
                             ff41 :: !Word8,
                             ff42 :: !Word8,
                             ff43 :: !Word8,
                             ff44 :: !Word8,
                             ff45 :: !Word8,
                             ff46 :: !Word8,
                             ff47 :: !Word8,
                             ff48 :: !Word8,
                             ff49 :: !Word8,
                             ff4a :: !Word8,
                             ff4b :: !Word8,
                             ffff :: !Word8,
--Timers para sincronizar:
                             lcdTicks :: !Int, 
                             divTicks :: !Int,
                             timerTicks :: !Int
                           }



leerEntre :: AddrSpace -> Word16 -> Word16 -> [Word8]
leerEntre space a b | (a > b) = []
                    | otherwise = (readMem8 space a) : (leerEntre space (a+1) b )

oamdma :: Word16 -> AddrSpace -> AddrSpace
oamdma addr space = space{oam = listArray (0xFE00, 0xFE9F) (leerEntre space addr (addr + 0x9C))}


readMem8 :: AddrSpace -> Word16 -> Word8
readMem8 space dir | (dir  >= 0xFF80 && dir  < 0xFFFF) = (zram space) ! dir
                   | (dir < 0x8000) = readRom (rom space) dir --Rom
                   | (dir < 0x9800) = tilesets space ! dir
                   | (dir < 0xA000) = tilemaps space ! dir
                   | (dir < 0xC000) = readRom (rom space) dir --RAM externa del cartucho
                   | (dir < 0xE000) = iram space ! dir --RAM Interna del gameboy
                   | (dir < 0xFE00) = iram space ! (dir - 0x2000) -- Shadow RAM
                   | (dir < 0xFEA0) = oam space ! dir --Sprites
                   | (dir == 0xff00) = ff00 space
                   | (dir == 0xff04) = ff04 space
                   | (dir == 0xff05) = ff05 space
                   | (dir == 0xff06) = ff06 space
                   | (dir == 0xff07) = ff07 space .|. 0xF8
                   | (dir == 0xff40) = ff40 space
                   | (dir == 0xff41) = ff41 space
                   | (dir == 0xff42) = ff42 space
                   | (dir == 0xff43) = ff43 space
                   | (dir == 0xff44) = ff44 space
                   | (dir == 0xff45) = ff45 space
                   | (dir == 0xff46) = ff46 space
                   | (dir == 0xff47) = ff47 space
                   | (dir == 0xff48) = ff48 space
                   | (dir == 0xff49) = ff49 space
                   | (dir == 0xff4a) = ff4a space
                   | (dir == 0xff4b) = ff4b space
                   | (dir == 0xFFFF) = ffff space
                   | (dir == 0xff26) = 0xF1
                   | otherwise = 0xFF

readMem16 :: AddrSpace -> Word16 -> Word16
readMem16 addr word = fromIntegral (readMem8 addr word) + shift (fromIntegral (readMem8 addr (word + 1))) 8

writeMem8 :: AddrSpace -> Word16 -> Word8 -> AddrSpace
writeMem8 space dir byte | (dir >= 0xFF80 && dir < 0xFFFF) = space{zram = zram space // [(dir, byte)]}
                         | (dir < 0x8000) = space{rom = writeRom (rom space) dir byte}
                         | (dir < 0x9800) = space{tilesets = tilesets space // [(dir, byte)]}
                         | (dir < 0xA000) = space{tilemaps = tilemaps space // [(dir, byte)]}
                         | (dir < 0xC000) = space{rom = writeRom (rom space) dir byte}
                         | (dir < 0xE000) = space{iram = (iram space) // [(dir, byte)]}
                         | (dir < 0xFE00) = space{iram = (iram space) // [(dir - 0x2000, byte)]}
                         | (dir < 0xFEA0) = space{oam = oam space //  [(dir, byte)]}
                         | (dir == 0xFF00) = if (byte .&. 0x20 == 0x20)
                                                   then space{ff00 = buttonpad space}
                                                   else space{ff00 = arrowpad space}
                         | (dir == 0xFF04) = space{ff04 = 0}
                         | (dir == 0xFF05) = space{ff05 = 0}
                         | (dir == 0xFF06) = space{ff06 = byte}
                         | (dir == 0xFF07) = space{ff07 = byte}
                         | (dir == 0xFF40) = space{ff40 = byte}
                         | (dir == 0xFF41) = space{ff41 = byte}
                         | (dir == 0xFF42) = space{ff42 = byte}
                         | (dir == 0xFF43) = space{ff43 = byte}
                         | (dir == 0xFF44) = space{ff44 = 0}
                         | (dir == 0xFF45) = space{ff45 = byte}
                         | (dir == 0xFF46) = space{oam = oam space //
                                                          (map (\n -> (0xFE00 + n, readMem8 space (fromIntegral byte*0x100 + n))))
                                                          [0 .. 0x9F]}
--                         | (dir == 0xFF46) = oamdma (fromIntegral byte * 0x100)  space
                         | (dir == 0xFF47) = space{ff47 = byte}
                         | (dir == 0xFF48) = space{ff48 = byte}
                         | (dir == 0xFF49) = space{ff49 = byte}
                         | (dir == 0xFF4A) = space{ff4a = byte}
                         | (dir == 0xFF4B) = space{ff4b = byte}
                         | (dir == 0xFFFF) = space{ffff = byte}
                         | (otherwise) = space

writeMem16 :: AddrSpace -> Word16 -> Word16 -> AddrSpace
writeMem16 addr dir word = let low = fromIntegral (word .&. 0x00FF) :: Word8
                               high = fromIntegral ((shift word (-8)) .&. 0x00FF) :: Word8
                               addr' = writeMem8 addr dir low
                                in writeMem8 addr' (dir + 1) high 
