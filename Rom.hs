--Este modulo sirve sólamente para cargar ROMS de 32kb
module Rom where
import Data.Array.Unboxed
import Data.Word
import qualified Data.ByteString  as B

data ROM = ROM { 
                 romType :: Int,
                 numRoms :: Int,
                 numRams :: Int, 
		 romBank0 :: UArray Word16 Word8,
                 romBank1 :: UArray Word16 Word8,
                 ramBank :: UArray Word16 Word8,
                 romBanks :: [UArray Word16 Word8],
                 ramBanks :: [UArray Word16 Word8]
              }

readRom :: ROM -> Word16 -> Word8
readRom rom addr | (addr < 0x4000) = romBank0 rom ! addr
                 | (addr < 0x8000) = romBank1 rom ! addr
                 | (addr >= 0xA000 && addr < 0xC000) = ramBank rom ! addr
                 | otherwise = 0


writeRom :: ROM -> Word16 -> Word8 -> ROM
writeRom rom addr byte | (addr >= 0xA000 && addr < 0xC000) = rom{ramBank = ramBank rom // [(addr, byte)]}
                       | otherwise = rom

openRom :: String -> IO ROM
openRom file = do lista0 <- B.readFile file 
                  let lista = B.unpack lista0
                      bank1 = listArray (0,0x3FFF) lista :: UArray Word16 Word8
                      bank2 = listArray (0x4000,0x7FFF) (drop 0x4000 lista) :: UArray Word16 Word8
                      ram = listArray (0xA000,0xBFFF) (let ceros = 0:ceros in ceros) :: UArray Word16 Word8
                        in return (ROM 0 2 0 bank1 bank2 ram [bank1, bank2] [])

