module Cpu where
import AddrSpace
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Array
import Data.Int
-- Cada una de las anteriores funciones hace lo que su nombre describe --

data Estado = Estado { 
                      cpu :: !CPU,
                      addrspace :: !AddrSpace
                     }

data CPU = CPU {
                 a_reg :: !Word8,
                 b_reg :: !Word8,
                 c_reg :: !Word8,
                 d_reg :: !Word8,
                 e_reg :: !Word8,
                 h_reg :: !Word8,
                 l_reg :: !Word8,
                 f_reg :: !Word8,
                 pc_reg :: !Word16,
                 sp_reg :: !Word16,
                 ime :: !Bool,
                 halted :: !Bool,
                 freq :: !Int,
                 ciclos :: !Int
               }
data Registro8 = A | B | C | D | E | F | H | L
data Registro16 = AF | BC | DE | HL | SP


type Instrucción = (CPU, AddrSpace) -> (CPU, AddrSpace)

readPC :: (CPU, AddrSpace) -> (Word8, (CPU, AddrSpace))
readPC (cpu, addr) = (readMem8 addr (pc_reg cpu), (cpu{pc_reg = (pc_reg cpu) + 1}, addr))
                                 
--Implemento los flags:
flagZ :: CPU -> Bool
flagZ cpu = ((f_reg cpu) .&. 0x80) == 0x80

flagO :: CPU -> Bool
flagO cpu = ((f_reg cpu) .&. 0x40) == 0x40

flagH :: CPU -> Bool
flagH cpu = ((f_reg cpu) .&. 0x20) == 0x20

flagC :: CPU -> Bool
flagC cpu = ((f_reg cpu) .&. 0x10) == 0x10

writeFlagZ :: Bool -> CPU -> CPU
writeFlagZ x cpu = if x then cpu{f_reg = (f_reg cpu) .|. 0x80} else cpu{f_reg = (f_reg cpu) .&. (complement 0x80)}

writeFlagO :: Bool -> CPU -> CPU
writeFlagO x cpu= if x then cpu{f_reg = (f_reg cpu) .|. 0x40} else cpu{f_reg = (f_reg cpu) .&. (complement 0x40)}

writeFlagH :: Bool -> CPU -> CPU
writeFlagH x cpu = if x then cpu{f_reg = (f_reg cpu) .|. 0x20} else cpu{f_reg = (f_reg cpu) .&. (complement 0x20)}

writeFlagC :: Bool -> CPU -> CPU
writeFlagC x cpu= if x then cpu{f_reg = (f_reg cpu) .|. 0x10} else cpu{f_reg = (f_reg cpu) .&. (complement 0x10)}

--Escribir todas las flags a la vez:
writeFlags :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> CPU -> CPU
writeFlags z o h c cpu = let z' = if (z == Nothing) then flagZ cpu else fromJust z
                             o' = if (o == Nothing) then flagO cpu else fromJust o
                             h' = if (h == Nothing) then flagH cpu else fromJust h
                             c' = if (c == Nothing) then flagC cpu else fromJust c
                             in writeFlagZ z' $ writeFlagO o' $ writeFlagH h' $ writeFlagC c' cpu
--Condiciones:

cond_z :: CPU -> Bool
cond_z cpu = flagZ cpu

cond_nz :: CPU -> Bool
cond_nz cpu = not (flagZ cpu)

cond_c :: CPU -> Bool
cond_c cpu = flagC cpu

cond_nc :: CPU -> Bool
cond_nc cpu = not (flagC cpu)

type CpuCond = CPU -> Bool

--Defino las funciones para acceder / modificar los registros de 16 bit

af_reg :: CPU -> Word16
af_reg cpu = shiftL (fromIntegral (a_reg cpu)) 8 + fromIntegral (f_reg cpu)

bc_reg :: CPU -> Word16
bc_reg cpu = shiftL (fromIntegral (b_reg cpu)) 8 + fromIntegral (c_reg cpu)

de_reg :: CPU -> Word16
de_reg cpu = shiftL (fromIntegral (d_reg cpu)) 8 + fromIntegral (e_reg cpu)

hl_reg :: CPU -> Word16
hl_reg cpu = shiftL (fromIntegral (h_reg cpu)) 8 + fromIntegral (l_reg cpu)

writeBC :: CPU -> Word16 -> CPU
writeBC cpu n = cpu{ b_reg = fromIntegral ((shiftR n 8) .&. 0x00FF), c_reg = fromIntegral (n .&. 0x00FF) }

writeDE :: CPU -> Word16 -> CPU
writeDE cpu n = cpu{ d_reg = fromIntegral ((shiftR n 8) .&. 0x00FF), e_reg = fromIntegral (n .&. 0x00FF) }

writeHL :: CPU -> Word16 -> CPU
writeHL cpu n = cpu{ h_reg = fromIntegral ((shiftR n 8) .&. 0x00FF), l_reg = fromIntegral (n .&. 0x00FF) }

writeAF :: CPU -> Word16 -> CPU
writeAF cpu n = cpu{ a_reg = fromIntegral ((shiftR n 8) .&. 0x00FF), f_reg = fromIntegral (n .&. 0x00FF) }

--Empiezo a definir las instrucciones

--auxiliares
leer8 :: Registro8 -> CPU -> Word8
leer8 A = a_reg
leer8 B = b_reg
leer8 C = c_reg
leer8 D = d_reg
leer8 E = e_reg
leer8 F = f_reg
leer8 H = h_reg
leer8 L = l_reg

escribir8 :: Registro8 -> Word8 -> CPU -> CPU
escribir8 A x cpu = cpu{a_reg = x}
escribir8 B x cpu = cpu{b_reg = x}
escribir8 C x cpu = cpu{c_reg = x}
escribir8 D x cpu = cpu{d_reg = x}
escribir8 E x cpu = cpu{e_reg = x}
escribir8 F x cpu = cpu{f_reg = x}
escribir8 H x cpu = cpu{h_reg = x}
escribir8 L x cpu = cpu{l_reg = x}

aplicar8 :: Registro8 -> (Word8 -> Word8) -> CPU -> CPU
aplicar8 A funcion cpu = cpu{ a_reg = funcion (a_reg cpu) }
aplicar8 B funcion cpu = cpu{ b_reg = funcion (b_reg cpu) }
aplicar8 C funcion cpu = cpu{ c_reg = funcion (c_reg cpu) }
aplicar8 D funcion cpu = cpu{ d_reg = funcion (d_reg cpu) }
aplicar8 E funcion cpu = cpu{ e_reg = funcion (e_reg cpu) }
aplicar8 F funcion cpu = cpu{ f_reg = funcion (f_reg cpu) }
aplicar8 H funcion cpu = cpu{ h_reg = funcion (h_reg cpu) }
aplicar8 L funcion cpu = cpu{ l_reg = funcion (l_reg cpu) }

leer16 :: Registro16 -> CPU -> Word16
leer16 AF = af_reg
leer16 BC = bc_reg
leer16 DE = de_reg
leer16 HL = hl_reg
leer16 SP = sp_reg

escribir16 :: Registro16 -> CPU -> Word16 -> CPU
escribir16 AF cpu x = writeAF cpu x
escribir16 BC cpu x = writeBC cpu x
escribir16 DE cpu x = writeDE cpu x
escribir16 HL cpu x = writeHL cpu x
escribir16 sp_reg cpu x = cpu{sp_reg = x}
--Instrucciones:

--La mas difícil:
nop :: (CPU, AddrSpace) -> (CPU, AddrSpace)
nop (cpu, ad) = (cpu{ciclos = 4}, ad)

--Aritmética de 8 bits:



add_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
add_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu
                          res = (x + y)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just (res .&. 0x0F < x .&. 0x0F)
                          carry = Just (res < x)
                          cpu' = cpu{a_reg = res, ciclos = 4}
                           in (writeFlags zero oper half carry cpu', ad)


add_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
add_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = (x + y)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just (res .&. 0x0F < x .&. 0x0F)
                          carry = Just (res < x)
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

add_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
add_a_d8 (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (pc_reg cpu)
                         res = (x + y)
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just (res .&. 0x0F < x .&. 0x0F)
                         carry = Just (res < x)
                         cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                         in (writeFlags zero oper half carry cpu', ad)

adc_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
adc_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu
                          res = (x + y) + if flagC cpu then 1 else 0
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just (res .&. 0x0F < x .&. 0x0F)
                          carry = Just (res < x)
                          cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


adc_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
adc_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = (x + y) + if flagC cpu then 1 else 0
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just (res .&. 0x0F < x .&. 0x0F)
                          carry = Just (res < x)
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

adc_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
adc_a_d8 (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (pc_reg cpu)
                         res = (x + y) + if flagC cpu then 1 else 0
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just (res .&. 0x0F < x .&. 0x0F)
                         carry = Just (res < x)
                         cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)


 
sub_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
sub_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu
                          res = (x - y)
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


sub_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sub_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = (x - y)
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

sub_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sub_a_d8  (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (pc_reg cpu)
                          res = (x - y)
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)


 
 
sbc_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
sbc_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu 
                          res = x - y - if flagC cpu then 1 else 0
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


sbc_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sbc_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = x - y - if flagC cpu then 1 else 0
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

sbc_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sbc_a_d8  (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (pc_reg cpu)
                          res = x - y - if flagC cpu then 1 else 0
                          zero = Just (res == 0)
                          oper = Just True
                          half = Just (res .&. 0x0F > x .&. 0x0F)
                          carry = Just (res > x)
                          cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)
 

 
 
and_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
and_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu
                          res = (x .&. y)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just True
                          carry = Just False
                          cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


and_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
and_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = (x .&. y)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just True
                          carry = Just False
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

and_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
and_a_d8  (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (pc_reg cpu)
                          res = (x .&. y)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just True
                          carry = Just False
                          cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)


 

xor_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
xor_a_r r (cpu, ad) = let x = leer8 A cpu
                          y = leer8 r cpu
                          res = xor x y
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just False
                          cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


xor_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
xor_a_pHL (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (hl_reg cpu)
                          res = xor x  y
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just False
                          cpu' = cpu{a_reg = res, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

xor_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
xor_a_d8  (cpu, ad) = let x = leer8 A cpu
                          y = readMem8 ad (pc_reg cpu)
                          res = xor x y
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just False
                          cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)


 

or_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
or_a_r r (cpu, ad) = let x = leer8 A cpu
                         y = leer8 r cpu
                         res = x .|. y
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just False
                         carry = Just False
                         cpu' = cpu{a_reg = res, ciclos=4}
                           in (writeFlags zero oper half carry cpu', ad)


or_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
or_a_pHL (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (hl_reg cpu)
                         res = x .|.  y
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just False
                         carry = Just False
                         cpu' = cpu{a_reg = res, ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

or_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
or_a_d8  (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (pc_reg cpu)
                         res = x .|. y
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just False
                         carry = Just False
                         cpu' = cpu{a_reg = res, pc_reg = pc_reg cpu + 1, ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

cp_a_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
cp_a_r r (cpu, ad) = (writeFlags zero oper half carry cpu', ad)
                       where x = leer8 A cpu
                             y = leer8 r cpu
                             res = (x - y)
                             zero = Just (res == 0)
                             oper = Just True
                             half = Just (res .&. 0x0F > x .&. 0x0F)
                             carry = Just (res > x)
                             cpu' = cpu{ciclos=4}
                               


cp_a_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
cp_a_pHL (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (hl_reg cpu)
                         res = (x - y)
                         zero = Just (res == 0)
                         oper = Just True
                         half = Just (res .&. 0x0F > x .&. 0x0F)
                         carry = Just (res > x)
                         cpu' = cpu{ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

cp_a_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
cp_a_d8  (cpu, ad) = let x = leer8 A cpu
                         y = readMem8 ad (pc_reg cpu)
                         res = (x - y)
                         zero = Just (res == 0)
                         oper = Just True
                         half = Just (res .&. 0x0F > x .&. 0x0F)
                         carry = Just (res > x)
                         cpu' = cpu{pc_reg = pc_reg cpu + 1, ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

inc_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
inc_r reg (cpu, ad) = let x = leer8 reg cpu
                          zero = Just (x + 1 == 0)
                          oper = Just False
                          half = Just (x  == 0x0F)
                          carry = Nothing
                          cpu' = escribir8 reg (x + 1) cpu{ciclos = 4}
                           in (writeFlags zero oper half carry cpu', ad)


inc_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
inc_pHL (cpu, ad) = let x = readMem8 ad (hl_reg cpu)
                        zero = Just (x + 1 == 0)
                        oper = Just False
                        half = Just (x  == 0x0F)
                        carry = Nothing
                        cpu' = cpu{ciclos = 4}
                         in (writeFlags zero oper half carry cpu', writeMem8 ad (hl_reg cpu) (x+1) )

dec_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
dec_r reg (cpu, ad) = (writeFlags zero oper half carry cpu', ad)
                       where x = leer8 reg cpu
                             zero = Just (x - 1 == 0)
                             oper = Just True
                             half = Just (x  /= 0xF0)
                             carry = Nothing
                             cpu' = aplicar8 reg (\_ -> x - 1) cpu{ciclos = 4}


dec_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
dec_pHL (cpu, ad) = let x = readMem8 ad (hl_reg cpu)
                        zero = Just (x - 1 == 0)
                        oper = Just False
                        half = Just (x  /= 0xF0)
                        carry = Nothing
                        cpu' = cpu{ciclos = 4}
                         in (writeFlags zero oper half carry cpu', writeMem8 ad (hl_reg cpu) (x-1) )

--Aritmética de 16-bit

add_hl_rr :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
add_hl_rr reg (cpu, ad) = let x = (hl_reg cpu)
                              y = leer16 reg cpu
                              res = x + y
                              zero = Nothing
                              oper = Nothing
                              half = Nothing
                              carry = Just (res < x)
                              cpu' = (writeHL cpu res){ciclos = 8}
                               in (writeFlags zero oper half carry cpu', ad)

add_SP_d8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
add_SP_d8 (cpu, ad) = let x = (sp_reg cpu)
                          y = readMem8 ad (pc_reg cpu)
                          res = if (y < 0x80) then (x + fromIntegral y) else  (x - (256 - fromIntegral y))
                          zero = Just False
                          oper = Just False
                          half = Nothing
                          carry = Just (res < x) 
                          cpu' = cpu{ sp_reg = res, ciclos = 16, pc_reg = pc_reg cpu + 1}
                            in (writeFlags zero oper half carry cpu', ad)

inc_rr :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
inc_rr reg (cpu, ad) = ((escribir16 reg cpu (leer16 reg cpu + 1)){ciclos = 8}, ad)


dec_rr :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
dec_rr reg (cpu, ad) = ((escribir16 reg cpu (leer16 reg cpu - 1)){ciclos = 8}, ad)

--Operaciones de Bits

bit_n_r :: Int -> Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
bit_n_r n reg (cpu, ad) = let zero = Just (testBit (leer8 reg cpu) n == False)
                              oper = Just False
                              half = Just True
                              carry = Nothing
                               in (writeFlags zero oper half carry cpu{ciclos = 8}, ad)

bit_n_pHL :: Int -> (CPU, AddrSpace) -> (CPU, AddrSpace)
bit_n_pHL n (cpu, ad) = let zero = Just (readMem8 ad (hl_reg cpu) .&. shift 1 n == 0)
                            oper = Just False
                            half = Just True
                            carry = Nothing
                             in (writeFlags zero oper half carry cpu{ciclos = 16}, ad)

ccf :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ccf (cpu, ad) = let zero = Nothing
                    oper = Just False
                    half = Just False
                    carry = Just (if flagC cpu then False else True)
                     in (writeFlags zero oper half carry cpu{ciclos=4}, ad)

cpl :: (CPU, AddrSpace) -> (CPU, AddrSpace)
cpl (cpu, ad) = let cpu' = cpu{a_reg = complement (a_reg cpu), ciclos=4}
                    zero = Nothing
                    oper = Just True
                    half = Just True
                    carry = Nothing
                     in ((writeFlags zero oper half carry cpu'), ad)


res_n_r :: Int -> Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
res_n_r n reg (cpu, ad) = let aux = complement (shift 1 n)
                              cpu' = aplicar8 reg (\x -> x .&. aux) cpu
                               in (cpu'{ciclos = 8}, ad)

res_n_pHL :: Int -> (CPU, AddrSpace) -> (CPU, AddrSpace)
res_n_pHL n (cpu, ad) = let aux = complement (shift 1 n)
                             in (cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu)  (readMem8 ad (hl_reg cpu) .&. aux))

set_n_r :: Int -> Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
set_n_r n reg (cpu, ad) = let res = (leer8 reg cpu) .|. (shift 1 n)
                              cpu' = escribir8 reg res cpu
                               in (cpu'{ciclos = 8}, ad)


set_n_pHL :: Int -> (CPU, AddrSpace) -> (CPU, AddrSpace)
set_n_pHL n (cpu, ad) = let res = (readMem8 ad (hl_reg cpu)) .|. (shift 1 n)
                             in(cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu) res)

daa :: (CPU, AddrSpace) -> (CPU, AddrSpace) --Con suerte, nunca tendré que probar un rom que use esta instrucción
daa (cpu, ad) = let accu = (a_reg cpu) +(if (a_reg cpu) .&. 0xf >= 0xa || flagH cpu
                                      then if flagO cpu then -0x6 else 0x6
                                      else 0)
                                   + if (a_reg cpu) .&. 0xf0 >= 0xa0 || flagC cpu
                                      then if flagO cpu then -0x60 else 0x60
                                      else 0
                    zero = Just (accu == 0)
                    oper = Nothing
                    half = Just False
                    carry = Just ( if ( ((a_reg cpu) + (if (a_reg cpu) .&. 0xf >= 0xa || flagH cpu
                                               then if flagO cpu then -0x6 else 0x6
                                               else 0)) .&. 0xf0 >= 0xa0 || flagC cpu) then True else False)
                     in (writeFlags zero oper half carry cpu{a_reg = accu, ciclos = 4}, ad)

rl_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
rl_r reg (cpu, ad) = let res = (rotate (leer8 reg cpu) 1) .&. if (flagC cpu) then 0xFF else 0xFE
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just False
                         carry = Just ((leer8 reg cpu) .&. 0x80 == 0x80)
                         cpu' = escribir8 reg res cpu{ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

rl_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
rl_pHL  (cpu, ad) = let res = (rotate (readMem8 ad (hl_reg cpu)) 1) .&. if (flagC cpu) then 0xFF else 0xFE
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x80 == 0x80)
                         in (writeFlags zero oper half carry cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu) res)



rlc_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
rlc_r reg (cpu, ad) = let res = (rotate (leer8 reg cpu) 1)
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just ((leer8 reg cpu) .&. 0x80 == 0x80)
                          cpu' = escribir8 reg res cpu{ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

rlc_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
rlc_pHL (cpu, ad) = let res = (rotate (readMem8 ad (hl_reg cpu)) 1)
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x80 == 0x80)
                         in (writeFlags zero oper half carry cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu) res)

rr_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
rr_r reg (cpu, ad) = let resu = (rotate (leer8 reg cpu) (-1)) .&. (if (flagC cpu) then 0xFF else 0x7F)
                         zero = Just (resu == 0)
                         oper = Just False
                         half = Just False
                         carry = Just ((leer8 reg cpu) .&. 0x1== 0x1)
                         cpu' = escribir8 reg resu cpu{ciclos = 8}
                          in (writeFlags zero oper half carry cpu', ad)

rr_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
rr_pHL  (cpu, ad) = let res = (rotate (readMem8 ad (hl_reg cpu)) (-1)) .&. if flagC cpu then 0xFF else 0x7F
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x1 == 0x1)
                         in (writeFlags zero oper half carry cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu) res)

rrc_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
rrc_r reg (cpu, ad) = let res = (rotate (leer8 reg cpu) (-1))
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just ((leer8 reg cpu) .&. 0x1 == 0x1)
                          cpu' = escribir8 reg res cpu{ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

rrc_pHL ::  (CPU, AddrSpace) -> (CPU, AddrSpace)
rrc_pHL (cpu, ad) = let res = (rotate (readMem8 ad (hl_reg cpu)) (-1))
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x1 == 0x1)
                         in (writeFlags zero oper half carry cpu{ciclos = 16}, writeMem8 ad (hl_reg cpu) res)

sla_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
sla_r reg (cpu, ad) = let res = (shift (leer8 reg cpu) 1) .&. 0xFE
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just (leer8 reg cpu .&. 0x80 ==  0x80)
                          cpu' = escribir8 reg res cpu{ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

sla_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sla_pHL (cpu, ad) = let res = (shift (readMem8 ad (hl_reg cpu)) 1) .&. 0xFE
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x80 ==  0x80)
                        cpu' = cpu{ciclos = 16}
                         in (writeFlags zero oper half carry cpu', writeMem8 ad (hl_reg cpu) res)

sra_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
sra_r reg (cpu, ad) = let msb = ((leer8 reg cpu) .&. 0x80 == 0x80)
                          res = if msb
                                   then (shift (leer8 reg cpu) (-1)) .|. 0x80
                                   else (shift (leer8 reg cpu) (-1)) .&. 0x7F
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just ((leer8 reg cpu) .&. 0x1 == 0x1)
                          cpu' = escribir8 reg res cpu{ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

sra_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
sra_pHL (cpu, ad) = let msb = ((readMem8 ad (hl_reg cpu)) .&. 0x80) == 0x80
                        res = if msb
                                    then (shift (readMem8 ad (hl_reg cpu)) (-1)) .|. 0x80 
                                    else (shift (readMem8 ad (hl_reg cpu)) (-1)) .&. 0x7F
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x1 == 0x1)
                        cpu' = cpu{ciclos = 16}
                         in (writeFlags zero oper half carry cpu', writeMem8 ad (hl_reg cpu) res)

srl_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
srl_r reg (cpu, ad) = let res = (shift (leer8 reg cpu) (-1)) .&. 0x7F
                          zero = Just (res == 0)
                          oper = Just False
                          half = Just False
                          carry = Just (leer8 reg cpu .&. 0x01 ==  0x01)
                          cpu' = escribir8 reg res cpu{ciclos = 8}
                           in (writeFlags zero oper half carry cpu', ad)

srl_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
srl_pHL (cpu, ad) = let res = (shift (readMem8 ad (hl_reg cpu)) (-1)) .&. 0x7F
                        zero = Just (res == 0)
                        oper = Just False
                        half = Just False
                        carry = Just ((readMem8 ad (hl_reg cpu)) .&. 0x01 ==  0x01)
                        cpu' = cpu{ciclos = 16}
                         in (writeFlags zero oper half carry cpu', writeMem8 ad (hl_reg cpu) res)



swap_r :: Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
swap_r reg (cpu, ad) = let val = leer8 reg cpu
                           res = (shift (val .&. 0x0F) 4) + 0x0F .&. (shift val (-4))
                           zero = Just (res == 0)
                           oper = Just False
                           half = Just False
                           carry = Just False
                           cpu' = escribir8 reg res cpu{ciclos = 8}
                            in (writeFlags zero oper half carry cpu', ad)


swap_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
swap_pHL (cpu, ad) = let val = readMem8 ad (hl_reg cpu)
                         res = (shift (val .&. 0x0F) 4) + 0x0F .&. (shift val (-4))
                         zero = Just (res == 0)
                         oper = Just False
                         half = Just False
                         carry = Just False
                         cpu' = cpu{ciclos = 16}
                          in (cpu', writeMem8 ad (hl_reg cpu) res)

--Stack:

push_rr :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
push_rr reg (cpu, ad) = (cpu{ciclos = 16, sp_reg = sp_reg cpu - 2}, writeMem16 ad (sp_reg cpu - 2) (leer16 reg cpu))

pop_rr :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
pop_rr reg (cpu, ad) = let cpu' = escribir16 reg cpu (readMem16 ad (sp_reg cpu))
                           in (cpu'{ciclos = 16, sp_reg = sp_reg cpu + 2}, ad)

--Instrucción auxiliar para manejar stack

push :: Word16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
push word (cpu, ad) = (cpu{sp_reg = sp_reg cpu - 2}, writeMem16 ad (sp_reg cpu - 2) word)

--Jumps, calls, returns:
jp_a16 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
jp_a16 (cpu, ad) = (cpu{ciclos = 16, pc_reg = addr}, ad) 
                     where low = readMem8 ad (pc_reg cpu)
                           high = readMem8 ad (pc_reg cpu + 1)
                           addr  = ((fromIntegral low) :: Word16) .|. (fromIntegral high)*256


jp_cc_a16 :: CpuCond -> (CPU, AddrSpace) -> (CPU, AddrSpace)
jp_cc_a16 cond (cpu, ad) = if cond cpu 
                              then jp_a16 (cpu, ad)
                              else (cpu{ciclos = 12, pc_reg = (pc_reg cpu) + 2}, ad)

jp_pHL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
jp_pHL (cpu, ad) = (cpu{pc_reg = hl_reg cpu, ciclos = 4}, ad)

jr_a8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
jr_a8 (cpu, ad) = (cpu{ciclos = 12, pc_reg = newPC}, ad)
                  where word = readMem8 ad (pc_reg cpu)
                        offset = fromIntegral word :: Int8
                        newPC = fromIntegral (fromIntegral (pc_reg cpu) + fromIntegral offset + 1:: Int) :: Word16


jr_cc_a8 :: CpuCond -> (CPU, AddrSpace) -> (CPU, AddrSpace)
jr_cc_a8 cond (cpu, ad) = if cond cpu
                             then jr_a8 (cpu, ad)
                             else (cpu{ciclos = 8, pc_reg = pc_reg cpu + 1}, ad)

ret :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ret (cpu, ad) = let addr = readMem16 ad (sp_reg cpu) 
                     in (cpu{pc_reg = addr, sp_reg = sp_reg cpu + 2, ciclos = 16}, ad)

ret_cc :: CpuCond -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ret_cc cond (cpu, ad) = if cond cpu
                           then let addr = hl_reg (fst (pop_rr HL (cpu, ad)))
                                  in (cpu{pc_reg = addr, sp_reg = sp_reg cpu + 2, ciclos = 20}, ad)
                           else (cpu{ciclos = 8}, ad)

reti :: (CPU, AddrSpace) -> (CPU, AddrSpace)
reti (cpu, ad) = let (cpu', ad') = ret (cpu, ad)
                      in  (cpu'{ime = True}, ad')

call :: (CPU, AddrSpace) -> (CPU, AddrSpace)
call (cpu, ad) = let next = pc_reg cpu + 2
                     (cpu', ad') = push next (cpu, ad)
                     low = readMem8 ad (pc_reg cpu)
                     high = readMem8 ad (pc_reg cpu + 1)
                     addr = fromIntegral low + shift  (fromIntegral high) 8:: Word16
                      in(cpu'{pc_reg = addr, ciclos = 24}, ad')

call_cc :: CpuCond -> (CPU, AddrSpace) -> (CPU, AddrSpace)
call_cc cond (cpu, ad) = if cond cpu
                            then call (cpu, ad)
                            else (cpu{ciclos = 12, pc_reg = pc_reg cpu + 2}, ad)

rst_n :: Word8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
rst_n word (cpu, ad) = let (cpu', ad') = push (pc_reg cpu) (cpu, ad)
                            in (cpu'{pc_reg = fromIntegral word, ciclos=16}, ad')

--Loads:

ld_r_r :: Registro8 -> Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_r_r regd rego (cpu, ad) = let cpu' = escribir8 regd  (leer8 rego cpu) cpu
                                 in (cpu'{ciclos = 4}, ad)

ld_r_pRR :: Registro8 -> Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_r_pRR reg reg16 (cpu, ad) = let val = readMem8 ad (leer16 reg16 cpu)
                                   in (escribir8 reg val cpu{ciclos = 8}, ad)

ld_pRR_r :: Registro16 -> Registro8 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pRR_r reg16 reg8 (cpu, ad) = (cpu{ciclos = 8}, writeMem8 ad (leer16 reg16 cpu) (leer8 reg8 cpu))

ld_pRR_d8 :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pRR_d8 reg (cpu, ad) = let val = readMem8 ad (pc_reg cpu)
                              ad' = writeMem8 ad (leer16 reg cpu) val
                               in(cpu{ciclos = 12, pc_reg = pc_reg cpu + 1}, ad')

ld_r_d8 :: Registro8 -> (CPU, AddrSpace)  -> (CPU, AddrSpace)
ld_r_d8 reg (cpu, ad) = let val = readMem8 ad (pc_reg cpu)
                             in(escribir8 reg val cpu{ciclos = 8, pc_reg = pc_reg cpu + 1}, ad)

ld_pa16_a :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pa16_a (cpu, ad) = let low = readMem8 ad (pc_reg cpu)
                          high = readMem8 ad (pc_reg cpu + 1)
                          addr = fromIntegral low + shift (fromIntegral high) 8:: Word16
                           in(cpu{ciclos = 8, pc_reg = pc_reg cpu + 2}, writeMem8 ad addr (a_reg cpu))

ld_a_pa16 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_a_pa16 (cpu, ad) = let low = readMem8 ad (pc_reg cpu)
                          high = readMem8 ad (pc_reg cpu + 1)
                          addr = fromIntegral low + shift (fromIntegral high) 8 :: Word16
                          val = readMem8 ad addr
                           in(cpu{a_reg = val, ciclos = 16, pc_reg = pc_reg cpu + 2}, ad)

ld_a_pC :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_a_pC (cpu, ad) = let val = readMem8 ad (0xFF00 + (fromIntegral (c_reg cpu)))
                         in(cpu{a_reg = val, ciclos = 8}, ad)






ld_a_pHLI :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_a_pHLI (cpu, ad) = let val = readMem8 ad (hl_reg cpu)
                          cpu' = escribir16 HL cpu (hl_reg cpu + 1)
                           in (cpu'{ciclos = 8, a_reg = val}, ad)

ld_a_pHLD :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_a_pHLD (cpu, ad) = let val = readMem8 ad (hl_reg cpu)
                          cpu' = escribir16 HL cpu (hl_reg cpu - 1)
                           in (cpu'{ciclos=8, a_reg = val}, ad)

ld_pC_a :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pC_a (cpu, ad) = (cpu{ciclos = 8}, writeMem8 ad (0xFF00 + fromIntegral (c_reg cpu)) (a_reg cpu))

ld_pHLI_a :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pHLI_a (cpu, ad) = (escribir16 HL  cpu{ciclos = 8} (hl_reg cpu + 1), writeMem8 ad (hl_reg cpu) (a_reg cpu))

ld_pHLD_a :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pHLD_a (cpu, ad) = (escribir16 HL  cpu{ciclos = 8} (hl_reg cpu - 1), writeMem8 ad (hl_reg cpu) (a_reg cpu))


ld_rr_d16 :: Registro16 -> (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_rr_d16 reg (cpu, ad) = let val = readMem16 ad (pc_reg cpu)
                               in (escribir16 reg cpu{ciclos = 12, pc_reg = pc_reg cpu + 2} val, ad)

ld_SP_HL :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_SP_HL (cpu, ad) = (cpu{sp_reg = hl_reg cpu, ciclos = 8}, ad)

ld_pa16_SP :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ld_pa16_SP (cpu, ad) = let low = readMem8 ad (pc_reg cpu)
                           high = readMem8 ad (pc_reg cpu)
                           addr = fromIntegral low + shift (fromIntegral high) 8 :: Word16
                            in(cpu{ciclos=20, pc_reg = pc_reg cpu + 2}, writeMem16 ad addr (sp_reg cpu))

ldh_pa8_a :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ldh_pa8_a (cpu, ad) = let addr = 0xFF00 + (fromIntegral (readMem8 ad (pc_reg cpu)) :: Word16 )
                           in(cpu{ciclos=12, pc_reg = pc_reg cpu +1}, writeMem8 ad addr (a_reg cpu))


ldh_a_pa8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ldh_a_pa8 (cpu, ad) = (cpu{a_reg = newA, ciclos = 12, pc_reg = pc_reg cpu + 1 }, ad)
                    where addr = 0xFF00 + fromIntegral (readMem8 ad (pc_reg cpu))
                          newA = readMem8 ad addr

ldhl_SP_a8 :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ldhl_SP_a8 (cpu, ad) = let offset = fromIntegral (readMem8 ad (pc_reg cpu)) :: Int8
                           newsp =  (fromIntegral (fromIntegral (sp_reg cpu) + offset)) :: Word16
                           zero = Just False
                           oper = Just False
                           half = Just (newsp .&. 0x000F < (sp_reg cpu) .&. 0x000F)
                           carry = Just (newsp < (sp_reg cpu))
                           cpu' = writeFlags zero oper half carry (writeHL cpu newsp)
                            in(cpu'{ciclos = 12, pc_reg = pc_reg cpu + 1}, ad)

--Control:
scf :: (CPU, AddrSpace) -> (CPU, AddrSpace)
scf (cpu, ad) = (writeFlags Nothing (Just False) (Just False) (Just True) cpu{ciclos = 4}, ad)

di :: (CPU, AddrSpace) -> (CPU, AddrSpace)
di (cpu, ad) = (cpu{ciclos = 4, ime = False}, ad)

ei :: (CPU, AddrSpace) -> (CPU, AddrSpace)
ei (cpu, ad) = (cpu{ciclos = 4, ime = True}, ad)

halt :: (CPU, AddrSpace) -> (CPU, AddrSpace)
halt (cpu, ad) = (cpu{halted = True, ime = True, ciclos = 4}, ad)

cb :: (CPU, AddrSpace) -> (CPU, AddrSpace)
cb s = let (w, s') = readPC s
             in (instruccionesCB ! w) s'

stop :: (CPU, AddrSpace) -> (CPU, AddrSpace)
stop (cpu,ad)= error "No implementada"


-- \o/ w00t \o/ --

lista :: [Instrucción]
lista =   [nop, --0x00
           ld_rr_d16 BC, 
           ld_pRR_r BC A,
           inc_rr BC, 
           inc_r B, 
           dec_r B, 
           ld_r_d8 B,
           rlc_r A,
           ld_pa16_SP,
           add_hl_rr BC,
           ld_r_pRR A BC,
           dec_rr BC, 
           inc_r C, 
           dec_r C, 
           ld_r_d8 C, 
           rrc_r A,
           stop, --0x10
           ld_rr_d16 DE,
           ld_pRR_r DE A,
           inc_rr DE,
           inc_r D, 
           dec_r D, 
           ld_r_d8 D, 
           rl_r A,
           jr_a8,
           add_hl_rr DE,
           ld_r_pRR A DE,
           dec_rr DE, 
           inc_r E, 
           dec_r E, 
           ld_r_d8 E, 
           rr_r A,
           jr_cc_a8 cond_nz, --0x20
           ld_rr_d16 HL,
           ld_pHLI_a,
           inc_rr HL, 
           inc_r H, 
           dec_r H,
           ld_r_d8 H,
           daa, 
           jr_cc_a8 cond_z,
           add_hl_rr HL, 
           ld_a_pHLI, 
           dec_rr HL,
           inc_r L,
           dec_r L,
           ld_r_d8 L,
           cpl, 
           jr_cc_a8 cond_nc, --0x30
           ld_rr_d16 SP,
           ld_pHLD_a, 
           inc_rr SP,
           inc_pHL, 
           dec_pHL,
           ld_pRR_d8 HL,
           scf,
           jr_cc_a8 cond_c,
           add_hl_rr SP,
           ld_a_pHLD,
           dec_rr SP, 
           inc_r A,
           dec_r A, 
           ld_r_d8 A, 
           ccf,
           ld_r_r B B, --0x40
           ld_r_r B C,
           ld_r_r B D,
           ld_r_r B E, 
           ld_r_r B H,
           ld_r_r B L,
           ld_r_pRR B HL,
           ld_r_r B A,
           ld_r_r C B, 
           ld_r_r C C,
           ld_r_r C D,
           ld_r_r C E,
           ld_r_r C H,
           ld_r_r C L,
           ld_r_pRR C HL,
           ld_r_r C A,
           ld_r_r D B, --0x50
           ld_r_r D C,
           ld_r_r D D,
           ld_r_r D E,
           ld_r_r D H,
           ld_r_r D L,
           ld_r_pRR D HL,
           ld_r_r D A,
           ld_r_r E B,
           ld_r_r E C,
           ld_r_r E D,
           ld_r_r E E,
           ld_r_r E H,
           ld_r_r E L,
           ld_r_pRR E HL,
           ld_r_r E A,
           ld_r_r H B, --0x60
           ld_r_r H C,
           ld_r_r H D,
           ld_r_r H E,
           ld_r_r H H,
           ld_r_r H L,
           ld_r_pRR H HL,
           ld_r_r H A,
           ld_r_r L B,
           ld_r_r L C,
           ld_r_r L D,
           ld_r_r L E,
           ld_r_r L H,
           ld_r_r L L,
           ld_r_pRR L HL,
           ld_r_r L A,
           ld_pRR_r HL B,--0x70
           ld_pRR_r HL C,
           ld_pRR_r HL D,
           ld_pRR_r HL E,
           ld_pRR_r HL H,
           ld_pRR_r HL L,
           halt,
           ld_pRR_r HL A,
           ld_r_r A B,
           ld_r_r A C,
           ld_r_r A D,
           ld_r_r A E,
           ld_r_r A H,
           ld_r_r A L,
           ld_r_pRR A HL,
           ld_r_r A A,
           add_a_r B, --0x80
           add_a_r C,
           add_a_r D,
           add_a_r E,
           add_a_r H,
           add_a_r L,
           add_a_pHL,
           add_a_r A,
           adc_a_r B,
           adc_a_r C,
           adc_a_r D,
           adc_a_r E,
           adc_a_r H,
           adc_a_r L,
           adc_a_pHL,
           adc_a_r A,
           sub_a_r B, --0x90
           sub_a_r C,
           sub_a_r D,
           sub_a_r E,
           sub_a_r H,
           sub_a_r L,
           sub_a_pHL,
           sub_a_r A,
           sbc_a_r B,
           sbc_a_r C,
           sbc_a_r D,
           sbc_a_r E,
           sbc_a_r H,
           sbc_a_r L,
           sbc_a_pHL,
           sbc_a_r A,
           and_a_r B, --0xA0
           and_a_r C,
           and_a_r D,
           and_a_r E,
           and_a_r H,
           and_a_r L,
           and_a_pHL,
           and_a_r A,
           xor_a_r B,
           xor_a_r C,
           xor_a_r D,
           xor_a_r E,
           xor_a_r H,
           xor_a_r L,
           xor_a_pHL,
           xor_a_r A,
           or_a_r B, --0xB0
           or_a_r C,
           or_a_r D,
           or_a_r E,
           or_a_r H,
           or_a_r L,
           or_a_pHL,
           or_a_r A,
           cp_a_r B,
           cp_a_r C,
           cp_a_r D,
           cp_a_r E,
           cp_a_r H,
           cp_a_r L,
           cp_a_pHL,
           cp_a_r A,
           ret_cc cond_nz, --0xC0
           pop_rr BC,
           jp_cc_a16 cond_nz,
           jp_a16,
           call_cc cond_nz,
           push_rr BC,
           add_a_d8,
           rst_n 0,
           ret_cc cond_z,
           ret,
           jp_cc_a16 cond_z,
           cb,
           call_cc cond_z,
           call,
           adc_a_d8,
           rst_n 0x08,
           ret_cc cond_nc, --0xD0
           pop_rr DE,
           jp_cc_a16 cond_nc,
           error "Instrucción inválida 0xD3",
           call_cc cond_nc,
           push_rr DE,
           sub_a_d8,
           rst_n 0x10,
           ret_cc cond_c,
           reti,
           jp_cc_a16 cond_c,
           error "Instrucción inválida 0xDB",
           call_cc cond_c,
           (\(cpu, ad) -> error ("Instrucción inválida 0xDD" ++ " pc: " ++ show (pc_reg cpu))),
           sbc_a_d8,
           rst_n 0x18,
           ldh_pa8_a, --0xE0
           pop_rr HL,
           ld_pC_a,
           error "Instrucción inválida 0xE3",
           error "Instrucción inválida 0xE4",
           push_rr HL,
           and_a_d8,
           rst_n 0x20,
           add_SP_d8,
           jp_pHL,
           ld_pa16_a,
           error "Instrucción inválida 0xEB",
           error "Instrucción inválida 0xEC",
           error "Instrucción inválida 0xED", 
           xor_a_d8,
           rst_n 0x28,
           ldh_a_pa8, --0xF0
           pop_rr AF,
           ld_a_pC,
           di,
           error "Instrucción inválida 0xF4",
           push_rr AF,
           or_a_d8,
           rst_n 0x30,
           ldhl_SP_a8,
           ld_SP_HL,
           ld_a_pa16,
           ei,
           error "Instrucción inválida 0xFC",
           error "Instrucción inválida 0xFD",
           cp_a_d8,
           rst_n 0x38]

instruccionesbase = listArray (0,255) lista :: Array Word8 Instrucción

listaCB :: [Instrucción]
listaCB = [ rlc_r B, --0x00
            rlc_r C, 
            rlc_r D,
            rlc_r E,
            rlc_r H,
            rlc_r L,
            rlc_pHL,
            rlc_r A,
            rrc_r B,
            rrc_r C,
            rrc_r D,
            rrc_r E,
            rrc_r H,
            rrc_r L,
            rrc_pHL,
            rrc_r A,
            rl_r B, --0x10
            rl_r C,
            rl_r D,
            rl_r E,
            rl_r H,
            rl_r L,
            rl_pHL,
            rl_r A,
            rr_r B,
            rr_r C,
            rr_r D,
            rr_r E,
            rr_r H,
            rr_r L,
            rr_pHL,
            rr_r A,
            sla_r B, --0x20
            sla_r C,
            sla_r D,
            sla_r E,
            sla_r H,
            sla_r L,
            sla_pHL,
            sla_r A,
            sra_r B,
            sra_r C,
            sra_r D,
            sra_r E,
            sra_r H,
            sra_r L,
            sra_pHL,
            sra_r A,
            swap_r B, --0x30
            swap_r C,
            swap_r D,
            swap_r E,
            swap_r H,
            swap_r L,
            swap_pHL,
            swap_r A,
            srl_r B,
            srl_r C,
            srl_r D,
            srl_r E,
            srl_r H,
            srl_r L,
            srl_pHL,
            srl_r A,
            bit_n_r 0 B,
            bit_n_r 0 C,
            bit_n_r 0 D,
            bit_n_r 0 E,
            bit_n_r 0 H,
            bit_n_r 0 L,
            bit_n_pHL 0,
            bit_n_r 0 A,
            bit_n_r 1 B,
            bit_n_r 1 C,
            bit_n_r 1 D,
            bit_n_r 1 E,
            bit_n_r 1 H,
            bit_n_r 1 L,
            bit_n_pHL 1,
            bit_n_r 1 A,
            bit_n_r 2 B,
            bit_n_r 2 C,
            bit_n_r 2 D,
            bit_n_r 2 E,
            bit_n_r 2 H,
            bit_n_r 2 L,
            bit_n_pHL 2,
            bit_n_r 2 A,
            bit_n_r 3 B,
            bit_n_r 3 C,
            bit_n_r 3 D,
            bit_n_r 3 E,
            bit_n_r 3 H,
            bit_n_r 3 L,
            bit_n_pHL 3,
            bit_n_r 3 A,
            bit_n_r 4 B,
            bit_n_r 4 C,
            bit_n_r 4 D,
            bit_n_r 4 E,
            bit_n_r 4 H,
            bit_n_r 4 L,
            bit_n_pHL 4,
            bit_n_r 4 A,
            bit_n_r 5 B,
            bit_n_r 5 C,
            bit_n_r 5 D,
            bit_n_r 5 E,
            bit_n_r 5 H,
            bit_n_r 5 L,
            bit_n_pHL 5,
            bit_n_r 5 A,
            bit_n_r 6 B,
            bit_n_r 6 C,
            bit_n_r 6 D,
            bit_n_r 6 E,
            bit_n_r 6 H,
            bit_n_r 6 L,
            bit_n_pHL 6,
            bit_n_r 6 A,
            bit_n_r 7 B,
            bit_n_r 7 C,
            bit_n_r 7 D,
            bit_n_r 7 E,
            bit_n_r 7 H,
            bit_n_r 7 L,
            bit_n_pHL 7,
            bit_n_r 7 A,
            res_n_r 0 B,
            res_n_r 0 C,
            res_n_r 0 D,
            res_n_r 0 E,
            res_n_r 0 H,
            res_n_r 0 L,
            res_n_pHL 0,
            res_n_r 0 A,
            res_n_r 1 B,
            res_n_r 1 C,
            res_n_r 1 D,
            res_n_r 1 E,
            res_n_r 1 H,
            res_n_r 1 L,
            res_n_pHL 1,
            res_n_r 1 A,
            res_n_r 2 B,
            res_n_r 2 C,
            res_n_r 2 D,
            res_n_r 2 E,
            res_n_r 2 H,
            res_n_r 2 L,
            res_n_pHL 2,
            res_n_r 2 A,
            res_n_r 3 B,
            res_n_r 3 C,
            res_n_r 3 D,
            res_n_r 3 E,
            res_n_r 3 H,
            res_n_r 3 L,
            res_n_pHL 3,
            res_n_r 3 A,
            res_n_r 4 B,
            res_n_r 4 C,
            res_n_r 4 D,
            res_n_r 4 E,
            res_n_r 4 H,
            res_n_r 4 L,
            res_n_pHL 4,
            res_n_r 4 A,
            res_n_r 5 B,
            res_n_r 5 C,
            res_n_r 5 D,
            res_n_r 5 E,
            res_n_r 5 H,
            res_n_r 5 L,
            res_n_pHL 5,
            res_n_r 5 A,
            res_n_r 6 B,
            res_n_r 6 C,
            res_n_r 6 D,
            res_n_r 6 E,
            res_n_r 6 H,
            res_n_r 6 L,
            res_n_pHL 6,
            res_n_r 6 A,
            res_n_r 7 B,
            res_n_r 7 C,
            res_n_r 7 D,
            res_n_r 7 E,
            res_n_r 7 H,
            res_n_r 7 L,
            res_n_pHL 7,
            res_n_r 7 A,
            set_n_r 0 B,
            set_n_r 0 C,
            set_n_r 0 D,
            set_n_r 0 E,
            set_n_r 0 H,
            set_n_r 0 L,
            set_n_pHL 0,
            set_n_r 0 A,
            set_n_r 1 B,
            set_n_r 1 C,
            set_n_r 1 D,
            set_n_r 1 E,
            set_n_r 1 H,
            set_n_r 1 L,
            set_n_pHL 1,
            set_n_r 1 A,
            set_n_r 2 B,
            set_n_r 2 C,
            set_n_r 2 D,
            set_n_r 2 E,
            set_n_r 2 H,
            set_n_r 2 L,
            set_n_pHL 2,
            set_n_r 2 A,
            set_n_r 3 B,
            set_n_r 3 C,
            set_n_r 3 D,
            set_n_r 3 E,
            set_n_r 3 H,
            set_n_r 3 L,
            set_n_pHL 3,
            set_n_r 3 A,
            set_n_r 4 B,
            set_n_r 4 C,
            set_n_r 4 D,
            set_n_r 4 E,
            set_n_r 4 H,
            set_n_r 4 L,
            set_n_pHL 4,
            set_n_r 4 A,
            set_n_r 5 B,
            set_n_r 5 C,
            set_n_r 5 D,
            set_n_r 5 E,
            set_n_r 5 H,
            set_n_r 5 L,
            set_n_pHL 5,
            set_n_r 5 A,
            set_n_r 6 B,
            set_n_r 6 C,
            set_n_r 6 D,
            set_n_r 6 E,
            set_n_r 6 H,
            set_n_r 6 L,
            set_n_pHL 6,
            set_n_r 6 A,
            set_n_r 7 B,
            set_n_r 7 C,
            set_n_r 7 D,
            set_n_r 7 E,
            set_n_r 7 H,
            set_n_r 7 L,
            set_n_pHL 7,
            set_n_r 7 A
      ]
instruccionesCB = listArray (0,255) listaCB :: Array Word8 Instrucción

oneInstruction ::  Estado  -> Estado
oneInstruction s = s{cpu = cpu', addrspace = addrspace'}
                where  (i, s') = readPC (cpu s, addrspace s)
                       (cpu', addrspace') = (instruccionesbase ! i) s'
