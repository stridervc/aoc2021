module Day16
  ( solve
  ) where

import Helpers
import Data.Char (ord)
import Control.Monad.State

type Bit    = Int
type Bits   = [Bit]
type Parsed = Bits

parseInput :: String -> Parsed
parseInput input = concatMap tobin $ init $ map todec input
  where todec c = if ord c >= 65 then ord c - 55 else ord c - 48
        tobin d = [ b3, b2, b1, b0 ]
                  where b3 = if d >= 8 then 1 else 0
                        b2 = if (d - b3 * 8) >= 4 then 1 else 0
                        b1 = if (d - b3 * 8 - b2 * 4) >= 2 then 1 else 0
                        b0 = d - b3 * 8 - b2 * 4 - b1 * 2

toDec :: Bits -> Int
toDec bits = fst $ foldl (\(accum,i) b -> (accum + b * 2^i, i+1)) (0,0) $ reverse bits

data Operator   = OpSum | OpProduct | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Eq, Show)
data PacketData = Literal Int | Op Operator [Packet] deriving (Eq, Show)

data Packet = Packet
  { packetVersion :: Int
  , packetType    :: Int
  , packetData    :: PacketData
  } deriving (Eq, Show)

intToOp :: Int -> Operator
intToOp 0 = OpSum
intToOp 1 = OpProduct
intToOp 2 = OpMin
intToOp 3 = OpMax
intToOp 5 = OpGT
intToOp 6 = OpLT
intToOp 7 = OpEQ
intToOp x = error $ "Unknown operator " ++ show x

type BitsParser = State Bits

takeN :: Int -> BitsParser Bits
takeN n = do
  bits <- get
  put $ drop n bits
  return $ take n bits

takeNToInt :: Int -> BitsParser Int
takeNToInt n = toDec <$> takeN n

parseLiteral :: BitsParser PacketData
parseLiteral = do
  bits <- get
  let (lbits, bits') = takeLiteral ([], bits)
  put bits'
  return $ Literal $ toDec lbits
  where takeLiteral (lbits, bits) | head bits == 0  = (lbits', bits')
                                  | otherwise       = takeLiteral (lbits', bits')
                                  where lbits'  = lbits <> tail (take 5 bits)
                                        bits'   = drop 5 bits

parseOperator :: Int -> BitsParser PacketData
parseOperator x = do
  mode <- takeNToInt 1
  case mode of
    0 -> do
      length' <- takeNToInt 15
      packetbits <- takeN length'
      return $ Op op $ evalState parsePackets packetbits
    _ -> do
      numpackets <- takeNToInt 11
      packets <- sequence $ takePackets numpackets
      return $ Op op packets
  where takePackets 0 = []
        takePackets n = parsePacket : takePackets (n-1)
        op            = intToOp x

parsePacket :: BitsParser Packet
parsePacket = do
  version <- takeNToInt 3
  typeid  <- takeNToInt 3
  dat <- case typeid of
    4 -> parseLiteral
    x -> parseOperator x
  return $ Packet version typeid dat

parsePackets :: BitsParser [Packet]
parsePackets = do
  bits <- get
  if length bits >= 8 then do
    packet  <- parsePacket
    packets <- parsePackets
    return $ packet : packets
  else
    return []

-- Part 1 --

nestedVersions :: Packet -> [Int]
nestedVersions p
  | isLiteral = [version]
  | otherwise = version : concatMap nestedVersions ps
  where version     = packetVersion p
        dat         = packetData p
        isLiteral   = case dat of
                        Literal _ -> True
                        _         -> False
        Op _ ps     = dat

part1 :: Parsed -> IO ()
part1 input = do
  let packet = evalState parsePacket input
  print $ sum $ nestedVersions packet

-- Part 2 --

evalPacketData :: PacketData -> Int
evalPacketData (Literal x)          = x
evalPacketData (Op OpSum ps)        = sum $ map evalPacket ps
evalPacketData (Op OpProduct ps)    = product $ map evalPacket ps
evalPacketData (Op OpMin ps)        = minimum $ map evalPacket ps
evalPacketData (Op OpMax ps)        = maximum $ map evalPacket ps
evalPacketData (Op OpGT (p1:p2:_))  = if evalPacket p1 > evalPacket p2 then 1 else 0
evalPacketData (Op OpLT (p1:p2:_))  = if evalPacket p1 < evalPacket p2 then 1 else 0
evalPacketData (Op OpEQ (p1:p2:_))  = if evalPacket p1 == evalPacket p2 then 1 else 0
evalPacketData _ = error "Error in evalPacketData"

evalPacket :: Packet -> Int
evalPacket = evalPacketData . packetData

part2 :: Parsed -> IO ()
part2 input = do
  let packet = evalState parsePacket input
  print $ evalPacket packet

solve :: String -> IO ()
solve input = do
  putStr "Part 1 : "
  part1 $ parseInput input
  putStr "Part 2 : "
  part2 $ parseInput input
