{-# LANGUAGE OverloadedLists #-}

module Day2
    (emptyMem, readMem, writeMem,
     opcode1, opcode2, opcode99,
     processor, execute, run, find,
     loadBytes) where

    import Data.Map (Map, findWithDefault, empty, insert, delete, fromList)
    import qualified Data.Map as DM(lookup)

    type Halt = Bool
    type Instruction = Int -> Int -> Int -> Memory -> (Halt, Memory)
    type Offset = Int
    type Processor = Map Int Instruction

    type Key = Int
    type Value = Int
    type Memory = Map Key Value

    emptyMem :: Memory
    emptyMem = empty

    readMem :: Key -> Memory -> Value
    readMem = findWithDefault 0

    writeMem :: Key -> Value -> Memory -> Memory
    writeMem key 0   mem = delete key mem
    writeMem key val mem = insert key val mem

    opcode1 :: Instruction
    opcode1 a b c mem = 
        let xa = readMem a mem in
        let xb = readMem b mem in
        (False, writeMem c (xa + xb) mem)

    opcode2 :: Instruction
    opcode2 a b c mem = 
        let xa = readMem a mem in
        let xb = readMem b mem in
        (False, writeMem c (xa * xb) mem)
    
    opcode99 :: Instruction
    opcode99 _ _ _ mem = (True, mem)

    processor :: Processor
    processor = [ (1, opcode1), (2, opcode2), (99, opcode99) ]

    execute :: Processor -> Memory -> Offset -> Either Memory (Memory, Offset)
    execute p mem i =
        let bytecode = readMem i mem in
        let [a, b, c] = [readMem j mem | j <- [i+1..i+3]] in
        case DM.lookup bytecode p of 
            Nothing -> fail "undefined instruction"
            Just ix -> next $ ix a b c mem
        where
            next :: (Halt, Memory) -> Either Memory (Memory, Offset)
            next (True, mem) = Left mem
            next (   _, mem) = Right (mem, i + 4)

    run :: Processor -> Memory -> Memory
    run p mem = runImpl p mem 0
        where
            runImpl :: Processor -> Memory -> Offset -> Memory
            runImpl p mem i = case execute p mem i of
                Left mem -> mem
                Right (mem, offset) -> runImpl p mem offset

    run0 :: Processor -> Memory -> Int
    run0 = (readMem 0 .) . run -- reads first mem cell

    find :: Processor -> Memory -> Int -> (Int, Int)
    find p mem target =
        head $ [ (x, y) | x <- [0..99], y <- [0..99], test x y ]
        where
            test :: Int -> Int -> Bool
            test x y = run0 p (writeMem 1 x $ writeMem 2 y mem) == target

    loadBytes :: [Int] -> Memory
    loadBytes bs = fromList $ zip [0..] bs