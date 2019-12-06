{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Day5
    ( emptyMem, readMem, writeMem
    , writeInput, readOutput
    , opcode1, opcode2, opcode3, opcode4, opcode99
    , decode, positionMode, immediateMode
    , mkComputer, pushInput
    , intcode, execute, run, run0, runOut, find
    , loadBytes
    , Computer(..)) where

    import Data.Map (Map, findWithDefault, empty, insert, delete, fromList)
    import qualified Data.Map as DM(lookup)

    type Offset = Int
    type OpCode = Int
    type Arity  = Int 
    type Mode = Int -> Computer -> Int
    type Instruction = [Mode] -> [Int] -> Computer -> Computer

    newtype InstructionCell = InstructionCell { value :: (Arity, Instruction) }
    type InstructionSet = Map OpCode InstructionCell

    instance Show InstructionCell where
        show ic = show (fst . value $ ic)

    instance Eq InstructionCell where
        x == y = (fst . value $ x) == (fst . value $ y)

    type Halt = Bool
    data Computer = Computer {
        iset :: InstructionSet
      , cmem :: Memory
      , halt :: Halt
      , iptr :: Offset
      , cin  :: [Int]
      , cout :: [Int]}
      deriving (Eq, Show)

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

    readInput :: Computer -> (Computer, Int)
    readInput c@ Computer { cin = [] } = (c, 0)
    readInput c@ Computer { cin = h : t } = (c {cin = t}, h)

    readOutput :: Computer -> (Computer, Int)
    readOutput c@ Computer { cout = [] } = (c, 0)
    readOutput c@ Computer { cout = h : t } = (c {cout = t}, h)

    writeInput :: Int -> Computer -> Computer
    writeInput i comp = comp { cin = i : cin comp }

    writeOutput :: Int -> Computer -> Computer
    writeOutput i comp = comp { cout = i : cout comp }

    positionMode :: Mode
    positionMode i c = readMem i (cmem c)

    immediateMode :: Mode
    immediateMode i _ = i

    decode :: InstructionSet -> Int -> Either String (Instruction, [Mode])
    decode inset code =
        let opcode = code `rem` 100 in
        case DM.lookup opcode inset of
            Nothing -> Left "undefined instruction"
            Just (InstructionCell (arity, ix)) -> 
                let mask = reverse $ lpad arity $ show $ code `div` 100 in
                Right (ix, selectMode <$> mask)
        where
            lpad m xs = replicate (m - length ys) '0' ++ ys
                where ys = take m xs
            selectMode '0' = positionMode
            selectMode _   = immediateMode

    opcode1 :: Instruction -- add
    opcode1 [ma, mb, _] [a, b, c] comp = 
        let mem = cmem comp in
        let xa = ma a comp in
        let xb = mb b comp in
        advance 4 $ comp { cmem = writeMem c (xa + xb) mem }

    opcode2 :: Instruction -- multiply
    opcode2 [ma, mb, _] [a, b, c] comp = 
        let mem = cmem comp in
        let xa = ma a comp in
        let xb = mb b comp in
        advance 4 $ comp { cmem = writeMem c (xa * xb) mem }
    
    opcode3 :: Instruction -- input
    opcode3 _ [a] comp =
        let mem = cmem comp in
        let (comp', i) = readInput comp in
        advance 2 $ comp' { cmem = writeMem a i mem }
    
    opcode4 :: Instruction -- output
    opcode4 (ma:_) [a] comp =
        let xa = ma a comp in
        advance 2 $ writeOutput xa comp
    
    opcode5 :: Instruction -- jump-if-true
    opcode5 [ma, mb] [a, b] comp =
        let xa = ma a comp in
        let xb = mb b comp in
        if xa /= 0 then comp { iptr = xb } else advance 3 comp

    opcode6 :: Instruction -- jump-if-false
    opcode6 [ma, mb] [a, b] comp =
        let xa = ma a comp in
        let xb = mb b comp in
        if xa == 0 then comp { iptr = xb } else advance 3 comp

    opcode7 :: Instruction -- less-than
    opcode7 [ma, mb, _] [a, b, c] comp =
        let mem = cmem comp in
        let xa = ma a comp in
        let xb = mb b comp in
        let out = if xa < xb then 1 else 0 in
        advance 4 $ comp { cmem = writeMem c out mem }

    opcode8 :: Instruction -- equals
    opcode8 [ma, mb, _] [a, b, c] comp =
        let mem = cmem comp in
        let xa = ma a comp in
        let xb = mb b comp in
        let out = if xa == xb then 1 else 0 in
        advance 4 $ comp { cmem = writeMem c out mem }

    opcode99 :: Instruction
    opcode99 _ _ comp = comp { halt = True }

    intcode :: InstructionSet
    intcode = [ (1,  InstructionCell (3, opcode1)), 
                (2,  InstructionCell (3, opcode2)), 
                (3,  InstructionCell (1, opcode3)), 
                (4,  InstructionCell (1, opcode4)), 
                (5,  InstructionCell (2, opcode5)), 
                (6,  InstructionCell (2, opcode6)), 
                (7,  InstructionCell (3, opcode7)), 
                (8,  InstructionCell (3, opcode8)), 
                (99, InstructionCell (0, opcode99)) ]

    mkComputer :: Memory -> Computer
    mkComputer mem = Computer {
        iset = intcode
      , cmem = mem
      , halt = False
      , iptr = 0
      , cin  = []
      , cout = []}

    advance :: Offset -> Computer -> Computer
    advance i comp = comp { iptr = iptr comp + i }

    pushInput :: Int -> Computer -> Computer
    pushInput i comp = comp { cin = i : cin comp }

    execute :: Computer -> Either String Computer
    execute comp =
        let ip = iptr comp in
        let mem = cmem comp in
        let bytecode = readMem ip mem in
        let execute :: (Instruction, [Mode]) -> Computer
            execute (ix, modes) = 
                let ar = length modes in 
                let args = [readMem j mem | j <- [ip+1..ip+ar]] in
                    ix modes args comp in
        execute <$> decode (iset comp) bytecode

    run :: Computer -> Either String Computer
    run comp = case execute comp of
                Left err -> Left err
                Right comp -> if halt comp then Right comp else run comp 

    run0 :: Computer -> Either String Int
    run0 = (readMem 0 . cmem <$>) . run -- reads first mem cell

    runOut :: Computer -> Either String Int
    runOut = (head . cout <$>) . run

    find :: Computer -> Int -> (Int, Int)
    find comp target =
        head [ (x, y) | x <- [0..99], y <- [0..99], test x y ]
        where
            withMem :: Int -> Int -> Computer
            withMem x y = comp { cmem = writeMem 1 x $ writeMem 2 y (cmem comp) }
            test :: Int -> Int -> Bool
            test x y = run0 (withMem x y) == Right target

    loadBytes :: [Int] -> Memory
    loadBytes bs = fromList $ zip [0..] bs