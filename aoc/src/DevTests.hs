{-# LANGUAGE OverloadedLists #-}

module DevTests(test, suite) where

    import Test.Hspec
    import Data.Either.Combinators(fromRight')
    import Data.List(intersperse)
    import Data.Map(toList)
    import qualified Data.Map as DM
    import Data.Sort(sortOn)

    import qualified Day1 as D1
    import qualified Day2 as D2
    import qualified Day3 as D3
    import qualified Day4 as D4
    import qualified Day5 as D5

    day1 =
        describe "Day 1" $ do
            it "calculates fuel correctly" $
                D1.calculateFuel 100756 `shouldBe` 50346
            it "sums correctly" $
                D1.calculateAll [6, 20] `shouldBe` 4

    day2 = do
        describe "Day 2 - Memory" $ do
            it "reads empty values as 0" $
                D2.readMem 42 D2.emptyMem `shouldBe` 0

            let mem = D2.writeMem 42 43 D2.emptyMem
            it "reads a written value" $
                D2.readMem 42 mem `shouldBe` 43

        let mem = [(0, 1), (1, 5), (2, 6)]
        describe "Day 2 - Opcode 1" $ do
            it "m[a] + m[b] -> m[c]" $
                D2.readMem 3 (snd $ D2.opcode1 1 2 3 mem) `shouldBe` 11
            it "aliases correctly (a + b -> a)" $
                D2.readMem 1 (snd $ D2.opcode1 1 2 1 mem) `shouldBe` 11

        describe "Day 2 - Opcode 2" $
            it "m[a] * m[b] -> m[c]" $
                D2.readMem 3 (snd $ D2.opcode2 1 2 3 mem) `shouldBe` 30
        
        describe "Day 2 - Opcode 99" $ 
            it "halts the program" $
                D2.opcode99 1 2 3 mem `shouldBe` (True, mem)
        
        let inputProgram = D2.writeMem 1 12 $
                D2.writeMem 2 02 $
                D2.loadBytes [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,9,23,1,23,9,27,1,10,27,31,1,13,31,35,1,35,10,39,2,39,9,43,1,43,13,47,1,5,47,51,1,6,51,55,1,13,55,59,1,59,6,63,1,63,10,67,2,67,6,71,1,71,5,75,2,75,10,79,1,79,6,83,1,83,5,87,1,87,6,91,1,91,13,95,1,95,6,99,2,99,10,103,1,103,6,107,2,6,107,111,1,13,111,115,2,115,10,119,1,119,5,123,2,10,123,127,2,127,9,131,1,5,131,135,2,10,135,139,2,139,9,143,1,143,2,147,1,5,147,0,99,2,0,14,0]

        describe "Day 2 - Execute" $ do
            it "my example works" $
                let program = [2, 5, 6, 5, 0, 2, 1] in
                testOne program program
            it "example 1 works" $ 
                testOne [1,0,0,0,99] [2,0,0,0,99]
            it "example 2 works" $ 
                testOne [2,3,0,3,99] [2,3,0,6,99]
            it "example 3 works" $ 
                testProgram [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

        describe "Day 2 - Results" $ do
            it "calculates result 1" $ do
                let mem = D2.run D2.processor inputProgram
                D2.readMem 0 mem `shouldBe` 5534943
            it "calculates result 2" $ 
                D2.find D2.processor inputProgram 19690720 `shouldBe` (76, 3)
    
    day3 = do
        describe "Day 3 - Manhattan Distance" $ do
            it "(0, 0) (1, 1) -> 2" $
                D3.manhattanDistance 0 0 1 1 `shouldBe` 2
            it "(0, 0) (0, 0) -> 0" $
                D3.manhattanDistance 0 0 0 0 `shouldBe` 0
            it "(-5, 90) (8, 15) -> 88" $
                D3.manhattanDistance (-5) 90 8 15 `shouldBe` 88
    
        describe "Day 3 - Parse Path" $ do
            it "U10 -> ('U', 10)" $
                D3.parsePath "U10" `shouldBe` [('U', 10)]
            it "R8,U5,L5,D3" $
                D3.parsePath "R8,U5,L5,D3" `shouldBe` [('R',8),('U',5),('L',5),('D',3)]

        describe "Day 3 - Make Wire" $ do
            it "simple" $
                D3.mkWire [ ('U', 10) ] `shouldBe` [D3.mkSegment 0 0 0 10]
            it "R8,U5,L5,D3" $
                head (D3.mkWire $ D3.parsePath "R8,U5,L5,D3")
                    `shouldBe` D3.mkSegment 3 5 3 2

        describe "Day 3 - Examine" $
            it "simple" $ do
                let s1 = D3.mkSegment 5 5 5 10
                let s2 = D3.mkSegment 3 7 7 7
                D3.examine s1 s2 `shouldBe` Just (5, 7)
 
        describe "Day 3 - Intersections" $
            it "simple" $ do
                let w1 = D3.mkWire [ ('U', 5), ('L', 10) ]
                let w2 = D3.mkWire [ ('L', 5), ('U', 10) ]
                D3.intersections w1 w2 `shouldBe` [(-5, 5)]

        describe "Day 3 - Closest" $ do
            it "example 1 correct" $ do
                let w1 = D3.mkWire $ D3.parsePath "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                let w2 = D3.mkWire $ D3.parsePath "U62,R66,U55,R34,D71,R55,D58,R83"
                let fromCenter = D3.manhattanDistance' (0,0)
                fromCenter <$> D3.closestIntersection w1 w2 `shouldBe` Just 159
            it "example 2 correct" $ do
                let w1 = D3.mkWire $ D3.parsePath "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                let w2 = D3.mkWire $ D3.parsePath "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                fromCenter <$> D3.closestIntersection w1 w2 `shouldBe` Just 135
        
        describe "Day 3 - Results 1" $ 
            it "correct" $ do
                pathStrings <- lines <$> readFile "../inputs/d3.txt"
                let [l, r] = D3.mkWire . D3.parsePath <$> pathStrings
                fromCenter <$> D3.closestIntersection l r `shouldBe` Just 3247
        
        describe "Day 3 - Enumerate" $ do
            it "simple" $ do
                let wire = D3.mkWire $ D3.parsePath "R8,U5,L5,D3"
                let ws = last wire
                D3.enumerate (8, 0) ws `shouldBe` [(8,0),(7,0),(6,0),(5,0),(4,0),(3,0),(2,0),(1,0),(0,0)]
            it "other" $ do
                let wire = D3.mkWire $ D3.parsePath "R8,U5,L5,D3"
                let ws = head wire
                D3.enumerate (3, 2) ws `shouldBe` [(3,2),(3,3),(3,4),(3,5)]
        
        describe "Day 3 - Annotate" $
            it "simple" $ do
                let wire = D3.mkWire $ D3.parsePath "R8,U5,L5,D3"
                let flip (x, y) = (y, x)
                let lst = sortOn fst $ flip <$> toList (D3.annotate wire) 
                last lst `shouldBe` (21,(3,2))
        
        describe "Day 3 - Closest by steps" $ do
            it "example 1 correct" $ do
                let w1 = D3.mkWire $ D3.parsePath "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                let w2 = D3.mkWire $ D3.parsePath "U62,R66,U55,R34,D71,R55,D58,R83"
                D3.closestIntersectionBySteps w1 w2 `shouldBe` Just 610
            it "example 2 correct" $ do
                let w1 = D3.mkWire $ D3.parsePath "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                let w2 = D3.mkWire $ D3.parsePath "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                D3.closestIntersectionBySteps w1 w2 `shouldBe` Just 410

        describe "Day 3 - Results" $
            it "correct" $ do
                pathStrings <- lines <$> readFile "../inputs/d3.txt"
                let [l, r] = D3.mkWire . D3.parsePath <$> pathStrings
                D3.closestIntersectionBySteps l r `shouldBe` Just 48054

    day4 = do
        describe "Day 4 - isSixDigits" $ do
            it "accepts six-digit numbers" $ 
                D4.isSixDigit 123456 `shouldBe` True
            it "does not accept shorter numbers" $
                D4.isSixDigit 223 `shouldBe` False
            it "does not accept longer numbers" $
                D4.isSixDigit 987654321 `shouldBe` False

        describe "Day 4 - hasAdjacentDigits" $ do
            it "accepts 22" $
                D4.hasAdjacentDigits 22 `shouldBe` True
            it "accepts 111111" $
                D4.hasAdjacentDigits 111111 `shouldBe` True
            it "rejects no adjacents" $
                D4.hasAdjacentDigits 123456 `shouldBe` False

        describe "Day 4 - digitsIncrease" $ do
            it "accepts 22" $
                D4.digitsIncrease 22 `shouldBe` True
            it "accepts 111111" $
                D4.digitsIncrease 111111 `shouldBe` True
            it "accepts correct password" $
                D4.digitsIncrease 123889 `shouldBe` True
            it "rejects incorrect password" $
                D4.digitsIncrease 223450 `shouldBe` False

        describe "Day 4 - findRightPasswords" $
            it "is correct between 100000 and 101000" $
                D4.findRightPasswords 555555 555566 `shouldBe` [555566]
        
        describe "Day 4 - atLeastOneDouble" $ do
            it "accepts correct example" $
                D4.atLeastOneDouble 111122 `shouldBe` True
            it "rejects incorrect example" $
                D4.atLeastOneDouble 123444 `shouldBe` False

        describe "Day 4 - Results" $
            it "correct" $
                length (D4.findRightPasswords 128392 643281) `shouldBe` 1390

    day5 = do
        describe "Day 5 - Memory" $ do
            it "reads empty values as 0" $
                D5.readMem 42 D5.emptyMem `shouldBe` 0

            let mem = D5.writeMem 42 43 D5.emptyMem
            it "reads a written value" $
                D5.readMem 42 mem `shouldBe` 43

        let comp = D5.mkComputer [(0, 1), (1, 5), (2, 6)]
        let modes = replicate 3 D5.positionMode
        let modes' = replicate 3 D5.immediateMode
        describe "Day 5 - Opcode 1" $ do
            it "m[a] + m[b] -> m[c]" $
                D5.readMem 3 (D5.cmem $ D5.opcode1 modes [1, 2, 3] comp) `shouldBe` 11
            it "aliases correctly (a + b -> a)" $
                D5.readMem 1 (D5.cmem $ D5.opcode1 modes [1, 2, 1] comp) `shouldBe` 11
            it "works immediate" $
                D5.readMem 1 (D5.cmem $ D5.opcode1 modes' [1, 2, 1] comp) `shouldBe` 3
            it "works mixed" $ do
                let mixed = head modes : tail modes'
                D5.readMem 3 (D5.cmem $ D5.opcode1 mixed [1, 2, 3] comp) `shouldBe` 7
            it "works mixed differently" $ do
                let mixed = head modes' : tail modes
                D5.readMem 3 (D5.cmem $ D5.opcode1 mixed [1, 2, 3] comp) `shouldBe` 7

        describe "Day 5 - Opcode 2" $
            it "m[a] * m[b] -> m[c]" $
                D5.readMem 3 (D5.cmem $ D5.opcode2 modes [1, 2, 3] comp) `shouldBe` 30
        
        describe "Day 5 - Opcode 3" $
            it "i -> m[a]" $ do
                let comp' = D5.writeInput 42 comp
                let comp'' = D5.opcode3 modes [3] comp'
                D5.readMem 3 (D5.cmem comp'') `shouldBe` 42
                D5.cin comp'' `shouldBe` []

        describe "Day 5 - Opcode 4" $
            it "m[a] -> o" $ do
                let comp' = D5.opcode4 modes [2] comp
                D5.cout comp' `shouldBe` [6]

        describe "Day 5 - Opcode 99" $
            it "halts the program" $
                D5.opcode99 modes [1, 2, 3] comp `shouldBe` comp { D5.halt = True }
        
        describe "Day 5 - Decode" $ do
            it "simple" $
                (($ comp) . ($ 1) <$> snd (fromRight' $ D5.decode D5.intcode 1)) 
                    `shouldBe` [5,5,5]
            it "example 1 works" $ do
                let comp = D5.mkComputer $ D5.loadBytes [1002, 4, 3, 4, 33]
                let result = D5.cmem $ fromRight' $ D5.execute comp
                D5.readMem 4 result `shouldBe` 99
        
        let inputProgram = D5.writeMem 1 12 $
                           D5.writeMem 2 02 $
                           D5.loadBytes [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,9,23,1,23,9,27,1,10,27,31,1,13,31,35,1,35,10,39,2,39,9,43,1,43,13,47,1,5,47,51,1,6,51,55,1,13,55,59,1,59,6,63,1,63,10,67,2,67,6,71,1,71,5,75,2,75,10,79,1,79,6,83,1,83,5,87,1,87,6,91,1,91,13,95,1,95,6,99,2,99,10,103,1,103,6,107,2,6,107,111,1,13,111,115,2,115,10,119,1,119,5,123,2,10,123,127,2,127,9,131,1,5,131,135,2,10,135,139,2,139,9,143,1,143,2,147,1,5,147,0,99,2,0,14,0]

        describe "Day 5 - Execute" $ do
            it "my example works" $
                let program = [2, 5, 6, 5, 0, 2, 1] in
                testOne program program
            it "example 1 works" $ 
                testOne [1,0,0,0,99] [2,0,0,0,99]
            it "example 2 works" $ 
                testOne [2,3,0,3,99] [2,3,0,6,99]
            it "example 3 works" $ 
                testProgram [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

        describe "Day 5 - More opcodes" $ do
            it "works with example 1" $ do
                let program = D5.loadBytes [3,9,8,9,10,9,4,9,99,-1,8]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp 8) `shouldBe` Right 1
                D5.runOut (comp 9) `shouldBe` Right 0
            it "works with example 2" $ do
                let program = D5.loadBytes [3,9,7,9,10,9,4,9,99,-1,8]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp 7) `shouldBe` Right 1
                D5.runOut (comp 8) `shouldBe` Right 0
            it "works with example 3" $ do
                let program = D5.loadBytes [3,3,1108,-1,8,3,4,3,99]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp 7) `shouldBe` Right 0
                D5.runOut (comp 8) `shouldBe` Right 1
            it "works with example 4" $ do
                let program = D5.loadBytes [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp 0) `shouldBe` Right 0
                D5.runOut (comp 8) `shouldBe` Right 1
            it "works with example 5" $ do
                let program = D5.loadBytes [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp 0) `shouldBe` Right 0
                D5.runOut (comp 8) `shouldBe` Right 1
            it "works with example 6" $ do
                let program = D5.loadBytes [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                let comp x = D5.writeInput x $ D5.mkComputer program
                D5.runOut (comp (-7)) `shouldBe` Right 999
                D5.runOut (comp 8) `shouldBe` Right 1000
                D5.runOut (comp 90) `shouldBe` Right 1001

        describe "Day 5 - Results" $ do
            let file = readFile "../inputs/d5.txt"
            it "first part correct" $ do
                nums <- file
                let str = "[" <> nums <> "]"
                let program = D5.loadBytes $ read str
                let comp = D5.writeInput 1 $ D5.mkComputer program
                D5.runOut comp `shouldBe` Right 15426686
            it "first part correct" $ do
                nums <- file
                let str = "[" <> nums <> "]"
                let program = D5.loadBytes $ read str
                let comp = D5.writeInput 5 $ D5.mkComputer program
                D5.runOut comp `shouldBe` Right 11430197


    suite = do
        day1
        day2
        day3
        day4
        day5
     
    test :: IO ()
    test = hspec day5

    
    -- why does `where` clause not work properly

    fromCenter = D3.manhattanDistance' (0,0)

    fetchMem (Left mem) fn = fn mem
    fetchMem (Right (mem, _)) fn = fn mem
    testOne :: [Int] -> [Int] -> IO ()
    testOne code result =
        let program = D2.loadBytes code in
        fetchMem (D2.execute D2.processor program 0) id
            `shouldBe` D2.loadBytes result
    
    testProgram :: [Int] -> [Int] -> IO ()
    testProgram code result =
        D2.run D2.processor (D2.loadBytes code) `shouldBe`
            D2.loadBytes result