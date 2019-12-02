{-# LANGUAGE OverloadedLists #-}

module DevTests where

    import Test.Hspec
    import qualified Day1 as D1
    import qualified Day2 as D2

    suite = do
        describe "Day 1" $ do
            it "calculates fuel correctly" $
                D1.calculateFuel 100756 `shouldBe` 50346
            it "sums correctly" $
                D1.calculateAll [6, 20] `shouldBe` 4

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

        describe "Day 3 - Results" $ do
            it "calculates result 1" $ do
                let mem = D2.run D2.processor inputProgram
                D2.readMem 0 mem `shouldBe` 5534943
            it "calculates result 2" $ 
                D2.find D2.processor inputProgram 19690720 `shouldBe` (76, 3)

        describe "Day 3 - Execute" $ do
            it "my example works" $
                let program = [2, 5, 6, 5, 0, 2, 1] in
                testOne program program
            it "example 1 works" $ 
                testOne [1,0,0,0,99] [2,0,0,0,99]
            it "example 2 works" $ 
                testOne [2,3,0,3,99] [2,3,0,6,99]
            it "example 3 works" $ 
                testProgram [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

                where 
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
                            (D2.loadBytes result)

    test :: IO ()
    test = hspec suite