import Lib
import Test.Hspec


main :: IO ()
main = hspec $ do
    describe "fromList" $ do
        it "standard" $ 
             toListPQ (fromListPQ [1, 2, 3, 4, 5]) `shouldBe` [5, 4, 3, 2, 1]
        it "double" $ 
             toListPQ (fromListPQ [1, 1, 3, 4, 5]) `shouldBe` [5, 4, 3, 1, 1]
    describe "modify" $ do
        it "standard" $
            (toListPQ . modifyFirst (*2) $ fromListPQ [1, 2, 3, 4]) `shouldBe` [8, 3, 2, 1]
        it "changeOrder" $
            (toListPQ . modifyFirst (`div` 2) $ fromListPQ [1, 2, 3, 4]) `shouldBe` [3, 2, 2, 1]
