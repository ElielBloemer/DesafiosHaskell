module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Autos(autoVeloz, kilometros)" $ do
    it "si nombre del modelo tiene una cantidad par de caracteres, devuelve false. " $ do autoVeloz("Dodge 1500",1)  `shouldBe` False

    it "si nombre del modelo tiene una cantidad impar de caracteres, devuelve true. " $ do autoVeloz("Uno",1)  `shouldBe` True
    
    it "si el auto tiene mas de 10000 kilometros, necesita un cambio de aceite, devuelve true. " $ do cambioDeAceite("Dodge 1500",10001) `shouldBe` True 

    it "si el auto tiene menos de 10000 kilometros,NO necesita un cambio de aceite, devuelve false. " $ do cambioDeAceite("Dodge 1500",9999) `shouldBe` False

    it "si el auto tiene 10000 kilometros, NO necesita un cambio de aceite, devuelve false. " $ do cambioDeAceite("Dodge 1500",10000) `shouldBe` False