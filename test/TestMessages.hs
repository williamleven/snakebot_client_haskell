module TestMessages (
    messagesTest
) where
import Messages
import Data.Aeson
import Data.Maybe
import Test.Hspec

messagesTest :: Spec
messagesTest = context "MessagesTest" $ do
    it "Json Encode/Decode" $ do 
        let m = InvalidPlayerName $ InvalidPlayerNameData 12
        let decoded = decode $ encode m  :: Maybe InboundMessage
        m `shouldBe` fromJust decoded