module TestMessages (
    messagesTest
) where
import Messages
import Data.Aeson
import Data.Maybe
import Test.Hspec

-- | Makes sure that Encode acts as the revers of Decode
messagesTest :: Spec
messagesTest = context "MessagesTest" $
    it "Json Encode/Decode" $
        m `shouldBe` fromJust decoded
    where 
        m       = InvalidPlayerName $ InvalidPlayerNameData 12
        decoded = decode $ encode m  :: Maybe InboundMessage