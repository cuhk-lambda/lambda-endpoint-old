module Lambda.Endpoint where
import Settings.Lambda 
import Data.UUID.V4 
import Data.UUID
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Short as TS
import qualified Crypto.Argon2 as CA

hashedSecret :: IO BS.ByteString
hashedSecret = do
    salt <- nextRandom
    let (Right result) = CA.hashEncoded CA.defaultHashOptions secret (toStrict $ toByteString salt)
    return $ TS.toByteString result