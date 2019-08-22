{-# LANGUAGE OverloadedStrings #-}

module Lambda.Endpoint where

import qualified Crypto.Argon2         as CA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text.Short       as TS
import           Data.UUID
import           Data.UUID.V4
import           Import
import           Settings.Lambda

hashedSecret :: IO BS.ByteString
hashedSecret = do
  salt <- nextRandom
  let (Right result) =
        CA.hashEncoded
          CA.defaultHashOptions
          secret
          (BL.toStrict $ toByteString salt)
  return $ TS.toByteString result

verify :: TS.ShortText -> Bool
verify pass =
  case CA.verifyEncoded pass secret of
    CA.Argon2Ok -> True
    _           -> False

withVerification :: Handler a -> Handler a
withVerification handler = do
  header <- lookupHeader "Authorization"
  let checking =
        if isNothing header
          then False
          else uuid == BS.pack endpointUUID && verify pass
        where
          Just y = header
          (uuid, Just pass) =
            fmap TS.fromByteString $ BS.breakSubstring "$argon2" y
  if checking
    then handler
    else do
      notAuthenticated
