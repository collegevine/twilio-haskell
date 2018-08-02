{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , PostIncomingPhoneNumber(..)
  , Twilio.IncomingPhoneNumbers.get
  , Twilio.IncomingPhoneNumbers.post
  , VoiceMethod(..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Maybe (catMaybes)

import Control.Monad.Twilio
import Twilio.IncomingPhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

data VoiceMethod = Get | Post deriving Show

{- Resource -}

data PostIncomingPhoneNumber = PostIncomingPhoneNumber {
    pipnPhoneNumber :: Text,
    pipnFriendlyName :: Maybe Text,
    pipnSmsApplicationSID :: ApplicationSID,
    pipnVoiceMethod :: VoiceMethod,
    pipnVoiceUrl :: Maybe Text
}

data IncomingPhoneNumbers = IncomingPhoneNumbers
  { incomingPhoneNumberList :: [IncomingPhoneNumber]
  } deriving (Show, Eq)

instance List IncomingPhoneNumbers IncomingPhoneNumber where
  getListWrapper = wrap (const IncomingPhoneNumbers)
  getList = incomingPhoneNumberList
  getPlural = Const "incoming_phone_numbers"

instance FromJSON IncomingPhoneNumbers where
  parseJSON = parseJSONToList

instance Post1 PostIncomingPhoneNumber () where
  post1 n = request (const $ return ()) =<<
    makeTwilioPOSTRequest "/IncomingPhoneNumbers.json"
      ([ ("PhoneNumber", encodeUtf8 $ pipnPhoneNumber n)
      , ("SmsApplicationSid", encodeUtf8 . getSID $ pipnSmsApplicationSID n)
      , ("VoiceMethod", encodeUtf8 . pack . show $ pipnVoiceMethod n)
      ] ++ catMaybes [
            ("FriendlyName",) . encodeUtf8 <$> pipnFriendlyName n,
            ("VoiceUrl",) . encodeUtf8 <$> pipnVoiceUrl n
            ]
      )

instance Get0 IncomingPhoneNumbers where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/IncomingPhoneNumbers.json"

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: MonadThrow m => TwilioT m IncomingPhoneNumbers
get = Resource.get

-- | Provision a new number
post :: MonadThrow m => PostIncomingPhoneNumber -> TwilioT m ()
post = Resource.post
