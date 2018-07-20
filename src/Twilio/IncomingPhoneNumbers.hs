{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Twilio.IncomingPhoneNumbers
  ( -- * Resource
    IncomingPhoneNumbers(..)
  , PostIncomingPhoneNumber(..)
  , VoiceMethod(..)
  , Twilio.IncomingPhoneNumbers.get
  , Twilio.IncomingPhoneNumbers.post
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Text (Text, pack, toUpper)
import Data.Text.Encoding

import Control.Monad.Twilio
import Twilio.IncomingPhoneNumber
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data VoiceMethod = Get | Post deriving Show
type VoiceUrl = Text

data PostIncomingPhoneNumber = PostIncomingPhoneNumber {
    pipnPhoneNumber :: Text,
    pipnFriendlyName :: Maybe Text,
    pipnSmsApplicationSID :: ApplicationSID,
    pipnVoiceMethodAndUrl :: Maybe (VoiceMethod, VoiceUrl)
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
    makeTwilioPOSTRequest "/IncomingPhoneNumbers.json" params
      where params = [ph, sid] ++ fn ++ vm
            ph  = ("PhoneNumber", encodeUtf8 $ pipnPhoneNumber n)
            sid = ("SmsApplicationSid", encodeUtf8 . getSID $ pipnSmsApplicationSID n)
            fn  = maybe [] (:[]) (("FriendlyName",) . encodeUtf8 <$> pipnFriendlyName n)
            vm  = maybe [] (\(vm, vUrl) -> [("VoiceMethod", encodeUtf8 . toUpper . pack $ show vm),
                                            ("VoiceUrl", encodeUtf8 vUrl)]) (pipnVoiceMethodAndUrl n)

instance Get0 IncomingPhoneNumbers where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/IncomingPhoneNumbers.json"

-- | Get 'IncomingPhoneNumbers' for a particular country.
get :: MonadThrow m => TwilioT m IncomingPhoneNumbers
get = Resource.get

-- | Provision a new number
post :: MonadThrow m => PostIncomingPhoneNumber -> TwilioT m ()
post = Resource.post
