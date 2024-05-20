module Types where

import Data.Time (Day)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField

data User = User { userId :: Int, userName :: String, userPassword :: ByteString }
  deriving (Show)

data Event = Event { eventId :: Int, eventName :: String, eventDate :: Day }
  deriving (Show)

data Reservation = Reservation { reservationId :: Int, reservationUserId :: Int, reservationEventId :: Int }
  deriving (Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id name password) = toRow (id, name, password)

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

instance ToRow Event where
    toRow (Event id name date) = toRow (id, name, date)

instance FromRow Reservation where
    fromRow = Reservation <$> field <*> field <*> field

instance ToRow Reservation where
    toRow (Reservation id userId eventId) = toRow (id, userId, eventId)
