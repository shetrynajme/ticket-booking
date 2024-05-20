module Database where

import Database.PostgreSQL.Simple
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.String (fromString)
import Data.Time (Day)
import Types

connectDB :: IO Connection
connectDB = connect defaultConnectInfo
    { connectHost = "localhost"
    , connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = "trzeciak"
    , connectPort = 5432
    }

initializeDB :: Connection -> IO ()
initializeDB conn = do
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT UNIQUE, password TEXT)")
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS events (id SERIAL PRIMARY KEY, name TEXT, date DATE)")
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS reservations (id SERIAL PRIMARY KEY, user_id INT, event_id INT)")
    return ()

hashPassword :: String -> IO (Maybe ByteString)
hashPassword password = hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack password)

checkPassword :: ByteString -> String -> Bool
checkPassword hashedPassword password = validatePassword hashedPassword (pack password)
