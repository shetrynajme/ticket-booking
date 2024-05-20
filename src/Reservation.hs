module Reservation where

import Database.PostgreSQL.Simple
import Control.Monad (forM_)
import Data.String (fromString)
import Types

getReservations :: Connection -> Int -> IO [Reservation]
getReservations conn userId = query conn (fromString "SELECT id, user_id, event_id FROM reservations WHERE user_id = ?") (Only userId)

addReservation :: Connection -> Int -> Int -> IO ()
addReservation conn userId eventId = do
    execute conn (fromString "INSERT INTO reservations (user_id, event_id) VALUES (?,?)") (userId, eventId)
    putStrLn "Reservation added."

getUsersForEvent :: Connection -> Int -> IO [(Int, String)]
getUsersForEvent conn eventId = query conn (fromString "SELECT users.id, users.name FROM users JOIN reservations ON users.id = reservations.user_id WHERE reservations.event_id = ?") (Only eventId)
