module Main where

import Database.PostgreSQL.Simple
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Control.Exception (catch, SomeException)
import Data.Maybe (isJust, fromJust)
import Data.Time (Day)

import User
import Reservation
import Session
import Types
import Database

main :: IO ()
main = do
    conn <- catch (connectDB) handleSqlError
    initializeDB conn

    putStrLn "Welcome to the Ticket Booking System!"
    user <- loginOrRegister conn
    runSession user $ appLoop conn

loginOrRegister :: Connection -> IO User
loginOrRegister conn = do
    putStrLn "1) Register 2) Login"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Enter your name:"
            name <- getLine
            putStrLn "Enter your password:"
            password <- getLine
            registerUser conn name password
            loginOrRegister conn
        "2" -> do
            putStrLn "Enter your name:"
            name <- getLine
            putStrLn "Enter your password:"
            password <- getLine
            mUser <- loginUser conn name password
            if isJust mUser
                then return (fromJust mUser)
                else do
                    putStrLn "Invalid credentials, please try again."
                    loginOrRegister conn
        _ -> do
            putStrLn "Invalid option, please try again."
            loginOrRegister conn

appLoop :: Connection -> Session ()
appLoop conn = forever $ do
    lift $ putStrLn "Choose an option: 1) Add reservation 2) View reservations 3) Add event 4) View events 5) Check event attendees 6) Exit"
    choice <- lift getLine
    case choice of
        "1" -> do
            lift $ putStrLn "Enter event ID:"
            eventIdStr <- lift getLine
            let eventId = read eventIdStr :: Int
            user <- getCurrentUser
            lift $ addReservation conn (userId user) eventId
        "2" -> do
            user <- getCurrentUser
            reservations <- lift $ getReservations conn (userId user)
            lift $ putStrLn "Your reservations:"
            lift $ forM_ reservations (print . show)
        "3" -> do
            lift $ putStrLn "Enter event name:"
            eventName <- lift getLine
            lift $ putStrLn "Enter event date (YYYY-MM-DD):"
            eventDateStr <- lift getLine
            let eventDate = read eventDateStr :: Day
            lift $ addEvent conn eventName eventDate
        "4" -> do
            events <- lift $ getEvents conn
            lift $ putStrLn "Available events:"
            lift $ forM_ events (print . show)
        "5" -> do
            lift $ putStrLn "Enter event ID to check attendees:"
            eventIdStr <- lift getLine
            let eventId = read eventIdStr :: Int
            attendees <- lift $ getUsersForEvent conn eventId
            lift $ putStrLn $ "Attendees for event " ++ show eventId ++ ":"
            lift $ forM_ attendees $ \(userId, userName) -> putStrLn $ show userId ++ ": " ++ userName
        "6" -> liftIO $ putStrLn "Exiting..." >> error "Program terminated by user"
        _   -> lift $ putStrLn "Invalid option. Please try again."

handleSqlError :: SomeException -> IO Connection
handleSqlError e = do
    putStrLn $ "Database connection error: " ++ show e
    error "Failed to connect to the database"
