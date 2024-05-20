module User where

import Database.PostgreSQL.Simple
import Control.Exception (catch, SomeException)
import Crypto.BCrypt (hashPasswordUsingPolicy, validatePassword, slowerBcryptHashingPolicy)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Types

registerUser :: Connection -> String -> String -> IO ()
registerUser conn name password = do
    userExists <- doesUserExist conn name
    if userExists
        then putStrLn "User already exists. Please choose a different name."
        else do
            hashedPassword <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack password)
            case hashedPassword of
                Just hp -> do
                    _ <- execute conn (fromString "INSERT INTO users (name, password) VALUES (?,?)") (name, hp)
                    putStrLn "User registered successfully."
                Nothing -> putStrLn "Error hashing password"

doesUserExist :: Connection -> String -> IO Bool
doesUserExist conn name = do
    results <- query conn (fromString "SELECT id FROM users WHERE name = ?") (Only name) :: IO [Only Int]
    return $ not (null results)

loginUser :: Connection -> String -> String -> IO (Maybe User)
loginUser conn name password = do
    results <- query conn (fromString "SELECT id, name, password FROM users WHERE name = ?") (Only name)
    return $ case results of
        [(id, name, hashedPassword)] ->
            if validatePassword hashedPassword (pack password)
                then Just (User id name hashedPassword)
                else Nothing
        _ -> Nothing
