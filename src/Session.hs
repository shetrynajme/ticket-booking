module Session where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Types

type Session a = ReaderT User IO a

runSession :: User -> Session a -> IO a
runSession user session = runReaderT session user

getCurrentUser :: Session User
getCurrentUser = ask
