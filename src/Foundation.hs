{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just $ LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized ListarLivroR _ = return Authorized
    isAuthorized PesqLivroR _ = return Authorized
    isAuthorized BuscarLivroR _ = return Authorized
    isAuthorized (DetalheLivroR _ ) _ = return Authorized
    isAuthorized CadUserR _ = ehAdmin
    isAuthorized ListarUsuario _ = ehAdmin
    isAuthorized AdminR _ = ehAdmin
    isAuthorized _ _ = ehUsuario

ehAdmin :: Handler AuthResult
ehAdmin = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "admin") -> return Authorized
        (Just _ ) -> return $ Unauthorized "VC NAO EH O PAH!"
    
ehUsuario :: Handler AuthResult
ehUsuario = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just _) -> return Authorized


--instance Yesod App where
--    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
