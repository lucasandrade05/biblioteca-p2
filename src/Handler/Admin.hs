{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql

getAdminR :: Handler Html
getAdminR = do 
    defaultLayout [whamlet|
        <h1> BEM-VINDO ROOT!
        <form action=@{LogoutR} method=post>
            <input type="submit" value="Logout">
    |]