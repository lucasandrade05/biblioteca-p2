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
    defaultLayout $ do
        setTitle "Biblioteca Haskell - Admin"
        [whamlet|
            <h1> BEM-VINDO ADMIN!
            <br>
            <form action=@{HomeR} method=get>
                <input type="submit" value="Ir para o site!">
        |]