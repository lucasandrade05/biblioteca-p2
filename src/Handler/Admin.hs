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
        <br>
        <form action=@{HomeR} method=post>
            <input type="submit" value="Ir para o site!">
    |]