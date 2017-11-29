{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Arquivo where

import Import
import Database.Persist.Postgresql
import Control.Monad.Trans.Class

formArquivo :: Form FileInfo
formArquivo = renderDivs $ areq fileField "Arquivo: " Nothing

getArquivoR :: Handler Html
getArquivoR = do 
    (widget,enctype) <- generateFormPost formArquivo
    defaultLayout $ do
        [whamlet|
            <form action=@{ArquivoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postArquivoR :: Handler Html 
postArquivoR = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess arq -> do 
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect HomeR
        _ -> redirect HomeR