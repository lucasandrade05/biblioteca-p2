{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Alugar where

import Control.Monad
import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Handler.Livro

clientesLista = do
       entidades <- runDB $ selectList [] [Asc ClienteCpf] 
       optionsPairs $ fmap (\ent -> (clienteCpf $ entityVal ent, entityKey ent)) entidades


formAlugar :: Form Alugar
formAlugar = renderBootstrap $ Alugar
    <$> areq (selectField clientesLista) FieldSettings{fsId=Just "cli",
                           fsLabel="Cliente",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: João Alves")]} Nothing
    <*> areq (selectField livrosLista) FieldSettings{fsId=Just "li",
                           fsLabel="Livro",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma")]} Nothing
    

getAlugarR :: Handler Html
getAlugarR = do
    livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    (widget2, enctype) <- generateFormPost formPesquisa
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Alugar Livros" :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/alugar.hamlet")

    
postAlugarSacolaR :: AlugarId -> Handler Html
postAlugarSacolaR aluid = undefined

getAlugarSacolaR :: AlugarId -> Handler Html
getAlugarSacolaR aluid = undefined