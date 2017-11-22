{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Listar where

import Import
import Database.Persist.Postgresql

formLivro :: Form Livro
formLivro = renderBootstrap $ Livro
    <$> areq textField "Titulo: " Nothing
    <*> areq textField "Autor: " Nothing
    <*> areq textField "Editora: " Nothing
    <*> areq textField "Gênero: " Nothing
    <*> areq intField "Ano: "  Nothing
    <*> areq intField "Estoque: " Nothing    

getListarR :: Handler Html
getListarR = do 
    livros <- runDB $ selectList [] [Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        toWidget $ $(whamletFile "templates/listar.hamlet") 
        
        
        
postApagarLivroR :: LivroId -> Handler Html 
postApagarLivroR pid = do 
    runDB $ delete pid
    redirect ListarR
    
getCadLivroR :: Handler Html
getCadLivroR = do 
     (widget, enctype) <- generateFormPost formLivro
     defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <form action=@{CadLivroR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
            
        |]

postCadLivroR :: Handler Html
postCadLivroR = do 
    ((result,_),_) <- runFormPost formLivro
    case result of
        FormSuccess livro -> do 
            runDB $ insert livro 
            redirect ListarR
        _ -> redirect HomeR