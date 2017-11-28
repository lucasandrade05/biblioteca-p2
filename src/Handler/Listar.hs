{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Listar where

import Control.Monad
import Import
import Database.Persist.Postgresql

formLivro :: Form Livro
formLivro = renderBootstrap $ Livro
    <$> areq textField "Titulo: " Nothing
    <*> areq textField "Autor: " Nothing
    <*> areq textField "Editora: " Nothing
    <*> areq textField "Gênero: " Nothing
    <*> areq intField "Ano: "  Nothing
    <*> areq textField "Sinopse: " Nothing
    <*> areq intField "Estoque: " Nothing    
    

data Pesquisa = Pesquisa
    { pesquisa          :: Text
    }

formPesquisa :: Form Pesquisa
formPesquisa = renderBootstrap $ Pesquisa
    <$> areq textField "" Nothing
    
toTexto :: Pesquisa -> Text
toTexto (Pesquisa x) = x

getListarR :: Handler Html
getListarR = do 
    (widget, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/listar.hamlet") 
        


postPesqLivroR :: Handler Html
postPesqLivroR = do 
    ((result,_),_) <- runFormPost formPesquisa
    case result of
        FormSuccess pesquisar -> do 
            let livro = toTexto(pesquisar)
            redirect (BuscarLivroR livro)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect ListarR
            
getBuscarLivroR :: Text -> Handler Html
getBuscarLivroR livro = do
    (widget, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList [Filter LivroTitulo (Left $ "%"++ livro ++"%") (BackendSpecificFilter "ILIKE")] []
    --livros <- runDB $ selectList [LivroTitulo >. "%"++livros++"%"][Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        toWidget $ $(whamletFile "templates/menu.hamlet")
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
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/cadastrar.hamlet") 

postCadLivroR :: Handler Html
postCadLivroR = do 
    ((result,_),_) <- runFormPost formLivro
    case result of
        FormSuccess livro -> do 
            runDB $ insert livro
            redirect ListarR
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect CadLivroR
            