{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Listar where

import Import
import Database.Persist.Postgresql

getListarR :: Handler Html
getListarR = do 
    livros <- runDB $ selectList [] [Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <td> Id
                        <td> Titulo 
                        <td> Autor 
                        <td> Editora
                        <td> Estoque
                        <td> 
                
                <tbody>
                    $forall (Entity pid livro) <- livros
                        <tr> 
                            <td> #{fromSqlKey pid}
                            <td> #{livroTitulo livro}
                            <td> #{livroAutor livro}
                            <td> #{livroEditora livro}
                            <td> #{livroEstoque livro}
                            <td> 
                                <form action=@{ApagarProdR pid} method=post>
                                    <input type="submit" value="Deletar">
                            
        |]
        
        
postApagarLivroR :: LivroId -> Handler Html 
postApagarLivroR pid = do 
    runDB $ delete pid
    redirect ListarR