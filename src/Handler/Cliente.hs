{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Cliente where

import Import
import Database.Persist.Postgresql
import Handler.Livro

formCliente :: Form Cliente
formCliente = renderBootstrap $ Cliente
    <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Nome",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: João Carlos da Silva"),("style","display:inline-block")]} Nothing
    <*> areq emailField FieldSettings{fsId=Just "campo2",
                           fsLabel="Email",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: joao@bol.com.br"),("style","display:inline-block")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo3",
                           fsLabel="CPF",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: 123456789-0"),("style","display:inline-block")]} Nothing
    
formPesquisaCliente :: Form Pesquisa
formPesquisaCliente = renderBootstrap $ Pesquisa
        <$> areq textField FieldSettings{fsId=Just "search",
                           fsLabel="",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Nome, CPF ou E-mail")]} Nothing

getListarClienteR :: Handler Html
getListarClienteR = do 
    (widget2, enctype) <- generateFormPost formPesquisaCliente
    clientes <- runDB $ selectList [] [Asc ClienteNome, Asc ClienteEmail]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Clientes" :: Text
        toWidget $ $(whamletFile "templates/menucli.hamlet")
        toWidget $ $(whamletFile "templates/listarcliente.hamlet") 
        
getCadClienteR :: Handler Html
getCadClienteR = do 
     (widget, enctype) <- generateFormPost formCliente
     (widget2, enctype) <- generateFormPost formPesquisa
     defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Adicionar Cliente"  :: Text
        toWidget $ $(whamletFile "templates/menucli.hamlet")
        toWidget $ $(whamletFile "templates/cadastrarcliente.hamlet") 

postCadClienteR :: Handler Html
postCadClienteR = do 
    ((result,_),_) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            redirect ListarClienteR
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect CadClienteR        
        
        
        
        
postPesqClienteR :: Handler Html
postPesqClienteR = do 
    ((result,_),_) <- runFormPost formPesquisa
    case result of
        FormSuccess pesquisar -> do 
            let cliente = toTexto(pesquisar)
            redirect (BuscarClienteR cliente)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect ListarClienteR
         
getBuscarClienteR :: Text -> Handler Html
getBuscarClienteR cliente = do
    (widget2, enctype) <- generateFormPost formPesquisa
    clientes <- runDB $ selectList ([Filter ClienteNome (Left $ "%"++ cliente ++"%") (BackendSpecificFilter "ILIKE")]
                              ||.[Filter ClienteEmail  (Left $ "%"++ cliente ++"%") (BackendSpecificFilter "ILIKE")]
                              ||.[Filter ClienteCpf (Left $ "%"++ cliente ++"%") (BackendSpecificFilter "ILIKE")])[]
    --livros <- runDB $ selectList [LivroTitulo >. "%"++livros++"%"][Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Resultados da busca:"  :: Text
        toWidget $ $(whamletFile "templates/menucli.hamlet")
        toWidget $ $(whamletFile "templates/listarcliente.hamlet") 
        [whamlet| 
            <div class="col-sm-6"> <i>Exibindo resultados para "#{cliente}" em Clientes. 
        |]
        
