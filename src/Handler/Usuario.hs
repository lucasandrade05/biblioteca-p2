{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql
import Handler.Livro

formPesquisaUsuario :: Form Pesquisa
formPesquisaUsuario = renderBootstrap $ Pesquisa
        <$> areq textField FieldSettings{fsId=Just "search",
                           fsLabel="",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Login ou E-mail")]} Nothing

getListarUsuarioR :: Handler Html
getListarUsuarioR = do 
    userlogado <- lookupSession "_ID"
    (widget2, enctype) <- generateFormPost formPesquisa
    (widget5, enctype) <- generateFormPost formPesquisaUsuario
    usuarios <- runDB $ selectList [] [Asc UsuarioLogin]
    defaultLayout $ do 
        setTitle "Biblioteca Haskell - Usuarios"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Usuarios: " :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/listarusuario.hamlet") 
        [whamlet| <script>document.getElementById("top").style.display = "none";</script>|]
        
postApagarUsuarioR :: UsuarioId -> Handler Html 
postApagarUsuarioR pid = do 
    _ <- runDB $ get404 pid  -- EH UM SELECT(procura o registro),
    --  SE ACHAR, PROSSEGUE, SE N ACHAR, BARRA O RESTANTE JOGANDO STATUS 404
    runDB (delete pid)
    redirect ListarUsuarioR

postPesqUsuarioR :: Handler Html
postPesqUsuarioR = do 
    ((result,_),_) <- runFormPost formPesquisaUsuario
    case result of
        FormSuccess pesquisar -> do 
            let usuario = toTexto(pesquisar)
            redirect (BuscarUsuarioR usuario)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect ListarUsuarioR
         
getBuscarUsuarioR :: Text -> Handler Html
getBuscarUsuarioR usuario = do
    userlogado <- lookupSession "_ID"
    (widget2, enctype) <- generateFormPost formPesquisa
    (widget5, enctype) <- generateFormPost formPesquisaUsuario
    usuarios <- runDB $ selectList ([Filter UsuarioLogin (Left $ "%"++ usuario ++"%") (BackendSpecificFilter "ILIKE")]
                              ||.[Filter UsuarioEmail  (Left $ "%"++ usuario ++"%") (BackendSpecificFilter "ILIKE")])[]
    defaultLayout $ do 
        setTitle "Biblioteca Haskell - BuscarUsuario"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Resultados da busca:"  :: Text
        toWidget $ $(whamletFile "templates/menucli.hamlet")
        toWidget $ $(whamletFile "templates/listarusuario.hamlet") 
        [whamlet| <script>document.getElementById("top").style.display = "none";</script>|]
        [whamlet| 
            <div class="col-sm-6"> <i>Exibindo resultados para "#{usuario}" em Usuarios. 
        |]
        
