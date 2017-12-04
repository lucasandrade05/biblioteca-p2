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
       
--sacolaLista cliid = do
--       entidades <- runDB $ selectList [AlugarCliid =. cliid] [Asc ClienteCpf] 
--       entidades2 <- optionsPairs $ fmap (\ent -> (AlugarLivid $ entityVal ent, entityKey ent)) entidades
       


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
    (widget2, enctype) <- generateFormPost formPesquisa
    (widget, enctype) <- generateFormPost formAlugar
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Alugar Livros" :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/alugar.hamlet")
        toWidget $ [julius|
        document.getElementById('showaluglivro').style.display = 'none';
        |]

    
postRegistraLocarR :: Handler Html
postRegistraLocarR = do
    ((result,_),_) <- runFormPost formAlugar
    case result of
        FormSuccess alugar -> do 
            runDB $ insert alugar
            redirect (AlugarSacolaR (alugarCliid alugar))
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect AlugarR
 



getAlugarSacolaR :: ClienteId -> Handler Html
getAlugarSacolaR idCli = do
    (widget2, enctype) <- generateFormPost formPesquisa
    (widget, enctype) <- generateFormPost formAlugar
    listaalug <- runDB $ selectList [AlugarCliid ==. idCli] []
    cliente <- runDB $ get404 idCli
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Livros atualmente alugados por " ++ (clienteNome cliente) :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/alugarsacola.hamlet") 
        toWidget $ [julius|
            document.getElementById('showaluglivro').style.display = 'block';
        |]

livById :: AlugarId -> Widget
livById idAlug = do 
    alugar <- handlerToWidget $ runDB $ get404 idAlug
    let idLiv = (alugarLivid alugar) :: LivroId
    livro <- handlerToWidget $ runDB $ get404 idLiv
    [whamlet|
          <td> #{livroTitulo livro}
          <td> #{livroAutor livro}
          <td> #{livroEditora livro}
          <td class="forms"><center>
            <form style="display:inline-block" action="#" method=get><input type="submit" class="btn btn-success" value="Visualizar"></button></form>
            <form style="display:inline-block" action="#" method=post><input type="submit" class="btn btn-danger" value="Devolver"></button></form>
    |]