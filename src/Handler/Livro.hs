{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Livro where

import Import
import Database.Persist.Postgresql

formLivro :: Form Livro
formLivro = renderBootstrap $ Livro
    <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Titulo",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: O Senhor dos Aneis - A Sociedade do Anel"),("style","display:inline-block")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo2",
                           fsLabel="Autor",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Tolkien"),("style","display:inline-block")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo3",
                           fsLabel="Editora",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Martins Fontes"),("style","display:inline-block")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo4",
                           fsLabel="Genero",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Fantasia"),("style","display:inline-block")]} Nothing
    <*> areq intField FieldSettings{fsId=Just "campo5",
                           fsLabel="Ano",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: 1995"),("style","display:inline-block")]}  Nothing
    <*> areq textField FieldSettings{fsId=Just "campo6",
                           fsLabel="Sinopse",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
    <*> areq intField FieldSettings{fsId=Just "campo7",
                           fsLabel="Estoque",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: 3"),("style","display:inline-block")]} Nothing    

    
data Pesquisa = Pesquisa
    { pesquisa          :: Text
    }
    
formPesquisa :: Form Pesquisa
formPesquisa = renderBootstrap $ Pesquisa
        <$> areq textField FieldSettings{fsId=Just "search",
                           fsLabel="",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Digite sua pesquisa")]} Nothing
    
toTexto :: Pesquisa -> Text
toTexto (Pesquisa x) = x



getListarLivroR :: Handler Html
getListarLivroR = do 
    (widget2, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Livros" :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/listarlivro.hamlet") 
        
        
postApagarLivroR :: LivroId -> Handler Html 
postApagarLivroR pid = do 
    _ <- runDB $ get404 pid  -- EH UM SELECT(procura o registro),
    --  SE ACHAR, PROSSEGUE, SE N ACHAR, BARRA O RESTANTE JOGANDO STATUS 404
    runDB (delete pid)
    redirect ListarLivroR
    
getCadLivroR :: Handler Html
getCadLivroR = do 
     (widget, enctype) <- generateFormPost formLivro
     (widget2, enctype) <- generateFormPost formPesquisa
     defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Adicionar Livro"  :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/cadastrarlivro.hamlet") 

postCadLivroR :: Handler Html
postCadLivroR = do 
    ((result,_),_) <- runFormPost formLivro
    case result of
        FormSuccess livro -> do
            runDB $ insert livro
            redirect ListarLivroR
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect CadLivroR


getDetalheLivroR :: LivroId -> Handler Html
getDetalheLivroR livroid = do
    (widget2, enctype) <- generateFormPost formPesquisa
    --livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    livro <- runDB $ get404 livroid
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "#" ++ (livroTitulo livro)  :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/livro.hamlet")




postPesqLivroR :: Handler Html
postPesqLivroR = do 
    ((result,_),_) <- runFormPost formPesquisa
    case result of
        FormSuccess pesquisar -> do 
            let livro = toTexto(pesquisar)
            redirect (BuscarLivroR livro)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect ListarLivroR
            
getBuscarLivroR :: Text -> Handler Html
getBuscarLivroR livro = do
    (widget2, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList ([Filter LivroTitulo (Left $ "%"++ livro ++"%") (BackendSpecificFilter "ILIKE")]
                              ||.[Filter LivroAutor  (Left $ "%"++ livro ++"%") (BackendSpecificFilter "ILIKE")]
                              ||.[Filter LivroEditora(Left $ "%"++ livro ++"%") (BackendSpecificFilter "ILIKE")])[]
    --livros <- runDB $ selectList [LivroTitulo >. "%"++livros++"%"][Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Resultados da busca:"  :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/listarlivro.hamlet") 
        [whamlet| 
            <div class="col-sm-6"> <i>Exibindo resultados para "#{livro}" em Livros
        |]