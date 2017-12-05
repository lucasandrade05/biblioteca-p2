{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Livro where

import Import
import Database.Persist.Postgresql
import Text.Julius

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
    <*> areq textareaField FieldSettings{fsId=Just "campo6",
                           fsLabel="Sinopse",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
    <*> areq intField FieldSettings{fsId=Just "campo7",
                           fsLabel="Estoque",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: 3"),("style","display:inline-block")]}  Nothing
    <*> aopt hiddenField "" Nothing


                           
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

livrosLista = do
       entidades <- runDB $ selectList [] [Asc LivroTitulo] 
       optionsPairs $ fmap (\ent -> (livroTitulo $ entityVal ent, entityKey ent)) entidades
       
getListarLivroR :: Handler Html
getListarLivroR = do 
    userlogado <- lookupSession "_ID"
    (widget2, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    defaultLayout $ do 
        setTitle "Biblioteca Haskell - Livros"
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
     userlogado <- lookupSession "_ID"
     (widget, enctype) <- generateFormPost formLivro
     (widget2, enctype) <- generateFormPost formPesquisa
     (widget3, enctype) <- generateFormPost formAltera
     livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
     defaultLayout $ do
        setTitle "Biblioteca Haskell - Adicionar Livros"
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
            livid <- runDB $ insert livro
            _ <- runDB $ update livid [LivroDisponivel =. (Just (livroEstoque livro))]
            redirect (ArquivoR livid)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect CadLivroR


getDetalheLivroR :: LivroId -> Handler Html
getDetalheLivroR livroid = do
    userlogado <- lookupSession "_ID"
    (widget2, enctype) <- generateFormPost formPesquisa
    --livros <- runDB $ selectList [] [Asc LivroAutor, Asc LivroTitulo]
    livro <- runDB $ get404 livroid
    imagem <- runDB $ selectFirst [CapaIdlivro ==. livroid][]
    defaultLayout $ do 
        setTitle "Biblioteca Haskell - Detalhe Livro"
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

data Altera = Altera
   {    nome :: LivroId
    ,   qtd  :: Int }

formAltera :: Form Altera
formAltera = renderBootstrap $ Altera
    <$> areq (selectField livrosLista) FieldSettings{fsId=Just "li",
                           fsLabel="Livro",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma")]} Nothing
    <*> areq intField  FieldSettings{fsId=Just "qt",
                           fsLabel="Quantidade",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: 3")]} Nothing

alteraId :: Altera -> LivroId
alteraId(Altera a b) = a

alteraQtd :: Altera -> Int
alteraQtd(Altera a b) = b

alteraMaybeQtd :: Altera -> Maybe Int
alteraMaybeQtd (Altera a b) = Just b

maybeQtd :: Int -> Maybe Int
maybeQtd a = Just a

postAlteraEstoqueR :: Handler Html
postAlteraEstoqueR = do
    ((result,_),_) <- runFormPost formAltera
    case result of
        FormSuccess alterar -> do
            let pid = alteraId(alterar)
            runDB $ update pid [LivroEstoque +=. alteraQtd(alterar) ]
            runDB $ update pid [LivroDisponivel +=. alteraMaybeQtd(alterar)] 
            redirect (DetalheLivroR pid)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect CadLivroR
    --_ <- runDB $ get404 livid
    --runDB $ update livid [LivroEstoque +=. qtd ]
    --redirect (DetalheLivroR livid)
    
getBuscarLivroR :: Text -> Handler Html
getBuscarLivroR livro = do
    userlogado <- lookupSession "_ID"
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
