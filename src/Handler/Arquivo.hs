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
import Text.Julius
import Handler.Livro

formArquivo :: Form FileInfo
formArquivo = renderDivs $ areq fileField FieldSettings{fsId=Just "fi",
                           fsLabel="Pesquise seu Arquivo:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","btn btn-outline-default"),("placeholder","Selecione o arquivo")]} Nothing

postArquivoR :: LivroId -> Handler Html 
postArquivoR livid = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess arq -> do 
            livro <- runDB $ get404 livid
            liftIO $ fileMove arq ("static/capas/" ++ (unpack $ fileName arq))
            let filename = (fileName arq) :: Text
            imagem <- runDB $ insert (Capa filename livid)
            redirect (DetalheLivroR livid)
        _ -> do
            setMessage [shamlet| <script> alert("Algo deu errado... favor selecionar apenas fotos!");</script> |] 
            redirect (ArquivoR livid)
        
getArquivoR :: LivroId -> Handler Html
getArquivoR livid = do 
    (widget,  enctype) <- generateFormPost formArquivo
    (widget2, _) <- generateFormPost formPesquisa
    livro <- runDB $ get404 livid
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Selecionar capa do livro : # " ++ (livroTitulo livro)  :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/arquivo.hamlet")