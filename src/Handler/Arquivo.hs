{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Arquivo where

import System.Directory
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
                           fsAttrs=[("class","btn btn-default"),("placeholder","Selecione o arquivo")]} Nothing

postArquivoR :: LivroId -> Handler Html 
postArquivoR livid = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess arq -> do 
            livro <- runDB $ get404 livid
            filename <- writeToServer arq livid
            redirect (DetalheLivroR livid)
        _ -> do
            setMessage [shamlet| <script> alert("Algo deu errado... favor selecionar apenas fotos!");</script> |] 
            redirect (ArquivoR livid)
            

        
getArquivoR :: LivroId -> Handler Html
getArquivoR livid = do 
    userlogado <- lookupSession "_ID"
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

postDelCapaR :: CapaId -> Handler Html
postDelCapaR capaid = do
    image <- runDB $ get404 capaid
    let livid = (capaIdlivro image) :: LivroId
    let path = imageFilePath (capaNomeimagem image)
    liftIO $ removeFile path
    -- only delete from database if file has been removed from server
    stillExists <- liftIO $ doesFileExist path
    case (not stillExists) of 
        False  -> redirect (DetalheLivroR livid)
        True -> do
            runDB $ delete capaid
            setMessage "A foto anterior foi deletada!"
            redirect (ArquivoR livid)
    


imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static/capas/"

writeToServer :: FileInfo -> LivroId -> Handler FilePath
writeToServer file livid = do
    let filename = (show $ (fromSqlKey livid)) ++ (unpack $ fileName file)
        path = imageFilePath filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Capa filename livid)
    return filename