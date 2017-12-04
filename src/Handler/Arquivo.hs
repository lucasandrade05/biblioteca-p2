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
                           fsAttrs=[("class","btn btn-inverse"),("placeholder","Selecione o arquivo"),("accept","image/png, image/jpeg, image/gif"),("style","height:'150px'")]} Nothing

postArquivoR :: LivroId -> Handler Html 
postArquivoR livid = do 
    ((res,_),_) <- runFormPost formArquivo
    case res of 
        FormSuccess arq -> do 
            livro <- runDB $ get404 livid
            liftIO $ fileMove arq ("static/capas/" ++ (unpack $ fileName arq))
            let filename = (fileName arq) :: Text
            imagem <- runDB $ insert (Capa filename livid)
            redirect HomeR
        _ -> redirect HomeR
        
getArquivoR :: LivroId -> Handler Html
getArquivoR livroid = do 
    (widget,enctype) <- generateFormPost formArquivo
    (widget2, enctype) <- generateFormPost formPesquisa
    livro <- runDB $ get404 livroid
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        let nomePagina = "Selecionar capa do livro : # " ++ (livroTitulo livro)  :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet")
        [whamlet|
         <hr />
            <div id="actions" class="form-group col-md-5" style="Left:25%">
                <div class="col-md-12">
                    <br>
                    <form action=@{ArquivoR livroid} method=post enctype=#{enctype}>
                         <center>^{widget}
                         <br>
                         <center><input type="submit" class="btn btn-info" value="  Enviar  "></input>
        <script>
         document.getElementById("pesq").style.display="none";</script>
        |]
