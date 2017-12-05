{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Index where


import Import
import Text.Julius
import Handler.Livro

getHomeR :: Handler Html
getHomeR = do
    userlogado <- lookupSession "_ID"
    (widget2, enctype) <- generateFormPost formPesquisa
    livros <- runDB $ selectList [][Desc LivroId, LimitTo 3]
    defaultLayout $ do 
        setTitle "Biblioteca Haskell"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        let nomePagina = "Home" :: Text
        toWidget $ $(whamletFile "templates/menu.hamlet") 
        toWidget $ $(whamletFile "templates/home.hamlet")
        
capaById :: LivroId -> Widget
capaById idLiv = do 
    imagem <- handlerToWidget $ runDB $ selectFirst [CapaIdlivro ==. idLiv][]
    [whamlet|
     $forall (Entity img capa) <- imagem
      <img src="/static/capas/#{capaNomeimagem capa}" width="350px" height="527px"></img>
    |]