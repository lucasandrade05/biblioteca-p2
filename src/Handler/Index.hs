{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Index where


import Import
import Text.Julius

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do 
        setTitle "Biblioteca Haskell"
        addStylesheet $ (StaticR css_bootstrap_css)
        addStylesheet $ (StaticR css_main_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_jquery_dropotron_min_js
        addScript $ StaticR js_bootstrap_js
        addScript $ StaticR js_bootstrap_min_js
        addScript $ StaticR js_npm_js
        addScript $ StaticR js_skel_min_js
        addScript $ StaticR js_skel_viewport_min_js
        addScript $ StaticR js_util_js
        addScript $ StaticR js_main_js
        --toWidgetHead $ $(juliusFile "templates/js/jquery.dropotron.min.julius")
        --toWidgetHead $ $(juliusFile "templates/js/skel.min.julius")
        --toWidgetHead $ $(juliusFile "templates/js/skel-viewport.min.julius")
        --toWidgetHead $ $(juliusFile "templates/js/util.julius")
        --toWidgetHead $ $(juliusFile "templates/js/bootstrap.julius")
        --toWidgetHead $ $(juliusFile "templates/js/bootstrap.min.julius")
        --toWidgetHead $ $(juliusFile "templates/js/npm.julius")
        --toWidgetHead $ $(juliusFile "templates/js/main.julius")
        toWidgetHead [hamlet|
            <meta name="viewport" content="width=device-width, initial-scale=1" />
        |]
        toWidget $ $(whamletFile "templates/home.hamlet") 