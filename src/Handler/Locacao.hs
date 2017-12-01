{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Locacao where

import Control.Monad
import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getAlugarR :: Handler Html
getAlugarR = do
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        addStylesheet $ (StaticR css_bootstrap_combined_min_css)
        addStylesheet $ (StaticR css_select2_css)
        addStylesheet $ (StaticR css_select2_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        addScript $ StaticR js_bootstrap_select_js
        addScript $ StaticR js_select2_js
--        toWidget $ $(whamletFile "templates/teste.hamlet")

    
postAlugarSacolaR :: Handler Html
postAlugarSacolaR = undefined