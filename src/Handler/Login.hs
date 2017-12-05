{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql
import Handler.Livro
import Text.Julius

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
    <$> areq textField FieldSettings{fsId=Just "li",
                           fsLabel="Login",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX:fulano123")]} Nothing
    <*> areq passwordField FieldSettings{fsId=Just "qt",
                           fsLabel="Senha",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar login senha = runDB $ selectFirst [UsuarioLogin ==. login
                                             ,UsuarioSenha ==. senha] []
    
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    (widget2, enctype2) <- generateFormPost formPesquisa
    (widget3, enctype3) <- generateFormPost formCadUser
    let nomePagina = "Efetuar Login:" :: Text
    userlogado <- lookupSession "_ID"
    defaultLayout $ do 
        addStylesheet $ (StaticR css_login_css)
        addStylesheet $ (StaticR css_bootstrap_css)
        toWidget $ $(whamletFile "templates/menu.hamlet")
        toWidget $ $(whamletFile "templates/login.hamlet") 
        [whamlet| <script>document.getElementById("top").style.display = "none";</script>|]
        case userlogado of
            Just "admin" -> do
                let nomePagina = "Efetuar Cadastro:" :: Text
                toWidget $ [whamlet| <script>document.getElementById("register-form").style.display = "block"; 
                                     document.getElementById("login-form").style.display = "none"; |]
            Just userlog -> do
                redirect HomeR
            Nothing -> do
                toWidget $ [whamlet| <script>document.getElementById("register-form").style.display = "none"; 
                                     document.getElementById("login-form").style.display = "block"; |]


postLoginR :: Handler Html
postLoginR = do
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("root@root.com","root") -> do 
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (login,senha) -> do 
            usuario <- autenticar login senha 
            case usuario of 
                Nothing -> do 
                    setMessage $ [shamlet| Usuario ou senha invalido |]
                    redirect LoginR 
                Just (Entity usuid usuario) -> do 
                    setSession "_ID" (usuarioLogin usuario)
                    redirect HomeR
        _ -> redirect HomeR
                

postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
    
formCadUser :: Form Usuario
formCadUser = renderDivs $ Usuario
    <$> areq textField FieldSettings{fsId=Just "nm",
                           fsLabel="Login",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX:Fulano da Silva")]} Nothing
    <*> areq emailField FieldSettings{fsId=Just "log",
                           fsLabel="Email",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX:fulanoS123")]} Nothing
    <*> areq passwordField FieldSettings{fsId=Just "psw",
                           fsLabel="Senha",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing


postCadUserR :: Handler Html
postCadUserR =  do 
    ((result,_),_) <- runFormPost formCadUser
    case result of
        FormSuccess usuario -> do
            talvezUsuario <- runDB $ getBy (UniqueLogin (usuarioLogin usuario))
            case talvezUsuario of
                Nothing -> do
                        talvezEmail <- runDB $ getBy (EmailUsuario (usuarioEmail usuario))
                        case talvezEmail of
                            Nothing -> do
                                _ <- runDB $ insert usuario
                                setMessage $ [shamlet| <script> alert("Usario cadastrado com sucesso!");</script> |] 
                                redirect HomeR
                            Just email -> do
                                setMessage $ [shamlet| <script> alert("Este email ja existe!!");</script> |] 
                                redirect LoginR                                
                Just usuario -> do
                        setMessage $ [shamlet| <script> alert("Este login ja existe!!");</script> |] 
                        redirect LoginR