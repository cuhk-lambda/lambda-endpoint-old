{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           Control.Monad.Logger (LogSource)
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Text.Hamlet          (hamletFile)
import           Text.Jasmine         (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
-- import           Yesod.Auth.Dummy
-- import qualified Data.CaseInsensitive as CI
-- import qualified Data.Text.Encoding   as TE
-- import           Yesod.Auth.OpenId    (IdentifierType (Claimed), authOpenId)
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe    as Unsafe
import           Yesod.Default.Util   (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App =
  App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem =
  MenuItem
    { menuItemLabel          :: Text
    , menuItemRoute          :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a
   = forall (m :: * -> *). (MonadIO m) =>
                             ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
  approot :: Approot App
  approot =
    ApprootRequest $ \app req ->
      case appRoot $ appSettings app of
        Nothing   -> getApprootText guessApproot app req
        Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout :: Widget -> Handler Html
  defaultLayout _widget = do
    _master <- getYesod
    _mmsg <- getMessage
    _mcurrentRoute <- getCurrentRoute
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        -- Define the menu items of the header.
    let _menuItems = []
    let _navbarLeftMenuItems = [x | NavbarLeft x <- _menuItems]
    let _navbarRightMenuItems = [x | NavbarRight x <- _menuItems]
    let _navbarLeftFilteredMenuItems =
          [x | x <- _navbarLeftMenuItems, menuItemAccessCallback x]
    let _navbarRightFilteredMenuItems =
          [x | x <- _navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
    _pc <-
      widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
  addStaticContent ::
       Text -- ^ The file extension
    -> Text -- ^ The MIME content type
    -> LByteString -- ^ The contents of the file
    -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
        -- Generate a unique filename based on the content itself
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
    appShouldLogAll (appSettings app) ||
    level == LevelWarn || level == LevelError
  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
