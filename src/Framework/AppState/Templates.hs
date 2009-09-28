module Framework.AppState.Templates (
  TemplateMonad
, templateDir
, getDirectoryGroup
, getTemplate
, template
, templateWith
, assign
) where

import Control.Monad.State
import Control.Monad.Reader (asks)
import Data.Maybe (fromJust)
import Text.StringTemplate
import Text.StringTemplate.Classes
import Happstack.Server.SimpleHTTP (ServerPartT, Response, toResponse)

import Framework.AppState.Types (AppServerPartT, templateGroup)

-- | Template state monad type. Puts a 'StringTemplate' in a 'State' monad.
type TemplateMonad = State (StringTemplate String)

-- | Return default templates dir. /Not configurable for now./
templateDir :: FilePath
templateDir = "templates"

-- | Return 'STGroup' for default template dir (using 'templateDir').
getDirectoryGroup :: IO (STGroup String)
getDirectoryGroup = directoryGroup templateDir

-- | Get the template associated with given name from the template group
-- in the 'AppServerPartT' state. /Note: This produces an error if the template couldn't be retrieved!/
getTemplate ::  String                                -- ^ Name of the template in the template group.
            -> AppServerPartT (StringTemplate String) -- ^ The returned StringTemplate. See 'template' and 'templateWith' for easier usage.
getTemplate name = do
    dirGroup <- asks templateGroup
    return $ (fromJust.stGetFirst.dirGroup) name

-- | Gets a template from the system using the default template group (using 'getTemplate') and
-- renders it out to a response.
--
-- Usage example:
--
-- > index ::  AppServerPartT Response
-- > index = do
-- >    asHtml
-- >    template "index"
template :: FilePath -> AppServerPartT Response
template path = do
    tpl <- getTemplate path
    (return.toResponse.toString) tpl

-- | As 'template' but run in a special monad to assign parameters.
--
-- Usage example:
--
-- > index ::  AppServerPartT Response
-- > index = do
-- >    let items = ["Item1", "Item2", "Item3"]
-- >    templateWith "index" (assign "items" items)
--
-- Or:
--
-- > index ::  AppServerPartT Response
-- > index = do
-- >    let items = ["Item1", "Item2", "Item3"]
-- >    let otherItems = ["Other1", "Other2", "Other3"]
-- >    templateWith "index" $ do
-- >        assign "items" items
-- >        assign "otherItems" otherItems
templateWith :: FilePath -> TemplateMonad a -> AppServerPartT Response
templateWith path f = do
    tpl <- getTemplate path
    let finalTpl = execState f tpl
    (return.toResponse.toString) finalTpl

-- | Used within a 'TemplateMonad' to assign parameters to values.
-- See also 'templateWith'.
assign ::  (ToSElem a) => String -> a -> TemplateMonad ()
assign key value = do
    tpl <- get
    let newTpl = setAttribute key value tpl
    put newTpl
