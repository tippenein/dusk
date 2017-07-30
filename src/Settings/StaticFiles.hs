module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)

-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--     js_script_js
-- If the identifier is not available, you may use:
--     StaticFile ["js", "script.js"] []
staticFiles (appStaticDir compileTimeAppSettings)
