import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["hakyll"]

    phony "pull" $ do
        putNormal "Pulling from github"
        cmd "git pull"

    phony "css" $ do
        need ["pull"]
        putNormal "Generating CSS files"
        unit $ cmd "npm install"
        unit $ cmd "node_modules/.bin/grunt prepare"

    phony "hakyll" $ do
        need ["css"]
        putNormal "Generating HTML files"
        unit $ cmd (Cwd "hakyll") "stack build"
        unit $ cmd (Cwd "hakyll") "stack exec site rebuild"

    want [".bootstrap_fonts", ".fontawesome_fonts"] 
    ".bootstrap_fonts" %> copyFiles "node_modules/bootstrap/dist/fonts" "hakyll/fonts"
    ".fontawesome_fonts" %> copyFiles "node_modules/font-awesome/fonts" "hakyll/fonts"

    want [".fullpage_css"]
    ".fullpage_css" %> copyFile "node_modules/fullpage.js" "jquery.fullPage.css" "hakyll/css"

    want [".jquery_js", ".bootstrap_js", ".jquery_easing_js", ".fullpage_js"]
    ".jquery_js" %> copyFile "node_modules/jquery/dist" "jquery.min.js" "hakyll/js"
    ".bootstrap_js" %> copyFile "node_modules/bootstrap/dist/js" "bootstrap.min.js" "hakyll/js"
    ".jquery_easing_js" %> copyFile "node_modules/jquery.easing" "jquery.easing.min.js" "hakyll/js"
    ".fullpage_js" %> copyFile "node_modules/fullpage.js" "jquery.fullPage.js" "hakyll/js"


copyFile from filename to = \out -> do
        copyFile' (from </> filename) (to </> filename)
        writeFile' out ""

copyFiles from to = \out -> do
        files <- getDirectoryFiles from ["*"]
        forM_ files $ \file -> do
            copyFile' (from </> file) (to </> file)
        writeFile' out ""

