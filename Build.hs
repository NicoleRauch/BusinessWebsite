import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["HTML"]

    "pull" ~> do
        putNormal "Pulling from github"
        cmd "git pull"

        need ["pull"]
    "css" ~> do
        putNormal "Generating CSS files"
        need [".bootstrap_less"]
        need [".bootstrap_custom_variables_less"]
        need [".custom_less"]
        unit $ cmd "npm install"
        unit $ cmd "node_modules/.bin/grunt less"

    ".bootstrap_less" ~> copyFiles "node_modules/bootstrap/less" "build/stylesheets/less"
    ".bootstrap_custom_variables_less" ~> copyRenameFile "node_modules/bootstrap/less/variables.less" "build/stylesheets/less/original-variables.less"
    ".custom_less" ~> copyFiles "frontend/less" "build/stylesheets/less"

    "HTML" ~> do
        need ["css"]
        need [".fullpage_css"]
        putNormal "Generating HTML files"
        unit $ cmd (Cwd "hakyll") "stack build"
        unit $ cmd (Cwd "hakyll") "stack exec site rebuild"
        need [".bootstrap_fonts", ".fontawesome_fonts"] 
        need [".jquery_js", ".bootstrap_js", ".jquery_easing_js", ".fullpage_js"]

    ".bootstrap_fonts" ~> copyFiles "node_modules/bootstrap/dist/fonts" "HTML/fonts"
    ".fontawesome_fonts" ~> copyFiles "node_modules/font-awesome/fonts" "HTML/fonts"

    ".fullpage_css" ~> copyFile "node_modules/fullpage.js" "jquery.fullPage.css" "hakyll/css"

    ".jquery_js" ~> copyFile "node_modules/jquery/dist" "jquery.min.js" "HTML/js"
    ".bootstrap_js" ~> copyFile "node_modules/bootstrap/dist/js" "bootstrap.min.js" "HTML/js"
    ".jquery_easing_js" ~> copyFile "node_modules/jquery.easing" "jquery.easing.min.js" "HTML/js"
    ".fullpage_js" ~> copyFile "node_modules/fullpage.js" "jquery.fullPage.js" "HTML/js"

    
copyRenameFile from to = do
    copyFile' from to

copyFile from filename to = do
        unit $ cmd "mkdir -p" to
        copyFile' (from </> filename) (to </> filename)

copyFiles from to = do
        unit $ cmd "mkdir -p" to
        files <- getDirectoryFiles from ["*"]
        forM_ files $ \file -> do
            copyFile' (from </> file) (to </> file)

