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


copyFiles from to = \out -> do
        files <- getDirectoryFiles from ["*"]
        forM_ files $ \file -> do
            copyFile' (from </> file) (to </> file)
        writeFile' out ""

