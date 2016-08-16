import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["hakyll"]

    phony "pull" $ do
        putNormal "Pulling from github"
        cmd "git pull"

    phony "css" $ do
        need ["pull"]
        putNormal "Generating CSS files"
        cmd "npm start"

    phony "hakyll" $ do
        need ["css"]
        putNormal "Generating HTML files"
        unit $ cmd (Cwd "hakyll") "stack build"
        unit $ cmd (Cwd "hakyll") "stack exec site rebuild"
