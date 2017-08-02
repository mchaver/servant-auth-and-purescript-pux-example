import Distribution.Simple
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Process
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)

toolExists :: FilePath -> IO Bool
toolExists str = 
  (createProcess (proc str []) { std_out = CreatePipe
                               , std_err = CreatePipe
                               } >> return True)
    `catchIOError`
      (\e -> if isDoesNotExistError e then return False else return True)

main :: IO ()
main = do
  pulpExists <- toolExists "pulp"
  if pulpExists
    then do
      purescriptExists <- doesDirectoryExist "purescript"
      if purescriptExists
        then do 
          setCurrentDirectory "purescript"

          hPutStrLn stderr ""
          hPutStrLn stderr "Building purescript/src"
          hPutStrLn stderr "=========================\n"
          rawSystem "pulp" ["browserify", "--optimise", "--to", "index.js"]
          indexJs <- readFile "index.js"
          let indexHtmlContents = unlines 
                [ "<!DOCTYPE html>"
                , "<html>"
                , "<head>"
                , "  <meta charset=\"UTF-8\"/>"
                , "  <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\"/>"
                , "  <title>Servant Auth and Purescript Pux Example</title>"
                , "</head>"
                , "<body>"
                , "  <div name=\"app\" id=\"app\"></div>"
                , "  <script type=\"text/javascript\">"
                , indexJs
                , "  </script>"
                , "</body>"
                , "</html>"
                ]
          writeFile "../static/index.html" indexHtmlContents
              
          setCurrentDirectory ".."
          
          hPutStrLn stderr ""
          hPutStrLn stderr "Compiling servant-auth-and-purescript-pux-example"
          hPutStrLn stderr "======================================\n"
          defaultMain
          hPutStrLn stderr "servant-auth-and-purescript-pux-example build is complete."
        else do 
          hPutStrLn stderr "Unable to find the purescript directory."
    else do
      hPutStrLn stderr "Unable to complete servant-auth-and-purescript-pux-example build.\n Setup.hs cannot find 'pulp'. Make sure it is installed in your system and available on PATH."
