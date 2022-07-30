{-# LANGUAGE ApplicativeDo #-}

module Main where

import qualified Data.Text.IO              as TIO
import qualified Data.Version              as V
import qualified Options.Applicative       as Opt
import           Paths_ToyJs
import           System.Console.ANSI
import           System.Directory
import           System.IO

import           Prettyprinter.Render.Text (hPutDoc)
import qualified ToyLang.Compile           as Compile

data Options = Options
  { inputFile      :: FilePath
  , outputFilename :: String
  }

showVersion :: V.Version -> String
showVersion v =
  unwords
    [ "ToyJs"
    , "version"
    , "-"
    , V.showVersion v
    ]

options :: Opt.Parser Options
options = do
  inputFile <-
    Opt.strArgument $
      Opt.help "Input file"
      <> Opt.metavar "FILE"
      <> Opt.action "file"

  outputFilename <-
    Opt.strOption $
      Opt.short 'o'
      <> Opt.long "output"
      <> Opt.help "Output file"
      <> Opt.metavar "NAME"

  Opt.infoOption (showVersion version) $
    Opt.short 'v'
    <> Opt.long "version"
    <> Opt.help "Show ToyJs version"

  pure Options {..}

parserInfo :: Opt.ParserInfo Options
parserInfo =
  Opt.info options $
    Opt.fullDesc
    <> Opt.progDesc "ToyJs is compiler."

main :: IO ()
main = do
  Options{..} <- Opt.customExecParser (Opt.prefs Opt.showHelpOnError) parserInfo
  existsSourceFile <- doesFileExist inputFile
  if existsSourceFile then do
    src <- TIO.readFile inputFile
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ "Compiling" ++ inputFile ++ " ..."

    case Compile.compile inputFile src of
      Left err -> do
        setSGR [SetColor Foreground Vivid Red]
        hPutStrLn stderr err

      Right out -> do
        withFile outputFilename WriteMode $ \f ->
          hPutDoc f out

        setSGR [SetColor Foreground Vivid Green]
        putStrLn "COMPLETE!"

    setSGR [Reset]

  else do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    hPutStrLn stderr $ inputFile ++ " is not exists."
    hSetSGR stderr [Reset]
