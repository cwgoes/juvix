module Main where

import qualified Compile as Compile
import qualified Config as Config
import Development.GitRev
import Juvix.Library
import Options
import Options.Applicative
import System.Directory
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Text.RawString.QQ

context :: IO Context
context = do
  pwd <- getCurrentDirectory
  home <- getHomeDirectory
  return (Context pwd home)

main :: IO ()
main = do
  ctx <- context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run ctx =<< execParser opts

disclaimerDoc :: Doc
disclaimerDoc =
  mconcat
    [ "This is ",
      red "experimental",
      " software released for research purposes only – use at your own risk.",
      line,
      "Juvix may diverge from canonical"
        <> "protocol implementations in unexpected ways."
    ]

aboutDoc :: Doc
aboutDoc =
  mconcat
    [ text
        ( "Juvix smart contract language compiler,"
            <> "debugging toolkit, & stateful deployment system"
        ),
      line,
      text
        ( "(c) Christopher Goes 2018-2019, "
            <> "(c) Cryptium Labs / Metastate 2019-2020 • https://juvix.org"
        ),
      line,
      disclaimerDoc
    ]

versionDoc :: Doc
versionDoc =
  mconcat
    [ aboutDoc,
      line <> line,
      mconcat ["Prerelease version.", line],
      mconcat
        [ "Built from branch ",
          white $(gitBranch),
          " at commit ",
          magenta $(gitHash),
          " (commit date ",
          cyan $(gitCommitDate),
          ").",
          line
        ]
    ]

interactiveDoc :: Doc
interactiveDoc =
  mconcat
    [ aboutDoc,
      line,
      white
        [r|
     | \ \   / /\ \/ (_)
  _  | |\ \ / /  \  /| |
 | |_| | \ V /   /  \| |
  \___/   \_/   /_/\_\_|
|],
      mconcat
        [ line,
          "Juvix interactive alpha.",
          line,
          "Currently supported backends: "
            <> "in-process interpreter, in-process interaction net.",
          line,
          "Coming soon: Michelson, LLVM, WASM.",
          line,
          "Enter :? for help. Enter :tutorial for an interactive tutorial.",
          line
        ]
    ]

run :: Context -> Options -> IO ()
run _ctx (Options cmd configPath) = do
  maybeConfig <- Config.loadT configPath
  case cmd of
    Parse fin -> do
      Compile.parse fin >> pure ()
    Typecheck fin backend -> do
      Compile.typecheck fin backend >> pure ()
    Compile fin fout backend -> do
      Compile.compile fin fout backend
    Version -> do
      putDoc versionDoc
      exitSuccess
    {-
    Interactive -> do
      putDoc interactiveDoc
      if isJust maybeConfig
        then putStrLn ("Loaded runtime configuration from " <> configPath <> "\n")
        else putStrLn ("Loaded default runtime configuration.\n" :: Text)
      Interactive.interactive ctx conf
      exitSuccess
    -}
    _ -> do
      putText "Not yet implemented!"
      exitFailure
