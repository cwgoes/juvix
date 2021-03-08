module Main where

import qualified Compile as Compile
import qualified Config as Config
import Development.GitRev
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
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

-- | Run the main program.
run :: Context -> Options -> IO ()
run ctx opt = do
  feedback <- Feedback.runFeedbackT $ run' ctx opt
  case feedback of
    Feedback.Success msgs _ -> mapM putStrLn msgs >> exitSuccess
    Feedback.Fail msgs -> mapM putStrLn msgs >> exitFailure
  where
    run' :: Context -> Options -> Compile.Pipeline ()
    run' _ (Options cmd _) = do
      case cmd of
        Parse fin ->
          do (liftIO $ readFile fin)
            >>= Compile.parse
            >>= liftIO . print
        Typecheck fin backend ->
          do (liftIO $ readFile fin)
            >>= Compile.parse
            >>= Compile.typecheck backend
            >>= liftIO . print
        Compile fin fout backend ->
          do (liftIO $ readFile fin)
            >>= Compile.parse
            >>= Compile.typecheck backend
            >>= Compile.compile backend
            >>= Compile.writeout fout
            >>= liftIO . print
        Version -> liftIO $ putDoc versionDoc
        _ -> Feedback.fail "Not implemented yet."
