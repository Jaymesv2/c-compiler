
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Compiler.Parser.Grammar
import Compiler.Parser.Lexer
import Compiler.Parser.Preprocessor
import Compiler.SymbolTable 

--import Compiler.Parser.GrammarHelpers

import Data.Text.IO qualified as TIO
import System.Environment

import Data.Kind
import GHC.TypeLits


-- import System.IO.Error

import Effectful
import Effectful.State.Static.Local

import Conduit

import System.Console.GetOpt
import Data.Maybe (fromMaybe)
-- import System.Console.CmdArgs.Implicit
-- 
-- data CompilerArgs = CArgs {file_path :: String, include_paths :: [String]} deriving stock (Show, Data, Typeable)
-- 
-- default_search_path :: [String]
-- 
-- cargs = CArgs{file_path = def &= help "The file to compile" &= argPos 0, include_paths= def &= help "The include path" &= opt default_search_path}
--          &= summary "Sample v1"


data Flag
 = 
 Version
 -- Verbose  
 -- | Input String 
 | Output String 
 | LibDir String
   deriving Show

data Options = Options {
    optShowVersion :: Bool,
    optIncludePath :: [String],
    optOutput :: String
}

defaultSearchPath :: [String]
defaultSearchPath = ["/usr/include/sys"]

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optIncludePath = defaultSearchPath,
    optOutput = "a.out"
}


options :: [OptDescr (Options -> Options)]
options =
    -- [ Option ['v']     ["verbose"]
    --     (NoArg (\ opts -> opts { optVerbose = True }))
    --     "chatty output on stderr"
    [ Option ['?'] ["version"]
        (NoArg (\ opts -> opts { optShowVersion = True }))
        "show version number"
    , Option ['o']     ["output"]
        (OptArg ((\ f opts -> opts { optOutput = f }) . fromMaybe "output")
                "FILE")
        "output FILE"
    -- , Option ['c']     []
    --     (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
    --             "FILE")
    --     "input FILE"
    , Option ['I']     []
        (ReqArg (\ d opts -> opts { optIncludePath = optIncludePath opts ++ [d] }) "DIR")
        "library directory"
    ]
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."



main :: IO ()
main = do
    args_ <- getArgs
    (opts, args) <- compilerOpts args_

    -- This should only fail on weird platforms that I could care less about targeting.
    --currentPath <- fromJust executablePath 
    
    let path = case args of
            [] -> error "Cannot find a program to parse"
            h : _ -> h -- will throw exception if the file doesn't exist
    source <- TIO.readFile  path
    let preprocOptions = PPOptions {
        includePaths = optIncludePath opts
    }
    --runEff (alexPrintCondTokens source)
    --res <- runEff (evalState newParserState (runAlex source $ runPreprocessor $ runConduit (preprocess .| injectTypeNameTokens .| sinkList)))
    res <- runEff (evalState newParserScope (runAlex source path $ runPreprocessor preprocOptions $ runConduit (preprocess .| injectTypeNameTokens .| clike)))
    print res

    {-case res of
        Left (_, err) -> error $ "failed to parse with error: " ++ err
        Right tree -> print $ head tree-}

    pure ()

-- For withUtf8, see https://serokell.io/blog/haskell-with-utf8

--type Effec = (Type -> Type) -> Type -> Type

