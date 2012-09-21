-- | Experimental compilation via System FC.

module Language.Fay.Core where

import Control.Monad
import Control.Monad.Identity
import Data.Generics.Text.Extra
import Data.String
import Language.Core.Core
import Language.Core.ParseGlue
import Language.Core.Parser
import Language.Fay.Print ()
import Language.Fay.Types (JsExp(..),JsStmt(..),JsName,printJS)
import Prelude hiding (exp)
import System.Process.Extra

type Compile = Identity

-- | Compile the given Haskell file.
compileFile :: FilePath -> IO [JsStmt]
compileFile fp = do
  ast <- getCoreAst fp
  return (runIdentity (compileModule ast))

-- | Print the JS compiled from a file.
printCompileFile :: FilePath -> IO ()
printCompileFile fp = do
  js <- compileFile fp
  putStrLn $ printJS $ js

-- | Get the core AST of a Haskell file.
getCoreAst :: FilePath -> IO Module
getCoreAst fp = do
  string <- getCoreString fp
  case parse string 0 of
    FailP err -> error err
    OkP m -> return m

-- | Get the core AST of a Haskell file.
printCoreAstPretty :: FilePath -> IO ()
printCoreAstPretty = getCoreAst >=> putStrLn . gshow

-- | Get the ghc -fext-core output of a Haskell file.
getCoreString :: FilePath -> IO String
getCoreString fp = do
  result <- readAllFromProcess' "ghc" [fp,"-v0","-fext-core"] ""
  case result of
    Left err -> error err
    Right (_,_) -> readFile (reverse (dropWhile (/='.') (reverse fp)) ++ "hcr")

-- | Compile a module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module name types vals) = do
  typeDecls <- fmap concat (mapM compileType types)
  valDecls <- fmap concat (mapM compileVal vals)
  return [JsVar (fromString (show name))
                (JsFun [] (typeDecls ++ valDecls) Nothing)]

-- | Compile a type declaration.
compileType :: Tdef -> Compile [JsStmt]
compileType _ = return []

-- | Compile a value declaration.
compileVal :: Vdefg -> Compile [JsStmt]
compileVal val =
  case val of
    Nonrec vdef -> compileDef vdef
    _ -> return []

-- | Compile a non-recursive value definition.
compileDef :: Vdef -> Compile [JsStmt]
compileDef (Vdef (var,typ,exp)) = do
  e <- compileExp exp
  return [JsVar (fromString (qualToString var)) e]

-- | Convert a qualified thing to a name.
qualToName :: Qual String -> JsName
qualToName = fromString . qualToString

-- | Convert a qualified thing to a string.
qualToString :: Qual String -> String
qualToString (name,var) =
  show name ++ "." ++ var

-- | Compile an expression.
compileExp :: Exp -> Compile JsExp
compileExp exp =
  case exp of
    Var qvar   -> return (JsName (qualToName qvar))
    Dcon qcon  -> return (JsName (qualToName qcon))
    Lit lit    -> compileLit lit
    App op arg -> compileApp op arg
    Appt exp _ -> compileExp exp

-- | Compile a literal.
compileLit :: Lit -> Compile JsExp
compileLit lit =
  return JsNull

-- | Compile a function application.
compileApp :: Exp -> Exp -> Compile JsExp
compileApp op arg = do
  o <- compileExp op
  a <- compileExp arg
  return (JsApp o [a])
