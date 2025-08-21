-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import StatSem.Utils
import StatSem.Decl
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import qualified Errors         as E
import qualified Data.Map.Lazy  as Map

-- Main annotation function.
annotate :: Decls -> AnnDecls
annotate decls = annDecls preludeEnv decls

-- Function to retrieve errors from an annotated AST.
getErrs :: AnnDecls -> [String]
getErrs pd@AS.ProgramDecl { AS.declsAnnot = DeclAnn { errs = errs } } = checkMain pd ++ errs
getErrs AS.ProgramDecl { AS.declsAnnot = _ } = error "Unreachable: `ProgramDecl` annotated with an annotation other than `DeclAnn`."

-- Check if there is the main procedure.
checkMain :: AnnDecls -> [String]
checkMain AS.ProgramDecl { AS.declsAnnot = DeclAnn { bindings = Env { routines = routinesEnv } } } = 
    case Map.lookup (AS.Ident "main" LG.defaultPosn) routinesEnv of
        Just (Proc _ [])    -> []
        Just (Proc _ _)     -> [": " ++ E.prettyerr ++ ": `main` procedure must have no parameters."]
        Just (Fun _ _ _)    -> [": " ++ E.prettyerr ++ ": `main` routine must be a procedure."]
        _                   -> [": " ++ E.prettyerr ++ ": Missing `main` procedure."]
checkMain AS.ProgramDecl { AS.declsAnnot = _ } = error "Unreachable: `ProgramDecl` annotated with an annotation other than `DeclAnn`."