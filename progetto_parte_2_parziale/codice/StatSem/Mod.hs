-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import StatSem.Utils
import StatSem.Decl
import AbsSyn as AS

-- Main annotation function.
annotate :: Decls -> AnnDecls
annotate decls = annDecls emptyEnv decls

-- Function to retrieve errors from an annotated AST.
getErrs :: AnnDecls -> [String]
getErrs AS.ProgramDecl { AS.declsAnnot = DeclAnn { errs = errs } } = errs
getErrs AS.ProgramDecl { AS.declsAnnot = _ } = error "Unreachable: `ProgramDecl` annotated with an annotation other than `DeclAnn`."