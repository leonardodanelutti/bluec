-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import StatSem.Utils
import StatSem.Decl

-- Main annotation function.
annotate :: Decls -> AnnDecls
annotate decls = annDecls emptyEnv decls