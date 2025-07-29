-- Implementation of the annotation rules for STATEMENTS.
module StatSem.Stmt where

import qualified AbsSyn as AS
import StatSem.Utils

-- TYPES
-- Void annotations after parsing.
type Stmt = AS.Statement ()
-- Annotated statements after static semantics analysis.
type AnnStmt = AS.Statement Ann

-- Annotate a statement node.
annStmt :: Env -> Stmt -> AnnStmt
annStmt env stmt@AS.StmtAss{} = annStmtAss env stmt
-- ...

-- Annotate an assignment statement.
annStmtAss :: Env -> Stmt -> AnnStmt
annStmtAss _ _ = AS.StmtAss {} --TODO: