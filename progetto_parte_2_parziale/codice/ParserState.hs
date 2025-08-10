module ParserState where

import Control.Monad.State
import LexGrammar (Token)

type ParserState = Maybe Token

type ParserM a = StateT ParserState (Either String) a

saveToken :: Token -> ParserM ()
saveToken tok = put (Just tok)

getToken :: ParserM (Maybe Token)
getToken = get

lastToken :: ParserState -> Maybe Token
lastToken = id

runParser :: ParserM a -> ParserState -> Either String (a, ParserState)
runParser parser initialState = runStateT parser initialState


