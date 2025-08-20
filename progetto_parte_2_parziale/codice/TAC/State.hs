module TAC.State where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified Types                     as T
import TAC.Def
import Data.Set

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCount, staticCount :: Int,
    -- The routine stack and the current routine is used to menage nested routines
    routineStack :: [Instructions],
    currentRoutine :: Instructions,
    -- Current stream for TAC code
    stream :: Instructions,
    -- Static data stream
    staticStream :: [StaticData],
    -- List of labels to be attached to the next instruction
    nextLabel :: [Label],
    -- current loop labels stack
    loopLabelsStack :: [(Label, Label)],
    -- If the functions for string/array comparisons are already generated
    stringComp :: Set AS.RelOp,
    arrayComp :: Set (AS.RelOp, T.Type)
}

initState :: TACState
initState = TACState {
    tempCount = 0,
    labelCount = 0,
    staticCount = 0,
    routineStack = [],
    currentRoutine = [],
    stream = [],
    staticStream = [],
    nextLabel = [],
    loopLabelsStack = [],
    stringComp = empty,
    arrayComp = empty
}

-- Add instruction to the stream
outLabeled :: LabeledInstruction -> StateMonad ()
outLabeled instrLab = do
    state <- St.get
    let -- Add pending label
        instrLab' = instrLab { label = nextLabel state }
        newStream = instrLab' : currentRoutine state
    St.put $ state { currentRoutine = newStream, nextLabel = [] }

-- Add instruction (with no label) to the stream
out :: Instruction -> StateMonad ()
out = outLabeled . simpleInstruction

-- Add static data to the static stream
outStatic :: StaticValue -> StateMonad Address
outStatic instrLab = do
    state <- St.get
    let k = staticCount state
        newStream = Static (Stat k) instrLab : staticStream state
    St.put $ state { staticStream = newStream,
                     staticCount = k + 1 }
    return (Stat k)

-- Return a new temporary variable
newTmp :: StateMonad Address
newTmp = do
    state <- St.get
    let k = tempCount state
    St.put $ state { tempCount = k + 1 }
    return $ Temp (Temporary k)

getOutAddr :: Maybe Address -> StateMonad Address
getOutAddr Nothing = newTmp
getOutAddr (Just addr) = return addr

-- Return a new label
newLabel :: StateMonad Label
newLabel = do
    state <- St.get
    let k = labelCount state
    St.put $ state { labelCount = k + 1 }
    return $ Loc k

-- Attach a label to the next instruction
attachToNext :: Label -> StateMonad ()
attachToNext label = do
    state <- St.get
    St.put $ state { nextLabel = label : nextLabel state }

-- Call when starting a new routine, pushes the current routine onto the stack
-- This is used to menage nested routines
newRoutine :: StateMonad ()
newRoutine = do
    state <- St.get
    St.put $ state { routineStack = currentRoutine state : routineStack state,
                     currentRoutine = [] }

-- Call when ending a routine (cannot be called before a newRoutine)
-- This is used to menage nested routines
endRoutine :: StateMonad ()
endRoutine = do
    state <- St.get
    let currentRout = currentRoutine state
    case routineStack state of
        [] -> St.put state
        (r:rs) -> do
            St.put $ state { routineStack = rs,
                             currentRoutine = r,
                             stream = currentRout ++ stream state }

-- Call when starting a new loop. Creates a new labels for the loop start and
-- end and pushes them onto a stack.
newLoop :: StateMonad ()
newLoop = do
    loopStart <- newLabel
    loopEnd <- newLabel
    attachToNext loopStart
    state <- St.get
    St.put $ state { loopLabelsStack = (loopStart, loopEnd) : loopLabelsStack state }

-- Call when ending a loop. Attaches the loop end label to the next instruction
-- Only call when exiting a loop
endLoop :: StateMonad ()
endLoop = do
    state <- St.get
    let (_, loopEnd) : stack = loopLabelsStack state
    attachToNext loopEnd
    -- Get updated state
    state' <- St.get
    St.put $ state' { loopLabelsStack = stack }

-- Only call when inside a loop
getLoopLabels :: StateMonad (Label, Label)
getLoopLabels = do
    head . loopLabelsStack <$> St.get

isStringCompGen :: AS.RelOp -> StateMonad Bool
isStringCompGen relOp = do
    state <- St.get
    return $ relOp `member` stringComp state

addStringCompGen :: AS.RelOp -> StateMonad ()
addStringCompGen relOp = do
    state <- St.get
    St.put $ state { stringComp = insert relOp (stringComp state) }

isArrayCompGen :: (AS.RelOp, T.Type) -> StateMonad Bool
isArrayCompGen relOp = do
    state <- St.get
    return $ relOp `member` arrayComp state

addArrayCompGen :: (AS.RelOp, T.Type) -> StateMonad ()
addArrayCompGen relOp = do
    state <- St.get
    St.put $ state { arrayComp = insert relOp (arrayComp state) }
