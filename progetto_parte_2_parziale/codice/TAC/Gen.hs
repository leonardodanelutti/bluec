module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import TAC.Def

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCoount :: Int,
    stream :: TAC
}


newtmp :: StateMonad Address
newtmp = do
    state <- St.get
    let k = tempCount state
    St.put (state { tempCount = k + 1 })
    return (Temp (Temporary k))

