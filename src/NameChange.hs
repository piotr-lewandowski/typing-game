{-# LANGUAGE TemplateHaskell #-}

module NameChange where

import Config (Name)
import InputEvents
import Lens.Micro
import Lens.Micro.TH

data NameChangeState = NameChangeState {_currentName :: Name, _newName :: Name} deriving (Show)

makeLenses ''NameChangeState

handleNameChangeInput :: InputEvents -> NameChangeState -> Either Name NameChangeState
handleNameChangeInput (Typed c) nameChange =
    Right $
        if length (nameChange ^. newName) < 10
            then nameChange & newName <>~ [c]
            else nameChange
handleNameChangeInput Backspace nameChange = Right $ nameChange & newName %~ init
handleNameChangeInput Confirm nameChange =
    Left $
        if null (nameChange ^. newName)
            then nameChange ^. currentName
            else nameChange ^. newName
handleNameChangeInput Exit nameChange = Left $ nameChange ^. currentName
handleNameChangeInput _ nameChange = Right nameChange