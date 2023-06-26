{-# LANGUAGE TemplateHaskell #-}

module NameChange where

import Config (Name)
import InputEvents
import Lens.Micro
import Lens.Micro.TH

data NameChangeState = NameChangeState {_currentName :: Name, _newName :: Name} deriving (Show)

makeLenses ''NameChangeState

handleNameChangeInput :: InputEvents -> NameChangeState -> Either Name NameChangeState
handleNameChangeInput (Typed c) nameChange = Right $ nameChange & newName <>~ [c]
handleNameChangeInput Backspace nameChange = Right $ nameChange & newName %~ init
handleNameChangeInput Confirm nameChange = Left $ nameChange ^. newName
handleNameChangeInput Exit nameChange = Left $ nameChange ^. currentName
handleNameChangeInput _ nameChange = Right nameChange