module Main where

import Prelude

import Data.Array
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Nullable
import Data.List

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)

import React (ReactClass, ReadWrite, ReactState, Event, ReactThis,
              createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.AddressBook
import Data.AddressBook.Validation

main :: Eff (console :: CONSOLE, dom :: DOM) Unit
main = void do
  log "Rendering address book component"
