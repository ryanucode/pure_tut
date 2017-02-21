module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Entry = HasName (address :: Address)

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type HasName n =
  {firstName :: String, lastName :: String | n}

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

--emptyName :: forall a. HasName a
--emptyName = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

filterEntry :: (Entry -> Boolean) -> AddressBook -> AddressBook
filterEntry = filter

sameName :: forall a. HasName a -> HasName a -> Boolean
sameName a b = a.firstName == b.firstName && a.lastName == b.lastName

--findEntry :: String -> String -> AddressBook -> Maybe Entry
--findEntry firstName lastName =
  --head <<< filter (sameName (HasName firstName lastName))

--containsEntry :: String -> String -> AddressBook -> Boolean
--containsEntry firstName lastName = isJust <<< findEntry firstName lastName

--printEntry :: String -> String -> AddressBook -> Maybe String
--printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)

--removeDuplicates :: List Entry -> List Entry
--removeDuplicates = nubBy sameName

