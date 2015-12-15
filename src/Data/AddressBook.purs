module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.firstName ++ ", " ++
                  entry.lastName ++ ": " ++
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street ++ ", " ++ 
                   addr.city ++ ", " ++ 
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

exampleAddress :: Address
exampleAddress = { street: "123 Fake St.", city: "Faketown", state: "CA" }

exampleEntry :: Entry
exampleEntry = { firstName: "John", lastName: "Smith", address: exampleAddress }

exampleBook :: AddressBook
exampleBook = insertEntry exampleEntry emptyBook

duplicateBook :: AddressBook
duplicateBook = insertEntry exampleEntry exampleBook

findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

nameExists :: String -> String -> AddressBook -> Boolean
nameExists firstName lastName = Data.Maybe.isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = Data.List.nubBy equalNames
  where
  equalNames :: Entry -> Entry -> Boolean
  equalNames a b = a.firstName == b.firstName && a.lastName == b.lastName
