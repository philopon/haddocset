{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Documentation.Haddocset.Index
    ( SearchIndex
    , ReadWrite
    , ReadOnly

    , EntryType(..)
    , IndexEntry(..)

    , withSearchIndex
    , withReadWrite
    , insert
    , sinkEntries
    ) where


import Data.Text            (Text)
import Distribution.Package (PackageId)
import Distribution.Text    (display)

import qualified Data.Conduit                   as C
import qualified Data.Conduit.List              as CL
import qualified Database.SQLite.Simple         as Sql
import qualified Database.SQLite.Simple.ToField as Sql

data ReadWrite

data ReadOnly

-- | A handle to a docset search index.
--
-- This will be tagged with 'ReadWrite' or 'ReadOnly'.
newtype SearchIndex a = SearchIndex Sql.Connection

data EntryType
    = PackageEntry
    | ModuleEntry
    | TypeEntry
    | ConstructorEntry
    | FunctionEntry
  deriving (Show, Ord, Eq)

instance Sql.ToField EntryType where
    toField PackageEntry = Sql.SQLText "Package"
    toField ModuleEntry = Sql.SQLText "Module"
    toField TypeEntry = Sql.SQLText "Type"
    toField ConstructorEntry = Sql.SQLText "Constructor"
    toField FunctionEntry = Sql.SQLText "Function"


-- | An entry in the search index.
data IndexEntry = IndexEntry
    { entryName    :: !Text
    , entryType    :: !EntryType
    , entryPath    :: !String
    , entryPackage :: !PackageId
    } deriving (Show, Ord, Eq)

instance Sql.ToRow IndexEntry where
    toRow IndexEntry{..} =
        Sql.toRow (entryName, entryType, entryPath, display entryPackage)

-- | Executes the given operation on the search index at the specified
-- location.
withSearchIndex :: FilePath -> (SearchIndex ReadOnly -> IO a) -> IO a
withSearchIndex path f =
    Sql.withConnection path $ \conn -> do
        Sql.execute_ conn
            "CREATE TABLE IF NOT EXISTS searchIndex \
                \ ( id INTEGER PRIMARY KEY \
                \ , name TEXT \
                \ , type TEXT \
                \ , path TEXT \
                \ , package TEXT \
                \ )"

        Sql.execute_ conn
          "CREATE UNIQUE INDEX IF NOT EXISTS \
                \ anchor ON searchIndex (name, type, path, package)"

        f (SearchIndex conn)


-- | Executes an operation on a 'ReadWrite' SearchIndex.
--
-- Opens a database transaction. If the operation fails for any reason, the
-- changes are rolled back.
withReadWrite :: SearchIndex ReadOnly -> (SearchIndex ReadWrite -> IO a) -> IO a
withReadWrite (SearchIndex conn) f =
    Sql.withTransaction conn $
        f (SearchIndex conn)

-- | Inserts an item into a SearchIndex.
insert :: SearchIndex ReadWrite -> IndexEntry -> IO ()
insert (SearchIndex conn) = Sql.execute conn insertStmt


insertStmt :: Sql.Query
insertStmt =
    "INSERT OR IGNORE INTO searchIndex \
        \ (name, type, path,package) VALUES (?, ?, ?, ?)"


-- | A sink to write index entries.
sinkEntries :: SearchIndex ReadWrite -> C.Consumer IndexEntry IO ()
sinkEntries searchIndex = CL.mapM_ (insert searchIndex)
