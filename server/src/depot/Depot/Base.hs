{-# OPTIONS_GHC -fglasgow-exts #-}

module Depot.Base 
    (module Depot.Base, module Depot.Util.Utils)
    where

import Data.Map as Map
import Data.Set as Set
import Data.Int
import Data.Word
import System.IO
import System.Time

import Data.PackedString
    
import Depot.Util.Utils
    
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State

    
-- Note the HTTP data types are in Depot.Com.Protocol.hs this will
-- remain until I can refactor them (naming conflicts with the
-- journaling actions and the http methods prevent them from being
-- listed in this file.

-- Application state -------------------------------------------------

data AppState = AppState { depotKeyAS   :: !Word64,
                           pageCachesAS :: !BTreeCache } deriving Show

-- btree query data types --------------------------------------------

type FreePtr = BlockPtr
type RootPtr = BlockPtr
data BPageHeader = BP { sanityBPH     :: !String,
                        versionBPH    :: !Version,
                        freeBlocksBPH :: !FreeBlocks,
                        rootPtrBPH    :: !RootPtr }
                 deriving Show

    
-- a return type for operations that add data to a btree. Insertion is
-- a good candidate for becoming a monad because much of the
-- operations (particularly splitting) could be hidden
data Insertion a   = Inserted !(BTreeIndex a)
                   | Updated !(BTreeIndex a)
                   | InsNext !BlockPtr
                   | InSplit !(Split a)
                   deriving (Show)


-- a split response contains the values of the previous node, split
-- into two (right and left) and either an incomplete root node, or
-- the median key to be inserted into the parent of the split pair.
data Split a       = SplitRoot { rootINS   :: !(BTreeIndex a), 
                                 leftINS   :: !(BTreeIndex a),
                                 rightINS  :: !(BTreeIndex a) }
                   | SplitNode { leftINS     :: !(BTreeIndex a),
                                 rightINS    :: !(BTreeIndex a),
                                 medKeyINS   :: !Integer }
                   deriving (Show)

data ParentSplit a   = NoParentSplit !(BTreeIndex a) 
                     | ParentSplit { whichParentPS :: !LeftOrRight,
                                     splitPS       :: !(Split a) }
                                     
data LeftOrRight = LeftNode | RightNode

-- An similar type to Maybe that allows for a LookNext operator and an
-- Incomplete operator, different levels of "not found", etc...
data Selection a   = Found [a]
                   | Incomplete [a] BlockPtr
                   | LookNext BlockPtr
                   | SelectionNotFound
                   | EmptySelection

-- for allowing comparisons on ranges (from negative infinity, to
-- positive infinity, etc..)
data RangeKey  = R Integer
               | NegativeInfinity
               | PositiveInfinity
                 deriving (Eq, Show)


-- btree index data types --------------------------------------------

-- TODO perhaps do a redblack tree here for storing branch keys
-- instead of a plain list
data BTreeIndex a   = Branch { parentBT :: !BlockPtr,
                               keysBT   :: ![Integer],
                               kidsBT   :: ![BlockPtr] }
                    | Leaf   { parentBT :: !BlockPtr,
                               valsBT   :: !(LeafMap Integer a),
                               prevBT   :: !PrevLeaf,
                               nextBT   :: !NextLeaf }
                    deriving (Show, Eq)                                   



-- -- type aliases for default indexes ----------------------------------
type IdxPtr = BTreeIndex BlockPtr
type IdxInt = BTreeIndex Integer
-- type IdxPSMult   = BTreeIndex PackedString [BlockPtr]
-- type IdxIntMult  = BTreeIndex Integer [BlockPtr]
-- type IdxInt      = BTreeIndex Integer BlockPtr
-- type IdxPtrInt   = BTreeIndex BlockPtr Integer


type RHOHS a      = (BTreeIndex a, Handle, Order, BPageHeader, Schema)
    
data BTreeEntry   = BEntry { versionBE      :: !Version,
                             pathBE         :: !FilePath,
                             lastOidBE      :: !Integer,
                             datHdlBE       :: !DataHandle,
                             lastWriteBE    :: !BlockPtr,
                             numEntriesBE   :: !Integer,
                             metaDataBE     :: !(Map String String),
                             -- TODO: reverse index (IdxInt)          
                             idxRootsBE     :: !(Map IndexName (RHOHS BlockPtr)) }
                  deriving (Show)

type BTreeCache = Map String (Handle, BTreeEntry)

data PermNodes  = PermNodes { idxDirPN      :: !(Map String (Handle, BTreeEntry)),
                              idxUserPassPN :: !(Map String  (Handle, BTreeEntry)),
                              idxNoncePN    :: !(Map String (Handle, BTreeEntry)) }

type Order        = Int
type LeafMap a b  = Map a b
type PrevLeaf     = BlockPtr
type NextLeaf     = BlockPtr

-- schema data for btree indexes -------------------------------------

data SchemaKeyTerm = SInt | SStr | SPtr | SHex
                     deriving (Eq, Show)

data SchemaValTerm = SVInt | SVStr | SVAuth | SVPri
                     deriving (Eq, Show)

data Schema     = Schema { nameSC  :: !IndexName,
                           keySC   :: !SchemaKeyTerm, 
                           valSC   :: !SchemaValTerm,
                           orderSC :: !Order } deriving (Eq, Show)


type IndexName  = String
type Version    = Int

-- IO data types for cascading btree nodes ---------------------------

data FreeBlocks = FB { nxtFB  :: !BlockPtr, 
                       freeFB :: ![BlockPtr] }
                deriving Show
                         
data Span = Span { startSPAN   :: !BlockPtr,
                   entriesSPAN :: !Int }
          deriving (Eq)

instance Show Span where
    show (Span s n) = "{Span " ++ show s ++ ":" ++ show n ++ "}"


-- data store types --------------------------------------------------
type FileId     = Word32
type Offset     = Word32
    
-- A pointer to the physical location of a resource. Denoted by file
-- number and file offset. The file number is open to interpretation
-- by the application. It may be representative of a sector in a
-- contiguous block or it may be a standalone file named with a
-- convention that matches the id.
--
-- The offset should be relative to the fileid
data BlockPtr = BPtr { bpFile   :: !FileId,
                       bpOffset :: !Offset }
              | BNilPtr -- serialised as the constant -1
                deriving (Eq, Ord)

instance Show BlockPtr where
    show BNilPtr    = "(NIL)"
    show (BPtr f o) = "(" ++ (show f) ++ "," ++ (show o) ++ ")"

                                                  
data DataHandle = DatHdl Handle | DatHdlNil deriving Show


-- btree concurrency types -------------------------------------------

-- journaling --------------------------------------------------------
data JournalAction = JrnInsert | JrnDelete deriving (Show, Ord, Eq)
data JournalEntry  = JournalAction ClockTime [(Integer, BlockPtr)]

-- HTTP Response type declarations -----------------------------------
-- should these accept extra headers to be stuffed into a response?

data DepotError = NotFound       { messageERR :: !String, headersERR :: ![(String, String)] }
                | UnAuthorized   { messageERR :: !String, headersERR :: ![(String, String)] }
                | Forbidden      { messageERR :: !String, headersERR :: ![(String, String)] }
                | BadRequest     { messageERR :: !String, headersERR :: ![(String, String)] }
                | RequestTimeout { messageERR :: !String, headersERR :: ![(String, String)] }
                | Conflict       { messageERR :: !String, headersERR :: ![(String, String)] }
                | ServerError    { messageERR :: !String, headersERR :: ![(String, String)] }
                | NotImplemented { messageERR :: !String, headersERR :: ![(String, String)] }

data DepotStatus = Ok      { messageSTAT :: !String, headersSTAT :: ![(String, String)] }
                 | Created { messageSTAT :: !String, headersSTAT :: ![(String, String)] }
                 | Deleted { messageSTAT :: !String, headersSTAT :: ![(String, String)] }

-- monad transformer types for error state and io composition
type ErrorStateIO e a = ErrorT e (StateT AppState IO) a
type DepotErrorState  = ErrorStateIO DepotError DepotStatus

-- Permissions -------------------------------------------------------

data ActionP    = Create | Retrieve | Update | Delete 
                  deriving (Show, Eq, Ord, Enum)

data Permission = Perm { pAct :: Set ActionP }
                | Allow Permission Permission
                | Deny Permission Permission
                | Try Permission Permission
                  deriving (Show, Eq)

data Authorize action = Allowed action
                      | Denied

data Principal  = Admin
                | Anonymous
                | User !String
                  deriving (Eq)

data UserAuth   = UserAuth { princeUA :: !Principal,
                             pass64UA :: ![Word64] }

data HTTPAuth   = HTTPAuth { usernameHA  :: !String,
                             realmHA     :: !String,
                             nonceHA     :: !String,
                             uriHA       :: !String,
                             qopHA       :: !String,
                             ncHA        :: !String,
                             cnonceHA    :: !String,
                             responseHA  :: !String,
                             opaqueHA    :: !String }


data DirAuth = DirAuth { dirDA   :: !Permission,
                         entryDA :: !Permission }
             deriving (Show, Eq)

instance Ord Principal where
    compare Anonymous Anonymous = EQ
    compare (User _) Anonymous  = GT
    compare Anonymous (User _)  = LT
    compare Admin Admin         = EQ
    compare Admin _             = GT
    compare _ Admin             = LT
    compare (User u) (User u')  = u `compare` u'


instance Show Principal where
    show Anonymous = "__ANON__"
    show Admin     = "admin"
    show (User u)  = u


principalFromString "__ANON__" = Anonymous
principalFromString "admin"    = Admin
principalFromString u          = User u

instance Show HTTPAuth where
    show auth = "Authorization: Digest " ++ 
                "username" .=. usernameHA auth  .#.
                "realm"    .=. realmHA auth     .#.
                "nonce"    .=. nonceHA auth     .#.
                "uri"      .=. uriHA auth       .#.
                "qop"      .=. qopHA auth       .#.
                "nc"       .=. show (ncHA auth) .#.
                "cnonce"   .=. cnonceHA auth    .#.
                "response" .=. responseHA auth  .#.
                "opaque"   .=. opaqueHA auth



-- Url Parsing -------------------------------------------------------
data UrlExpression = RootDir
                   | DName String
                   | KStar
                   | DoubleKStar
                     deriving (Show, Eq)

data QueryVal   = Scalar String
                | RangeExclusive String String -- UNUSED, (not implemented yet)
                | RangeInclusive String String
                | RangeFrom String
                | RangeTo String
                | CommaDelimited [QueryVal]
                | QueryMax 
                | QueryMin 
                | SemanticallyIncorrect String
                  deriving Show
                           

-- logging -----------------------------------------------------------

data LogLevel = LogError
              | LogWarn
              | LogInfo
              | LogDebug
                deriving (Eq, Ord, Enum)

                 
