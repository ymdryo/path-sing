{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Path.Sing where

import Control.Exception (SomeException)
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import GHC.Generics (Generic)
import Path (toFilePath)
import qualified Path

data Base = Abs | Rel
data FsType = File | Dir

type family PathBase b where
    PathBase 'Abs = Path.Abs
    PathBase 'Rel = Path.Rel

type family PathFsType t where
    PathFsType 'File = Path.File
    PathFsType 'Dir = Path.Dir

data SBase b where
    SAbs :: SBase 'Abs
    SRel :: SBase 'Rel
deriving instance Show (SBase b)
deriving instance Eq (SBase b)
instance Hashable (SBase b) where
    hashWithSalt = hashUsing @Bool \case
        SAbs -> False
        SRel -> True

data SFsType t where
    SFile :: SFsType 'File
    SDir :: SFsType 'Dir
deriving instance Show (SFsType t)
deriving instance Eq (SFsType b)
instance Hashable (SFsType b) where
    hashWithSalt = hashUsing @Bool \case
        SFile -> False
        SDir -> True

data Path b t
    = Path (SBase b) (SFsType t) (Path.Path (PathBase b) (PathFsType t))
    deriving stock (Generic, Eq)
    deriving anyclass (Hashable)

data SomePath = forall b t. SomePath (Path b t)
data SomeBase t = forall b. SomeBase (Path b t)
data SomeFsType b = forall t. SomeFsType (Path b t)

data UnknownFsType b = UnknownFsType (SBase b) (Path.Path (PathBase b) Path.File)

pathToString :: Path b t -> String
pathToString (Path _ _ path) = toFilePath path

(</>) :: Path b 'Dir -> Path 'Rel t -> Path b t
(Path b _ p) </> (Path _ t q) = Path b t (p Path.</> q)

fileToDirPath :: forall b. Path b 'File -> Path b 'Dir
fileToDirPath (Path b SFile p) = case b of
    SRel -> convert Path.parseRelDir
    SAbs -> convert Path.parseAbsDir
  where
    convert ::
        (FilePath -> Either SomeException (Path.Path (PathBase b) Path.Dir)) ->
        Path b 'Dir
    convert parseDir =
        case parseDir $ Path.toFilePath $ Path.filename p of
            Left e -> error $ "Failed to convert path type from file to directory: " <> show e
            Right p' -> Path b SDir p'
