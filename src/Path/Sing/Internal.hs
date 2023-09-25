{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Path.Sing.Internal where

import Control.Exception (SomeException)
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Name, Q, appE, conE)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter), quoteExp)
import Language.Haskell.TH.Syntax (Lift)
import Path (toFilePath)
import Path qualified

-- | A DataKind that indicates whether the path is absolute or relative.
data Base = Abs | Rel

-- | A DataKind that indicates whether the path reveals a file or a directory.
data FsType = File | Dir

-- | Mapping to the tag representing the path's base in the Path library.
type family PathBase b where
    PathBase 'Abs = Path.Abs
    PathBase 'Rel = Path.Rel

-- | Mapping to the tag representing the path's type in the Path library.
type family PathFsType t where
    PathFsType 'File = Path.File
    PathFsType 'Dir = Path.Dir

-- | A singleton tag that indicates whether the path is absolute or relative.
data SBase b where
    SAbs :: SBase 'Abs
    SRel :: SBase 'Rel

deriving instance Show (SBase b)
deriving instance Eq (SBase b)
deriving instance Ord (SBase b)
deriving instance Lift (SBase b)

instance Hashable (SBase b) where
    hashWithSalt = hashUsing @Bool \case
        SAbs -> False
        SRel -> True

-- | A singleton tag that indicates whether the path reveals a file or a directory.
data SFsType t where
    SFile :: SFsType 'File
    SDir :: SFsType 'Dir

deriving instance Show (SFsType t)
deriving instance Eq (SFsType b)
deriving instance Ord (SFsType b)
deriving instance Lift (SFsType b)

instance Hashable (SFsType b) where
    hashWithSalt = hashUsing @Bool \case
        SFile -> False
        SDir -> True

-- | A singleton-type wrapper of the original Path type.
data Path b t
    = Path (SBase b) (SFsType t) (Path.Path (PathBase b) (PathFsType t))
    deriving stock (Generic, Eq, Ord, Lift)
    deriving anyclass (Hashable)

-- | Path of some type.
data SomePath = forall b t. SomePath (Path b t)

-- | Path of some base.
data SomeBase t = forall b. SomeBase (Path b t)

-- | Path of some type.
data SomeFsType b = forall t. SomeFsType (Path b t)

{- | Path of some type.
     The difference with `SomeFsType` is that information on whether the path is a file or
    directory is not distinguished here and is ambiguous.
-}
data UnknownFsType b = UnknownFsType (SBase b) (Path.Path (PathBase b) Path.File)
    deriving (Generic, Eq, Ord, Lift)
    deriving anyclass (Hashable)

-- | Convert to a String.
pathToString :: Path b t -> String
pathToString (Path _ _ path) = toFilePath path

-- | Append two paths.
(</>) :: Path b 'Dir -> Path 'Rel t -> Path b t
(Path b _ p) </> (Path _ t q) = Path b t (p Path.</> q)

{- | Reinterpret file paths as directory paths.
     It is also the operation of adding a '/' at the end.
-}
fileToDirPath :: forall b. Path b 'File -> Path b 'Dir
fileToDirPath (Path b SFile p) = case b of
    SRel -> convert Path.parseRelDir
    SAbs -> convert Path.parseAbsDir
  where
    convert
        :: (FilePath -> Either SomeException (Path.Path (PathBase b) Path.Dir))
        -> Path b 'Dir
    convert parseDir =
        case parseDir $ Path.toFilePath $ Path.filename p of
            Left e -> error $ "Failed to convert path type from file to directory: " <> show e
            Right p' -> Path b SDir p'

-- | A variant of `Path.absfile` that constructs a version of `Path` wrapped in a singleton.
absfile :: QuasiQuoter
absfile = applyPathWrapper 'SAbs 'SFile Path.absfile

-- | A variant of `Path.relfile` that constructs a version of `Path` wrapped in a singleton.
relfile :: QuasiQuoter
relfile = applyPathWrapper 'SRel 'SFile Path.relfile

-- | A variant of `Path.absdir` that constructs a version of `Path` wrapped in a singleton.
absdir :: QuasiQuoter
absdir = applyPathWrapper 'SAbs 'SDir Path.absdir

-- | A variant of `Path.reldir` that constructs a version of `Path` wrapped in a singleton.
reldir :: QuasiQuoter
reldir = applyPathWrapper 'SRel 'SDir Path.reldir

-- | Construct a `UnknownFsType` `SAbs`.
abspath :: QuasiQuoter
abspath = applyUnknownFsTypeWrapper 'SAbs Path.absfile

-- | Construct a `UnknownFsType` `SRel`.
rel :: QuasiQuoter
rel = applyUnknownFsTypeWrapper 'SRel Path.relfile

applyPathWrapper :: Name -> Name -> QuasiQuoter -> QuasiQuoter
applyPathWrapper base fsType = applyExp (conE 'Path `appE` conE base `appE` conE fsType `appE`)

applyUnknownFsTypeWrapper :: Name -> QuasiQuoter -> QuasiQuoter
applyUnknownFsTypeWrapper base = applyExp (conE 'UnknownFsType `appE` conE base `appE`)

applyExp :: (Q Exp -> Q Exp) -> QuasiQuoter -> QuasiQuoter
applyExp f qq@QuasiQuoter {quoteExp} =
    qq {quoteExp = f . quoteExp}