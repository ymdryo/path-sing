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

import Control.Applicative ((<|>))
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import Data.Text (Text)
import Data.Text qualified as T
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
    deriving stock (Generic, Eq, Ord, Lift, Show)
    deriving anyclass (Hashable)

-- | Path of some type.
data SomePath = forall b t. SomePath (Path b t)

deriving instance Show SomePath

-- | Path of some base.
data SomeBase t = forall b. SomeBase (Path b t)

deriving instance Show (SomeBase t)

-- | Path of some type.
data SomeFsType b = forall t. SomeFsType (Path b t)

deriving instance Show (SomeFsType t)

{- | Path of some type.
     The difference with `SomeFsType` is that information on whether the path is a file or
    directory is not distinguished here and is ambiguous.
-}
data UnknownFsType b = UnknownFsType (SBase b) (Path.Path (PathBase b) Path.File)
    deriving (Generic, Eq, Ord, Lift, Show)
    deriving anyclass (Hashable)

data SomeBaseUnknownFsType = forall b. SomeBaseUnknownFsType (UnknownFsType b)

deriving instance Show SomeBaseUnknownFsType

class IsFilePath a where
    -- | Convert to a String.
    pathToString :: a -> String

    -- | Convert to a Text.
    pathToText :: a -> Text
    pathToText = T.pack . pathToString
    {-# INLINE pathToText #-}

instance IsFilePath (Path b t) where
    pathToString (Path _ _ path) = toFilePath path
    {-# INLINE pathToString #-}

instance IsFilePath SomePath where
    pathToString (SomePath path) = pathToString path
    {-# INLINE pathToString #-}

instance IsFilePath (SomeBase t) where
    pathToString (SomeBase path) = pathToString path
    {-# INLINE pathToString #-}

instance IsFilePath (SomeFsType b) where
    pathToString (SomeFsType path) = pathToString path
    {-# INLINE pathToString #-}

instance IsFilePath (UnknownFsType b) where
    pathToString (UnknownFsType _ path) = toFilePath path
    {-# INLINE pathToString #-}

instance IsFilePath SomeBaseUnknownFsType where
    pathToString (SomeBaseUnknownFsType path) = pathToString path
    {-# INLINE pathToString #-}

forgetFsType :: Path b 'File -> UnknownFsType b
forgetFsType (Path b _ p) = UnknownFsType b p

parseSomeDir :: FilePath -> Maybe (SomeBase 'Dir)
parseSomeDir path =
    Path.parseSomeDir path
        >>= Just . \case
            Path.Abs absDir -> SomeBase $ Path SAbs SDir absDir
            Path.Rel relDir -> SomeBase $ Path SRel SDir relDir

parseSomeFile :: FilePath -> Maybe (SomeBase 'File)
parseSomeFile path =
    Path.parseSomeFile path
        >>= Just . \case
            Path.Abs absFile -> SomeBase $ Path SAbs SFile absFile
            Path.Rel relFile -> SomeBase $ Path SRel SFile relFile

parseUnknownFsType :: FilePath -> Maybe SomeBaseUnknownFsType
parseUnknownFsType path = do
    SomeBase (Path b _ p) <- parseSomeFile path
    Just $ SomeBaseUnknownFsType $ UnknownFsType b p

data DirOrUnknownFsType b
    = DirPath (Path b 'Dir)
    | UnknownFsTypePath (UnknownFsType b)
    deriving (Generic, Eq, Ord, Lift, Show)
    deriving anyclass (Hashable)

data SomeBaseDirOrUnknownFsType = forall b. SomeBaseDirOrUnknownFsType (DirOrUnknownFsType b)

deriving instance Show SomeBaseDirOrUnknownFsType

instance IsFilePath (DirOrUnknownFsType b) where
    pathToString = \case
        DirPath path -> pathToString path
        UnknownFsTypePath path -> pathToString path
    {-# INLINE pathToString #-}

instance IsFilePath SomeBaseDirOrUnknownFsType where
    pathToString (SomeBaseDirOrUnknownFsType path) = pathToString path
    {-# INLINE pathToString #-}

someBaseUnknownFsTypePath :: SomeBaseUnknownFsType -> SomeBaseDirOrUnknownFsType
someBaseUnknownFsTypePath (SomeBaseUnknownFsType p) =
    SomeBaseDirOrUnknownFsType $ UnknownFsTypePath p

someBaseDirPath :: SomeBase 'Dir -> SomeBaseDirOrUnknownFsType
someBaseDirPath (SomeBase p) =
    SomeBaseDirOrUnknownFsType $ DirPath p

parsePath :: FilePath -> Maybe SomeBaseDirOrUnknownFsType
parsePath path =
    (someBaseUnknownFsTypePath <$> parseUnknownFsType path)
        <|> (someBaseDirPath <$> parseSomeDir path)

-- | Append two paths.
(</>) :: Path b 'Dir -> Path 'Rel t -> Path b t
(Path b _ p) </> (Path _ t q) = Path b t (p Path.</> q)

{- |
Reinterpret the path as a directory path.
This can also be described as the action of appending a @\'/\'@ at the end.
-}
asDir :: forall b. UnknownFsType b -> Path b 'Dir
asDir (UnknownFsType b p) =
    case Path.parseRelDir $ Path.toFilePath $ Path.filename p of
        Right dirname -> Path b SDir (Path.parent p Path.</> dirname)
        Left e ->
            error $ "impossible: Failed to convert path type from file to directory: " <> show e

{- |
Reinterpret the path as a file path.
This can also be described as the action of appending a @\'/\'@ at the end.
-}
asFile :: forall b. UnknownFsType b -> Path b 'File
asFile (UnknownFsType b p) = Path b SFile p

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
