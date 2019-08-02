{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Query where

import Protolude

import Data.GADT.Compare.TH
import Data.HashMap.Lazy (HashMap)
import Rock

import Error (Error)
import qualified Module
import Name (Name)
import qualified Name
import qualified Position
import qualified Presyntax
import qualified Query.Mapped as Mapped
import Scope (Scope)
import qualified Scope
import qualified Span
import qualified Syntax
import Syntax.Telescope (Telescope)

data Query a where
  InputFiles :: Query [FilePath]
  FileText :: FilePath -> Query Text
  ModuleFile :: Mapped.Query Name.Module FilePath a -> Query a
  ParsedFile :: FilePath -> Query (Name.Module, Module.Header, [(Position.Absolute, (Name, Presyntax.Definition))])
  ModuleHeader :: Name.Module -> Query Module.Header
  ImportedNames :: Name.Module -> Mapped.Query Name.Pre Scope.Entry a -> Query a
  ParsedModuleMap :: Name.Module -> Query (HashMap (Scope.Key, Name) Presyntax.Definition)
  ModulePositionMap :: Name.Module -> Query (HashMap (Scope.Key, Name) Position.Absolute)
  ParsedDefinition :: Scope.KeyedName -> Query (Maybe Presyntax.Definition)
  Scopes :: Name.Module -> Query ((Scope, Scope.Visibility), Scope.Module)
  ResolvedName :: Scope.KeyedName -> Name.Pre -> Query (Maybe Scope.Entry)
  Visibility :: Scope.KeyedName -> Name.Qualified -> Query Scope.Key
  ElaboratedType :: Name.Qualified -> Query (Syntax.Type Void)
  ElaboratedDefinition :: Name.Qualified -> Query (Maybe (Syntax.Definition, Syntax.Type Void))
  ConstructorType :: Name.QualifiedConstructor -> Query (Telescope Syntax.Type Syntax.Type Void)
  ErrorSpan :: Error -> Query (FilePath, Span.Absolute)
  KeyedNameSpan :: Scope.KeyedName -> Query (FilePath, Span.Absolute)

fetchModuleFile :: MonadFetch Query m => Name.Module -> m FilePath
fetchModuleFile module_ = do
  maybeFilePath <- fetch $ ModuleFile $ Mapped.Query module_
  -- TODO error message
  pure $
    fromMaybe
      ("fetchModuleFile: no such module " <> show module_)
      maybeFilePath

fetchImportedName
  :: MonadFetch Query m
  => Name.Module
  -> Name.Pre
  -> m (Maybe Scope.Entry)
fetchImportedName module_ =
  fetch . ImportedNames module_ . Mapped.Query

deriving instance Show (Query a)

deriveGEq ''Query
deriveGCompare ''Query

instance HashTag Query where
  hashTagged query =
    case query of
      InputFiles {} -> hash
      FileText {} -> hash
      ModuleFile q -> hashTagged q
      ParsedFile {} -> hash
      ModuleHeader {} -> hash
      ImportedNames _ q -> hashTagged q
      ParsedModuleMap {} -> hash
      ModulePositionMap {} -> hash
      ParsedDefinition {} -> hash
      Scopes {} -> hash
      ResolvedName {} -> hash
      Visibility {} -> hash
      ElaboratedType {} -> hash
      ElaboratedDefinition {} -> hash
      ConstructorType {} -> hash
      ErrorSpan {} -> hash
      KeyedNameSpan {} -> hash
