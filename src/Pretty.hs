{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Pretty where

import Protolude

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Rock

import Index
import qualified Meta
import Name (Name(Name))
import qualified Name
import qualified Plicity
import Query (Query)
import qualified Query
import qualified Query.Mapped as Mapped
import qualified Scope
import qualified Syntax
import Syntax.Telescope (Telescope)
import qualified Syntax.Telescope as Telescope

-------------------------------------------------------------------------------
-- Pretty-printing environments

data Environment v = Environment
  { varNames :: Seq Name.Pre
  , usedNames :: HashMap Name.Pre (Maybe Scope.Entry)
  , importedConstructorAliases :: HashMap Name.QualifiedConstructor (HashSet Name.Pre)
  , importedAliases :: HashMap Name.Qualified (HashSet Name.Pre)
  }

extend :: Environment v -> Name -> (Environment (Succ v), Name.Pre)
extend env (Name name) =
  go (Name.Pre name : [Name.Pre $ name <> show (i :: Int) | i <- [0..]])
  where
    go (name':names)
      | name' `HashMap.member` usedNames env =
        go names
      | otherwise =
        (env
          { varNames = varNames env Seq.|> name'
          , usedNames = HashMap.insert name' Nothing (usedNames env)
          }
        , name'
        )
    go [] =
      panic "Pretty.extend"

empty :: Environment Void
empty = Environment
  { varNames = mempty
  , usedNames = mempty
  , importedConstructorAliases = mempty
  , importedAliases = mempty
  }

emptyM :: MonadFetch Query m => Name.Module -> m (Environment Void)
emptyM module_ = do
  importedNames <- fetch $ Query.ImportedNames module_ Mapped.Map
  ((localScope, _), _) <- fetch $ Query.Scopes module_
  (constrAliases, aliases) <- fetch $ Query.NameAliases module_
  pure Environment
    { varNames = mempty
    , usedNames = fmap Just $ importedNames <> localScope
    , importedConstructorAliases = constrAliases
    , importedAliases = aliases
    }

-------------------------------------------------------------------------------

prettyTerm :: Int -> Environment v -> Syntax.Term v -> Doc ann
prettyTerm prec env term =
  case term of
    Syntax.Var (Index i) ->
      pretty $
        Seq.index (varNames env) (Seq.length (varNames env) - i - 1)

    Syntax.Global global ->
      prettyGlobal env global

    Syntax.Con constr ->
      prettyConstr env constr

    Syntax.Meta (Meta.Index i) ->
      pretty
        ("?" <> show i :: Text)

    Syntax.Let name term' typ body ->
      prettyParen (prec > letPrec) $
        let
          (env', name') = extend env name
        in
        "let"
        <> line <> indent 2
          (pretty name' <+> ":" <+> prettyTerm 0 env typ
          <> line <> pretty name' <+> "=" <+> prettyTerm 0 env term'
          )
        <> line <> "in"
        <> line <> prettyTerm letPrec env' body

    Syntax.Pi {} ->
      prettyParen (prec > funPrec) $
        prettyPiTerm env term

    Syntax.Fun source domain ->
      prettyParen (prec > funPrec) $
        prettyTerm (funPrec + 1) env source <+> "->" <+> prettyTerm funPrec env domain

    Syntax.Lam {} ->
      prettyParen (prec > lamPrec) $
        "\\" <> prettyLamTerm env term

    Syntax.App t1 plicity t2 ->
      prettyParen (prec > appPrec) $
        prettyTerm appPrec env t1 <+> Plicity.prettyAnnotation plicity <> prettyTerm (appPrec + 1) env t2

    Syntax.Case scrutinee branches ->
      prettyParen (prec > casePrec) $
        "case" <+> prettyTerm 0 env scrutinee <+> "of" <> line <>
          indent 2
            (vcat
              [ prettyConstr env constr <+> prettyBranch env tele
              | Syntax.Branch constr tele <- branches
              ]
            )

prettyGlobal :: Environment v -> Name.Qualified -> Doc ann
prettyGlobal env global = do
  let
    aliases =
      sortOn (\(Name.Pre name) -> Text.length name) $
      filter (unambiguous env) $
      HashSet.toList $
      HashMap.lookupDefault mempty global $
      importedAliases env

  case aliases of
    [] ->
      pretty global

    alias:_ ->
      pretty alias

prettyConstr :: Environment v -> Name.QualifiedConstructor -> Doc ann
prettyConstr env constr = do
  let
    aliases =
      sortOn (\(Name.Pre name) -> Text.length name) $
      filter (unambiguous env) $
      HashSet.toList $
      HashMap.lookupDefault mempty constr $
      importedConstructorAliases env

  case aliases of
    [] ->
      pretty constr

    alias:_ ->
      pretty alias

unambiguous :: Environment v -> Name.Pre -> Bool
unambiguous env name =
  case HashMap.lookupDefault Nothing name $ usedNames env of
    Nothing ->
      True

    Just (Scope.Name _) ->
      True

    Just (Scope.Constructors cs) ->
      HashSet.size cs == 1

    Just (Scope.Ambiguous _ _) ->
      False

prettyLamTerm :: Environment v -> Syntax.Term v -> Doc ann
prettyLamTerm env term =
  case term of
    Syntax.Lam name typ plicity scope ->
      let
        (env', name') = extend env name
      in
      Plicity.prettyAnnotation plicity <> lparen <> pretty name' <+> ":" <+> prettyTerm 0 env typ <> rparen
      <> prettyLamTerm env' scope

    t ->
      "." <+> prettyTerm lamPrec env t

prettyPiTerm :: Environment v -> Syntax.Term v -> Doc ann
prettyPiTerm env term =
  case term of
    Syntax.Pi name typ plicity scope ->
      let
        (env', name') = extend env name
      in
      Plicity.prettyAnnotation plicity <> lparen <> pretty name' <+> ":" <+> prettyTerm 0 env typ <> rparen
      <> prettyPiTerm env' scope

    t ->
      " ->" <+> prettyTerm funPrec env t

prettyBranch
  :: Environment v
  -> Telescope Syntax.Type Syntax.Term v
  -> Doc ann
prettyBranch env tele =
  case tele of
    Telescope.Empty body ->
      "->" <> line <> indent 2 (prettyTerm casePrec env body)

    Telescope.Extend name type_ plicity tele' ->
      let
        (env', name') = extend env name
      in
      Plicity.prettyAnnotation plicity <> "(" <> pretty name' <+> ":" <+> prettyTerm 0 env type_ <> ")" <+>
      prettyBranch env' tele'

-------------------------------------------------------------------------------

prettyDefinition :: Environment Void -> Name.Qualified -> Syntax.Definition -> Doc ann
prettyDefinition env name def =
  case def of
    Syntax.TypeDeclaration type_ ->
      prettyGlobal env name <+> ":" <+> prettyTerm 0 env type_

    Syntax.ConstantDefinition term ->
      prettyGlobal env name <+> "=" <+> prettyTerm 0 env term

    Syntax.DataDefinition tele ->
      "data" <+> prettyGlobal env name <+> prettyConstructorDefinitions env tele

prettyConstructorDefinitions
  :: Environment v
  -> Telescope Syntax.Type Syntax.ConstructorDefinitions v
  -> Doc ann
prettyConstructorDefinitions env tele =
  case tele of
    Telescope.Empty (Syntax.ConstructorDefinitions constrs) ->
      "where" <> line <>
      indent 2
        (vcat
          [ pretty constr <+> ":" <+> prettyTerm 0 env type_
          | (constr, type_) <- constrs
          ]
        )

    Telescope.Extend name type_ plicity tele' ->
      let
        (env', name') = extend env name
      in
      Plicity.prettyAnnotation plicity <> "(" <> pretty name' <+> ":" <+> prettyTerm 0 env type_ <> ")" <+>
      prettyConstructorDefinitions env' tele'

-------------------------------------------------------------------------------

prettyParen :: Bool -> Doc a -> Doc a
prettyParen True doc = lparen <> doc <> rparen
prettyParen False doc = doc

funPrec, appPrec, lamPrec, letPrec, casePrec :: Int
funPrec = 0
appPrec = 10
lamPrec = 0
letPrec = 0
casePrec = 0
