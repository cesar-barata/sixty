{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module ClosureConverted.TypeOf where

import Protolude hiding (head)

import qualified Data.HashMap.Lazy as HashMap
import Rock

import qualified Binding
import qualified Builtin
import ClosureConverted.Context (Context)
import qualified ClosureConverted.Context as Context
import qualified ClosureConverted.Domain as Domain
import qualified ClosureConverted.Evaluation as Evaluation
import qualified ClosureConverted.Readback as Readback
import qualified ClosureConverted.Syntax as Syntax
import qualified Environment
import qualified Literal
import Monad
import qualified Name
import qualified Query
import Syntax.Telescope (Telescope)
import qualified Syntax.Telescope as Telescope

typeOfDefinition :: Context Void -> Syntax.Definition -> M (Syntax.Type Void)
typeOfDefinition context definition = do
  let
    env =
      Context.toEnvironment context
  typeValue <-
    case definition of
      Syntax.TypeDeclaration type_ ->
        Evaluation.evaluate env type_

      Syntax.ConstantDefinition term -> do
        value <- Evaluation.evaluate env term
        typeOf context value

      Syntax.FunctionDefinition tele ->
        Domain.Function <$> typeOfFunction context tele

      Syntax.DataDefinition _ ->
        pure $ Domain.global $ Name.Lifted Builtin.TypeName 0

      Syntax.ParameterisedDataDefinition tele ->
        Evaluation.evaluate env $
          Telescope.fold
            (\binding domain _ -> Syntax.Pi (Binding.toName binding) domain)
            (Telescope.hoist (const $ Syntax.Global $ Name.Lifted Builtin.TypeName 0) tele)

  Readback.readback (Context.toEnvironment context) typeValue

typeOfFunction
  :: Context v
  -> Telescope Syntax.Type Syntax.Term v
  -> M (Telescope Syntax.Type Syntax.Type v)
typeOfFunction context tele =
  case tele of
    Telescope.Empty body -> do
      let
        env =
          Context.toEnvironment context
      body' <- Evaluation.evaluate env body
      bodyType <- typeOf context body'
      bodyType' <- Readback.readback env bodyType
      pure $ Telescope.Empty bodyType'

    Telescope.Extend binding domain plicity target -> do
      domain' <- Evaluation.evaluate (Context.toEnvironment context) domain
      (context', _) <- Context.extend context domain'
      target' <- typeOfFunction context' target
      pure $ Telescope.Extend binding domain plicity target'


typeOf :: Context v -> Domain.Value -> M Domain.Type
typeOf context value =
  case value of
    Domain.Neutral head spine -> do
      headType <- typeOfHead context head
      typeOfApplication context headType spine

    Domain.Con con args -> do
      conType <- fetch $ Query.ClosureConvertedConstructorType con
      conType' <-
        Evaluation.evaluate (Context.toEnvironment context) $
          Telescope.fold
            (\binding domain _ -> Syntax.Pi (Binding.toName binding) domain)
            (Telescope.fromVoid conType)
      typeOfApplication context conType' args

    Domain.Lit lit ->
      case lit of
        Literal.Integer _ ->
          pure $ Domain.global $ Name.Lifted Builtin.IntName 0

    Domain.Pi {} ->
      pure $ Domain.global $ Name.Lifted Builtin.TypeName 0

    Domain.Function {} ->
      pure $ Domain.global $ Name.Lifted Builtin.TypeName 0

typeOfHead
  :: Context v
  -> Domain.Head
  -> M Domain.Type
typeOfHead context head =
  case head of
    Domain.Var var ->
      pure $ Context.lookupVarType var context

    Domain.Global global -> do
      type_ <- fetch $ Query.ClosureConvertedType global
      Evaluation.evaluate (Context.toEnvironment context) $ Syntax.fromVoid type_

    Domain.Case _ (Domain.Branches env branches defaultBranch) ->
      case defaultBranch of
        Just term -> do
          value' <- Evaluation.evaluate env term
          typeOf context value'

        Nothing ->
          case branches of
            Syntax.ConstructorBranches constructorBranches ->
              case HashMap.elems constructorBranches of
                branchTele:_ ->
                  typeOfTelescope context env branchTele

                [] ->
                  panic "TODO type of branchless case"

            Syntax.LiteralBranches literalBranches ->
              case HashMap.elems literalBranches of
                body:_ -> do
                  body' <- Evaluation.evaluate env body
                  typeOf context body'

                [] ->
                  panic "TODO type of branchless case"

typeOfApplication
  :: Context v
  -> Domain.Type
  -> [Domain.Value]
  -> M Domain.Type
typeOfApplication context outerType outerArgs =
  case (outerType, outerArgs) of
    (Domain.Function tele, _:_) -> do
      outerType' <-
        Evaluation.evaluate (Context.toEnvironment context) $
          Telescope.fold
            (\binding domain _ -> Syntax.Pi (Binding.toName binding) domain)
            (Telescope.fromVoid tele)
      go outerType' outerArgs

    _ ->
      go outerType outerArgs

  where
    go type_ args =
      case (type_, args) of
        (_, []) ->
          pure type_

        (Domain.Pi _ _ closure, arg:args') -> do
          target <- Evaluation.evaluateClosure closure arg
          go target args'

        _ ->
          panic "ClosureConverted.typeOf applying non-pi"

typeOfTelescope
  :: Context v'
  -> Domain.Environment v
  -> Telescope Syntax.Type Syntax.Term v
  -> M Domain.Type
typeOfTelescope context env tele =
  case tele of
    Telescope.Empty branch -> do
      branch' <- Evaluation.evaluate env branch
      typeOf context branch'

    Telescope.Extend _ type_ _ tele' -> do
      type' <- Evaluation.evaluate env type_
      (context', var) <- Context.extend context type'
      typeOfTelescope context' (Environment.extendVar env var) tele'