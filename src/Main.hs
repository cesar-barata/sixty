{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Protolude hiding (force)

import qualified Data.Dependent.Map as DMap
import qualified Data.HashSet as HashSet
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Rock

import Error (Error)
import qualified Error
import qualified Name
import qualified Pretty
import Query (Query)
import qualified Query
import qualified Rules
import qualified Span

main :: IO ()
main = do
  [inputModule] <- getArgs
  parseAndTypeCheck (fromString inputModule)

runQueryTask :: Task Query a -> IO (a, [(FilePath, Span.LineColumn, Text, Error)])
runQueryTask task = do
  startedVar <- newMVar mempty
  errorsVar <- newMVar mempty
  let
    writeErrors :: Query a -> [Error] -> Task Query ()
    writeErrors q errs =
      unless (null errs) $
        liftIO $ modifyMVar_ errorsVar $ pure . DMap.insert q (Const errs)

    rules :: Rules Query
    rules =
      memoise startedVar $ writer writeErrors Rules.rules

  runTask sequentially rules $ do
    result <- task
    errorsMap <- liftIO $ readMVar errorsVar
    let
      errors =
        flip foldMap (DMap.toList errorsMap) $ \(_ DMap.:=> Const errs) ->
          errs
    spannedErrors <- forM errors $ \err -> do
      (filePath, span) <- fetch $ Query.ErrorSpan err
      text <- fetch $ Query.ReadFile filePath
      let
        trimmedSpan =
          Span.trim text span
        (lineColumn, lineText) =
          Span.lineColumn trimmedSpan text
      pure (filePath, lineColumn, lineText, err)
    pure (result, spannedErrors)

parseAndTypeCheck :: Name.Module -> IO ()
parseAndTypeCheck module_ = do
  ((), errs) <- runQueryTask $ do
    defs <- fetch $ Query.ParsedModule module_
    let
      names =
        HashSet.fromList $
          Name.Qualified module_ . fst . snd <$> defs
    forM_ names $ \name -> do
      type_ <- fetch $ Query.ElaboratedType name
      liftIO $ putDoc $ pretty name <> " : " <> Pretty.prettyTerm 0 Pretty.empty type_ <> line
      maybeDef <- fetch $ Query.ElaboratedDefinition name
      liftIO $ forM_ maybeDef $ \(def, _) ->
        putDoc $ Pretty.prettyDefinition name def <> line
  forM_ errs $ \(filePath, lineColumn, lineText, err) ->
    liftIO $ putDoc $ Error.pretty filePath lineColumn lineText err <> line
