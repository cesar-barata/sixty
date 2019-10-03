{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language PackageImports #-}
{-# language RoleAnnotations #-}
module Syntax where

import Protolude hiding (Type, IntMap)

import Data.HashMap.Lazy (HashMap)
import Unsafe.Coerce

import "this" Data.IntMap (IntMap)
import Binding (Binding)
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Index
import qualified Meta
import qualified Name
import Plicity
import qualified Span
import Syntax.Telescope (Telescope)

data Term v
  = Var !(Index v)
  | Global !Name.Qualified
  | Con !Name.QualifiedConstructor
  | Meta !Meta.Index
  | Let !Binding !(Term v) !(Type v) !(Scope Term v)
  | Pi !Binding !(Type v) !Plicity !(Scope Type v)
  | Fun !(Type v) !Plicity !(Type v)
  | Lam !Binding !(Type v) !Plicity !(Scope Term v)
  | App !(Term v) !Plicity !(Term v)
  | Case !(Term v) (Branches v) !(Maybe (Term v))
  | Spanned !Span.Relative !(Term v)
  deriving (Eq, Show, Generic, Hashable)

type Type = Term

type Branches v = HashMap Name.QualifiedConstructor (Span.Relative, Telescope Type Term v)

implicitPi :: Binding -> Type v -> Plicity -> Scope Type v -> Type v
implicitPi name type_ plicity =
  Pi name type_ (implicitise plicity)

apps :: Foldable f => Term v -> f (Plicity, Term v) -> Term v
apps =
  foldl (\fun (plicity, arg) -> App fun plicity arg)

appsView :: Term v -> (Term v, Tsil (Plicity, Term v))
appsView term =
  case term of
    App t1 plicity t2 ->
      second (Tsil.:> (plicity, t2)) $ appsView t1

    Spanned span term' ->
      case appsView term' of
        (hd, Tsil.Empty) ->
          (Spanned span hd, Tsil.Empty)

        result ->
          result

    _ ->
      (term, mempty)

varView :: Term v -> Maybe (Index v)
varView term =
  case term of
    Var v ->
      Just v

    Spanned _ term' ->
      varView term'

    _ ->
      Nothing

funs :: Foldable f => f (Term v) -> Plicity -> Term v -> Term v
funs args plicity res =
  foldr (\a b -> Fun a plicity b) res args

succ :: Term v -> Term (Succ v)
succ =
  coerce

fromVoid :: Term Void -> Term v
fromVoid =
  coerce

coerce :: Term v -> Term v'
-- Can't be Data.Coerce.coerce anymore due to role limitations for Telescopes
coerce =
  unsafeCoerce

type MetaSolutions =
  IntMap Meta.Index (Syntax.Term Void, Syntax.Type Void)

data Definition
  = TypeDeclaration !(Type Void)
  | ConstantDefinition !(Term Void)
  | DataDefinition (Telescope Type ConstructorDefinitions Void)
  deriving (Show, Generic, Hashable)

newtype ConstructorDefinitions v =
  ConstructorDefinitions (HashMap Name.Constructor (Type v))
  deriving (Show, Generic, Hashable)

constructorFieldPlicities :: Type v -> [Plicity]
constructorFieldPlicities type_ =
  case type_ of
    Pi _ _ plicity type' ->
      plicity : constructorFieldPlicities type'

    Fun _ plicity type' ->
      plicity : constructorFieldPlicities type'

    _ ->
      []
