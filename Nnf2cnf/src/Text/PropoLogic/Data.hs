module Text.PropoLogic.Data where

import qualified Data.List as Lst
import qualified Data.Set as Set    

newtype PLVar = PLVar String
    deriving (Eq,Show,Ord)


data LogicalExpr = VAR PLVar
             | NOT LogicalExpr
             | OR LogicalExpr LogicalExpr
             | AND LogicalExpr LogicalExpr
             | IF LogicalExpr LogicalExpr
             | IFF LogicalExpr LogicalExpr

instance Show LogicalExpr where
    show (VAR (PLVar s)) = s
    show (NOT x) = '~':(show x)
    show (OR x y) = "(" ++ (show x) ++ " || "  ++ (show y) ++ ")"
    show (AND x y) = "(" ++ (show x) ++ " && "  ++ (show y) ++ ")"
    show (IF x y) = "(" ++ (show x) ++ " => "  ++ (show y) ++ ")"
    show (IFF x y) = "(" ++ (show x) ++ " <=> "  ++ (show y) ++ ")"

data CnfLiteral = CnfVar String
                | CnfNot String
                deriving (Eq,Ord)

instance Show CnfLiteral where
         show (CnfVar s) = s
         show (CnfNot x) = '~':x

data CnfClause = CnfClause (Set.Set CnfLiteral)

instance Eq CnfClause where
    (CnfClause s1) == (CnfClause s2) = (Set.isSubsetOf s1 s2) || (Set.isSubsetOf s2 s1)

instance Show CnfClause where
      show (CnfClause xs) = "(" ++ (Lst.intercalate " || " (map show . Set.toAscList $ xs)) ++ ")"

newtype Cnf = Cnf [CnfClause]

instance Show Cnf where
         show (Cnf cs) = Lst.intercalate " && " . map show  $ cs

isNnf :: LogicalExpr -> Bool
isNnf (VAR _) = True
isNnf (NOT (VAR _)) = True
isNnf (NOT _) = False
isNnf (OR l r) = (isNnf l) && (isNnf r)
isNnf (AND l r) = (isNnf l) && (isNnf r)
isNnf (IF l r) = (isNnf l) && (isNnf r)
isNnf (IFF l r) =  (isNnf l) && (isNnf r)

isImplFree :: LogicalExpr -> Bool
isImplFree (VAR _) = True
isImplFree (IF _ _) = False
isImplFree (IFF _ _) = False
isImplFree (NOT x) = isImplFree x
isImplFree (AND l r) = isImplFree l && (isImplFree r)
isImplFree (OR l r) =  isImplFree l && (isImplFree r)

nnfImplFreeToCnf expr = if isNnf expr && (isImplFree expr)
                        then Just . exprToCnf . toCnf $ expr 
                        else Nothing
    where toCnf e@(VAR (PLVar s)) = e
          toCnf e@(NOT (VAR (PLVar s))) = e
          toCnf (AND l r) = AND (toCnf l) (toCnf r)
          toCnf (OR l r) = distr (toCnf l) (toCnf r)

          distr (AND l r) e2 = AND (distr l e2) (distr r e2)
          distr e1 (AND l r) = AND (distr e1 l) (distr e1 r)
          distr e1 e2 = OR e1 e2

          exprToCnf (VAR (PLVar x)) = Cnf [CnfClause . Set.singleton $ CnfVar x]
          exprToCnf (NOT (VAR (PLVar x))) = Cnf [CnfClause . Set.singleton $ CnfVar x]
          exprToCnf e@(OR _ _) = Cnf [(exprToClause e)]
          exprToCnf (AND l r)  = case (exprToCnf l, exprToCnf r) of
                         (Cnf left, Cnf right) -> Cnf . Lst.nub $ (left ++ right)
          exprToCnf e          = error $ "invalid call to convert to cnf " ++ (show e)

          exprToClause e  = CnfClause . Set.fromList $ toLiterals e

          toLiterals (VAR (PLVar x)) = [CnfVar x]
          toLiterals (NOT (VAR (PLVar x))) = [CnfNot x]
          toLiterals (OR l1 l2) = toLiterals l1 ++ (toLiterals l2)
          toLiterals (AND l1 l2) = toLiterals l1 ++ (toLiterals l2)
          toLiterals (IF l1 l2) = toLiterals l1 ++ (toLiterals l2)
          toLiterals (IFF l1 l2) = toLiterals l1 ++ (toLiterals l2)
