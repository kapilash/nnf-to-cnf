module Text.PropoLogic.Data where

import qualified Data.List as Lst

newtype PLVar = PLVar String
    deriving (Eq,Show,Ord)


data LogicalExpr = VAR PLVar
             | NOT LogicalExpr
             | OR LogicalExpr LogicalExpr
             | AND LogicalExpr LogicalExpr
             | IF LogicalExpr LogicalExpr
             | IFF LogicalExpr LogicalExpr

toStr (VAR (PLVar s)) = s
toStr (NOT x) = '~':(show x)
toStr (OR x y) = "(" ++ (show x) ++ " || "  ++ (show y) ++ ")"
toStr (AND x y) = "(" ++ (show x) ++ " && "  ++ (show y) ++ ")"
toStr (IF x y) = "(" ++ (show x) ++ " => "  ++ (show y) ++ ")"
toStr (IFF x y) = "(" ++ (show x) ++ " <=> "  ++ (show y) ++ ")"

toLiterals e@(VAR _) = [e]
toLiterals e@(NOT _) = [e]
toLiterals (OR l1 l2) = toLiterals l1 ++ (toLiterals l2)
toLiterals (AND l1 l2) = toLiterals l1 ++ (toLiterals l2)
toLiterals (IF l1 l2) = toLiterals l1 ++ (toLiterals l2)
toLiterals (IFF l1 l2) = toLiterals l1 ++ (toLiterals l2)
                  
instance Show LogicalExpr where
         show = toStr

data CnfLiteral = CnfVar String
                | CnfNot String
                deriving Eq
                  

exprToLiteral (VAR (PLVar x)) = CnfVar x
exprToLiteral (NOT (VAR (PLVar x))) = CnfNot x
exprToLiteral x                    = error $ "invalid call to exprToLiteral " ++ (show x)
                  
instance Show CnfLiteral where
         show (CnfVar s) = s
         show (CnfNot x) = '~':x

data CnfClause = CnfSimple CnfLiteral
               | CnfClOr [CnfLiteral]

instance Eq CnfClause where
    (CnfSimple x) == (CnfSimple y) = x == y
    (CnfSimple x) == (CnfClOr xs) = case Lst.nub xs of
                                      [y] -> x == y
                                      _  ->  False
    (CnfClOr ys) == (CnfSimple x) = case Lst.nub ys of
                                      [y] -> x == y
                                      otherwise -> False
    (CnfClOr y1) == (CnfClOr y2) = case (Lst.nub y1) Lst.\\ (Lst.nub y2) of
                                     [] -> True
                                     otherwise -> False
                 
cnfClause2Literals (CnfSimple x) = [x]
cnfClause2Literals (CnfClOr xs) = xs


                                
exprToClause e  = e2c' (toLiterals e)
    where e2c' [y] = CnfSimple (exprToLiteral y)
          e2c' ys = CnfClOr . Lst.nub . map exprToLiteral $ ys
                          
instance Show CnfClause where
      show (CnfSimple x) = show x
      show (CnfClOr xs) = "(" ++ (Lst.intercalate " || " (map show xs)) ++ ")" 

data Cnf = Cnf1 CnfClause
          | CnfAnd Cnf Cnf

toClauses (Cnf1 c) = [c]
toClauses (CnfAnd c1 c2) = Lst.nub $ toClauses c1 ++ (toClauses c2)

cnfToLiteral (Cnf1 c) = cnfClause2Literals c
cnfToLiteral (CnfAnd c1 c2) = cnfToLiteral c1 ++ (cnfToLiteral c2)

                              
exprToCnf e@(VAR _) = Cnf1 (exprToClause e)
exprToCnf e@(NOT _) = Cnf1 (exprToClause e)
exprToCnf e@(OR _ _) = Cnf1 (exprToClause e)
exprToCnf (AND l r)  = CnfAnd (exprToCnf l) (exprToCnf r)
exprToCnf e          = error $ "invalid call to convert to cnf " ++ (show e)

                       
                          
instance Show Cnf where
         show (Cnf1 cs) = show cs
         show (CnfAnd c1 c2) = Lst.intercalate " && " . map show  $ (toClauses c1) ++ (toClauses c2)

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

