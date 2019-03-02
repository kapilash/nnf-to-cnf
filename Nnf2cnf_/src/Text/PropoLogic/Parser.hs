module Text.PropoLogic.Parser where

import Text.PropoLogic.Data
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Data.Functor.Identity    
import Control.Monad.Combinators.Expr     
implFreeNnfToCnf = undefined

type Parser = Parsec Dec String                   

--skipSpace :: Parser ()    
skipSpace = do
  c <- spaceChar
  return ()

--spaceConsumer :: Parser ()         
spaceConsumer = L.space skipSpace (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")                   

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

parens = between (symbol "(") (symbol ")")

variable :: Parser PLVar         
variable = lexeme variable'
    where
      variable' = do
        f <- letterChar
        r <- many alphaNumChar
        return $ PLVar (f:r)

simpleExpr :: Parser LogicalExpr               
simpleExpr = do
  v <- variable
  return $ VAR v

rawExpr :: Parser LogicalExpr
rawExpr = makeExprParser term [[rNegExpr],[rAndExpr, rOrExpr, rImplExpr, rIffExpr]] <?> "boolean expression"

term :: Parser LogicalExpr
term = parens rawExpr <|> simpleExpr <?> "term"

rAndExpr = InfixL $ do
           v <-  symbol "&&"
           return AND
rOrExpr = InfixL $ do
            v <- symbol "||"
            return OR
rImplExpr = InfixL $ do
              v <- symbol "=>"
              return IF
rNegExpr = Prefix $ do
             v <- symbol "~"
             return NOT

rIffExpr = InfixL $ do
             v <- symbol "<=>"
             return IFF
-- rImplExpr l r = return $ IF l r
  
tester :: String -> IO ()
tester txt = parseTest rawExpr txt

parseFile :: FilePath -> IO ()
parseFile f = do
          txt <- readFile f
          case parse rawExpr f txt of
               Left err   -> putStr (parseErrorPretty err)
               Right expr -> case nnfImplFreeToCnf expr of
                               Nothing -> putStrLn "Is not NNF or Impl free"
                               Just r  -> print r
