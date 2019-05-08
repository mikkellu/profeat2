{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Parser.Internal
  ( Language(..)
  , parseFile
  , parseFile'

  , model
  , specification

  , familyDef
  , family'

  , featureDef
  , feature
  , constraint
  , decomposition
  , rewards

  , controllerDef
  , moduleDef
  , moduleBody

  , globalDef
  , varDecl
  , varType
  , compoundVarType
  , simpleVarType

  , constantDef
  , constantType

  , formulaDef
  , formula

  , labelDef
  , label

  , propertyDef

  , stmt
  , update
  , assign
  , expr
  , name
  ) where

import Control.Lens hiding ( assign )
import Control.Monad.Identity

import Data.List.NonEmpty
import Data.Text.Lazy ( Text, pack )
import Data.Text.Lens

import Text.Parsec hiding ( Reply(..), label )
import Text.Parsec.Error ( errorMessages, showErrorMessages )
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Text.Lazy ()

import Error
import Syntax
import Syntax.Util
import Types

data Language = PrismLang | ProFeat

type UserState = Language
type Parser    = Parsec Text UserState

parseFile :: Parser a -> SourceName -> Text -> Either Error a
parseFile = parseFile' ProFeat

parseFile' :: Language -> Parser a -> SourceName -> Text -> Either Error a
parseFile' lang p src =
    over _Left toError . runParser (whiteSpace *> p <* eof) lang src

-- | Converts a 'ParseError' to an 'Error'.
toError :: ParseError -> Error
toError = Error <$> toSrcLoc . errorPos <*> toSyntaxError

-- | Converts Parsec's 'SourcePos' to a 'SrcLoc'.
toSrcLoc :: SourcePos -> SrcLoc
toSrcLoc = srcLoc <$> pack . sourceName <*> sourceLine <*> sourceColumn

-- | Converts Parsec's error message to an 'ErrorDesc'.
toSyntaxError :: ParseError -> ErrorDesc
toSyntaxError = SyntaxError . pack .
    showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEOF .
    errorMessages
  where
    msgOr         = "or"
    msgUnknown    = "unkown parse error"
    msgExpecting  = "expecting"
    msgUnExpected = "unexpected"
    msgEOF        = "end of input"

reservedNames, reservedOpNames :: [String]
reservedNames =
    [ "mdp", "dtmc", "ctmc", "family", "endfamily", "features", "feature"
    , "endfeature", "root", "global", "const", "formula", "label", "modules"
    , "all", "one", "some", "of", "optional", "as", "constraint", "initial"
    , "rewards", "endrewards", "controller", "endcontroller", "module"
    , "endmodule", "this", "active", "iactive", "activate", "deactivate"
    , "array", "bool", "int", "double", "init", "endinit", "invariant"
    , "endinvariant", "for", "endfor", "in", "id", "block", "min"
    , "max", "sample", "true", "false"
    ]
reservedOpNames =
    [ "/", "*", "-", "+", "=", "!=", ">", "<", ">=", "<=", "&", "|", "!"
    , "=>", "<=>", "->", "..", "...", "?", "."
    ]

languageDef :: T.GenLanguageDef Text UserState Identity
languageDef = T.LanguageDef
    { T.commentStart    = "/*"
    , T.commentEnd      = "*/"
    , T.commentLine     = "//"
    , T.nestedComments  = True
    , T.identStart      = getState >>= \case
                              PrismLang -> char '_' <|> letter
                              ProFeat   -> letter
    , T.identLetter     = alphaNum <|> char '_'
    , T.opStart         = oneOf "/*-+=!><&|.?"
    , T.opLetter        = oneOf "=.>?"
    , T.reservedNames   = reservedNames
    , T.reservedOpNames = reservedOpNames
    , T.caseSensitive   = True
    }

lexer :: T.GenTokenParser Text UserState Identity
lexer = T.makeTokenParser languageDef

T.TokenParser{..} = lexer

decimal' :: Parser Text
decimal' = T.lexeme lexer $ try $ do
    w <- many digit
    option (pack w) $ do
        void $ char '.'
        f <- many1 digit
        return . pack $ w ++ '.':f

bool :: Parser Bool
bool = False <$ reserved "false"
   <|> True  <$ reserved "true"

identifier' :: Parser Text
identifier' = pack <$> identifier

doubleQuotes :: forall a. Parser a -> Parser a
doubleQuotes = let quote = symbol "\"" in between quote quote

block :: String -> Parser a -> Parser a
block blockName p = braces p <|> (p <* reserved ("end" ++ blockName))

-- | The loc combinator annotates the value parsed by @p@ with its location
-- in the source file.
loc :: Parser (SrcLoc -> a) -> Parser a
loc p = do
    pos <- getPosition
    let src    = sourceName pos^.packed
        line   = sourceLine pos
        column = sourceColumn pos
    ($ srcLoc src line column) <$> p

model :: Parser LModel
model = do
    lang <- getState
    Model <$> modelType <*> many (choice $ definitions lang)
  where
    definitions ProFeat   = [ familyDef
                            , featureDef
                            , controllerDef
                            , moduleDef
                            , globalDef
                            , constantDef
                            , formulaDef
                            , labelDef
                            , initDef
                            , invariantDef
                            ]
    definitions PrismLang = [ moduleDef
                            , globalDef
                            , constantDef
                            , formulaDef
                            , labelDef
                            , initDef
                            , rewardsDef
                            ]

modelType :: Parser ModelType
modelType = option defaultModelType $ choice
  [ MDP  <$ reserved "mdp"
  , DTMC <$ reserved "dtmc"
  , CTMC <$ reserved "ctmc"
  ]

specification :: Parser LSpecification
specification = Specification <$> many (choice [ constantDef
                                               , formulaDef
                                               , labelDef
                                               , propertyDef
                                               ])

familyDef :: Parser LDefinition
familyDef = FamilyDef <$> family' <?> "family declaration"

family' :: Parser LFamily
family' = loc $ reserved "family" *> block "family"
    (Family <$> many varDecl <*> many constr <*> features)
  where
    constr = reserved "initial" *> reserved "constraint" *> expr <* semi
    features = option [] (reserved "features" *> commaSep1 name <* semi)

featureDef :: Parser LDefinition
featureDef = FeatureDef <$> feature <?> "feature"

feature :: Parser LFeature
feature = loc $ do
    isRoot <- option False (True <$ reserved "root")
    reserved "feature"
    ident <- if isRoot
                then return "root"
                else identifier'
    ps    <- params
    block "feature" $
        Feature isRoot ident ps
            <$> many varDecl
            <*> optionMaybe decomposition
            <*> many constraint
            <*> blockList
            <*> moduleList
            <*> many rewards
  where
    blockList  = option [] $ reserved "block" *> commaSep1 name <* semi
    moduleList = option [] $ reserved "modules" *> commaSep1 inst <* semi

constraint :: Parser LConstraint
constraint = loc $ Constraint <$> option False (True <$ reserved "initial")
                              <*> (reserved "constraint" *> expr <* semi)

decomposition :: Parser LDecomposition
decomposition = loc (Decomposition <$> (decompOp <* reserved "of")
                                   <*> (commaSep1 featureRef <* semi))
             <?> "decomposition"

decompOp :: Parser LDecompOp
decompOp =  AllOf  <$  reserved "all"
        <|> OneOf  <$  reserved "one"
        <|> SomeOf <$  reserved "some"
        <|> Group  <$> range
        <?> "decomposition operator"

featureRef :: Parser LFeatureRef
featureRef = FeatureRef <$> option False (True <$ reserved "optional")
                        <*> inst
                        <*> optionMaybe (reserved "as" *> identifier')
                        <*> optionMaybe (brackets expr)

inst :: Parser LInstance
inst = loc $ Instance <$> identifier' <*> option [] (args expr)

rewardsDef :: Parser LDefinition
rewardsDef = RewardsDef <$> rewards

rewards :: Parser LRewards
rewards = loc (Rewards <$> (reserved "rewards" *> doubleQuotes identifier')
                       <*> block "rewards" (repeatable reward many)) <?> "rewards"

reward :: Parser LReward
reward = loc (Reward <$> optionMaybe (brackets actionLabel)
                     <*> (expr <* colon)
                     <*> (expr <* semi))
      <?> "reward"

controllerDef :: Parser LDefinition
controllerDef = ControllerDef . Controller <$>
    (reserved "controller" *> block "controller" moduleBody)

moduleDef :: Parser LDefinition
moduleDef = fmap ModuleDef $ do
    reserved "module"
    ident <- identifier'
    ps    <- params
    block "module" $ Module ident ps <$> moduleBody

moduleBody :: Parser LModuleBody
moduleBody = loc (ModuleBody <$> many varDecl <*> repeatable stmt many)

globalDef :: Parser LDefinition
globalDef = GlobalDef <$> (reserved "global" *> varDecl)
         <?> "variable declaration"

varDecl :: Parser LVarDecl
varDecl = loc (VarDecl <$> identifier' <* colon
                       <*> varType
                       <*> optionMaybe (reserved "init" *> expr) <* semi)
       <?> "variable declaration"

varType :: Parser LVarType
varType = choice [ CompoundVarType <$> compoundVarType
                 , SimpleVarType   <$> simpleVarType
                 ]
       <?> "type"

compoundVarType :: Parser LCompoundVarType
compoundVarType = ArrayVarType <$> (reserved "array" *> range)
                               <*> (reserved "of" *> simpleVarType)
               <?> "type"

simpleVarType :: Parser LSimpleVarType
simpleVarType =  BoolVarType <$  reserved "bool"
             <|> IntVarType  <$> range
             <?> "base type"

constantDef :: Parser LDefinition
constantDef = ConstDef <$>
    loc (Constant <$> (reserved "const" *>
                       option IntConstType constantType)
                  <*> identifier'
                  <*> optionMaybe (reservedOp "=" *> expr) <* semi)
  <?> "constant definition"

constantType :: Parser ConstType
constantType =  BoolConstType   <$ reserved "bool"
            <|> IntConstType    <$ reserved "int"
            <|> DoubleConstType <$ reserved "double"
            <?> "type"

formulaDef :: Parser LDefinition
formulaDef = FormulaDef <$> formula

formula :: Parser LFormula
formula = loc (Formula <$> (reserved "formula" *> identifier')
                       <*> params
                       <*> (reservedOp "=" *> expr <* semi))
       <?> "formula"

labelDef :: Parser LDefinition
labelDef = LabelDef <$> label

label :: Parser LLabel
label = loc (Label <$> (reserved "label" *> doubleQuotes identifier')
                   <*> (reservedOp "=" *> expr <* semi))
     <?> "label definition"

initDef :: Parser LDefinition
initDef = InitDef <$> init' <?> "init definition"

init' :: Parser LInit
init' = loc (Init <$> (reserved "init" *> block "init" expr))

invariantDef :: Parser LDefinition
invariantDef = InvariantDef <$> invariant <?> "invariant definition"

invariant :: Parser LInvariant
invariant = loc (Invariant <$> (reserved "invariant" *> block "invariant" expr))

propertyDef :: Parser LDefinition
propertyDef = PropertyDef
    <$> loc (Property
        <$> optionMaybe (doubleQuotes identifier' <* colon)
        <*> property)
    <?> "property"


property :: Parser [LPropertyElem]
property = do
    (s, isEnd) <- anyChar `manyTill'` choice
        [ False <$ try (string "${")
        , True  <$ endOfLine
        , True  <$ semi
        ]
    let elemString = PropElemString (pack s)

    if isEnd
        then return [elemString]
        else do
            e <- whiteSpace *> expr <* whiteSpace <* char '}'
            elems <- property
            return (elemString : PropElemExpr e : elems)

stmt :: Parser LStmt
stmt =  loc (Stmt <$> brackets actionLabel
                  <*> expr <* reservedOp "->"
                  <*> updates)
    <?> "statement"
  where
    updates =  Repeatable [] <$ try (reserved "true" <* semi)
           <|> try (singleUpdate <* semi)
           <|> repeatable update (`sepBy1` reservedOp "+") <* semi
    singleUpdate = fmap (Repeatable . (:[]) . One) . loc $
        Update Nothing <$> assigns

actionLabel :: Parser LActionLabel
actionLabel = option NoAction . loc . choice $
    [ ActActivate   <$  reserved "activate"
    , ActDeactivate <$  reserved "deactivate"
    , Action        <$> name
    ]

update :: Parser LUpdate
update = loc (Update <$> probability <*> assignmentList) <?> "stochastic update"
  where
    probability    = Just <$> (expr <* colon)
    assignmentList =  Repeatable [] <$ reserved "true"
                  <|> assigns

assigns :: Parser (LRepeatable Assign)
assigns = repeatable assign (`sepBy1` reservedOp "&")

assign :: Parser LAssign
assign = loc (choice
    [ Activate   <$> (reserved "activate"   *> parens name)
    , Deactivate <$> (reserved "deactivate" *> parens name)
    , parens $ Assign <$> name <* symbol "'" <* reservedOp "=" <*> expr
    ]) <?> "assignment"

-- | Creates the expression parser. If @allowPctl@ is 'True' the parser
-- will accept temporal and probabilistic operators.
expr :: Parser LExpr
expr = simpleExpr >>= condExpr
  where
    simpleExpr =
        normalizeExpr <$> buildExpressionParser exprOpTable term
                      <?> "expression"
    condExpr e = option e $ CondExpr e <$> (reservedOp "?" *> expr)
                                       <*> (colon *> expr)
                                       <*> pure (exprAnnot e)

term :: Parser LExpr
term = atom >>= callExpr
  where
    callExpr e = option e $ CallExpr e <$> args expr
                                       <*> pure (exprAnnot e)

atom :: Parser LExpr
atom = parens expr
   <|> loc (choice
        [ MissingExpr<$  reservedOp "..."
        , idExpr     <$  reserved "id"
        , BoolExpr   <$> bool
        , DecimalExpr<$> try float
        , IntegerExpr<$> integer
        , LoopExpr   <$> forLoop expr
        , arrayExpr
        , sampleExpr
        , FuncExpr   <$> function
        , NameExpr   <$> name
        ])
   <?> "literal, variable or expression"
  where
    idExpr l = NameExpr (_Ident # ("id", l)) l

repeatable :: Parser (b SrcLoc)
           -> (Parser (Some b SrcLoc) -> Parser [Some b SrcLoc])
           -> Parser (LRepeatable b)
repeatable p c = Repeatable <$> c some'
  where
    some' =  Many <$> forLoop (repeatable p c)
         <|> One  <$> p

forLoop :: Parser (b SrcLoc) -> Parser (LForLoop b)
forLoop p = loc (ForLoop <$> (reserved "for" *> identifier')
                         <*> (reserved "in"  *> range)
                         <*> block "for" p)
         <?> "for loop"

sampleExpr :: Parser (SrcLoc -> LExpr)
sampleExpr = SampleExpr <$> (reserved "sample" *> parens (commaSep sampleArg))
  where
    sampleArg =
        (ArgString <$> doubleQuotes identifier') <|>
        (ArgExpr <$> expr)

arrayExpr :: Parser (SrcLoc -> LExpr)
arrayExpr = ArrayExpr . fromList <$> braces (commaSep1 expr)

function :: Parser Function
function = choice
    [ "min"     --> FuncMin
    , "max"     --> FuncMax
    , "floor"   --> FuncFloor
    , "ceil"    --> FuncCeil
    , "round"   --> FuncRound
    , "pow"     --> FuncPow
    , "mod"     --> FuncMod
    , "log"     --> FuncLog
    , "active"  --> FuncActive
    , "iactive" --> FuncIActive
    , "binom"   --> FuncBinom
    ] <?> "function call"
  where
    s --> func = func <$ reserved s

name :: Parser LName
name =  loc (toName <$> (this <|> qualifier)
                    <*> many (reservedOp "." *> qualifier))
    <?> "name"
  where
    toName q qs = Name (q :| qs)
    this        = ("this", Nothing) <$ reserved "this"
    qualifier   = (,) <$> identifier' <*> optionMaybe (brackets expr)

args :: Parser LExpr -> Parser [LExpr]
args = parens . commaSep

params :: Parser [Ident]
params = option [] . parens $ commaSep1 identifier'

range :: Parser LRange
range = brackets ((,) <$> expr <*> (reservedOp ".." *> expr))

exprOpTable :: OperatorTable Text UserState Identity LExpr
exprOpTable = -- operators listed in descending precedence, operators in same group have the same precedence
    [ [ unaryOp "-" $ ArithUnOp Neg
      ]
    , [ "*"   --> ArithBinOp Mul
      , "/"   --> ArithBinOp Div
      ]
    , [ "+"   --> ArithBinOp Add
      , "-"   --> ArithBinOp Sub
      ]
    , [ "="   --> EqBinOp Eq
      , "!="  --> EqBinOp Neq
      , ">"   --> RelBinOp Gt
      , "<"   --> RelBinOp Lt
      , ">="  --> RelBinOp Gte
      , "<="  --> RelBinOp Lte
      ]
    , [ unaryOp "!" $ LogicUnOp LNot
      ]
    , [ "&"   --> LogicBinOp LAnd
      ]
    , [ "|"   --> LogicBinOp LOr
      ]
    , [ "=>"  --> LogicBinOp LImpl
      , "<=>" --> LogicBinOp LEq
      ]
    ]
  where
    unaryOp s     = unary $ reservedOp s
    (-->) s = binary AssocLeft $ reservedOp s

unary :: ParsecT s u m a -> UnOp -> Operator s u m (Expr b)
unary p unOp = Prefix (unaryExpr unOp <$ p)

binary :: Assoc -> ParsecT s u m a -> BinOp -> Operator s u m (Expr b)
binary assoc p binOp = Infix (binaryExpr binOp <$ p) assoc

manyTill'
    :: Stream s m t
    => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill' p end = scan
  where
    scan = end' <|> p'
    end' = do
        e <- end
        return ([], e)
    p' = do
        x <- p
        (xs, e) <- scan
        return (x:xs, e)
