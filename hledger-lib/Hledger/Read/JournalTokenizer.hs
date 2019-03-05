{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Hledger.Read.JournalTokenizer where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor (($>))
import Data.Decimal (DecimalRaw (Decimal), Decimal)
import Data.List (intercalate, intersperse)
import Safe (lastMay)
import Data.String (fromString)
import Data.Maybe (isJust)

import qualified Hledger.Data as H
import Hledger.Data hiding (Journal, transaction)
import Text.Megaparsec.Custom
import qualified Hledger.Utils.Parse as P
import Hledger.Utils.Parse
import qualified Hledger.Utils.String as S
import qualified Hledger.Read.Common as C
import Hledger.Read.Common hiding (amountp, balanceassertionp, fixedlotpricep, priceamountp)
import qualified Hledger.Read.JournalReader as R

import Hledger.Utils.Debug
import Debug.Trace

type Parser a = Parsec CustomErr Text a

data Journal = Journal [Item]
data Item
  = DirectiveItem Directive
  | TransactionItem H.Transaction
  | TransactionModifierItem H.TransactionModifier
  | PeriodicTransactionItem H.PeriodicTransaction
  | MarketPriceDirectiveItem H.MarketPrice
  | CommentItem Text
data Directive
  = IncludeDirective FilePath
  | AliasDirective H.AccountAlias
  | EndAliasesDirective
  | AccountDirective H.AccountName (Maybe H.AccountType) [H.Tag]
  | ApplyAccountDirective H.AccountName
  | CommodityDirective H.Commodity
  | EndApplyAccountDirective
  | TagDirective
  | EndTagDirective
  | DefaultYearDirective H.Year
  | DefaultCommodityDirective H.CommoditySymbol H.AmountStyle
  | CommodityConversionDirective H.Amount H.Amount
  | IgnoredPriceCommodityDirective H.CommoditySymbol
  deriving (Show)


--- * from JournalReader.hs

-- | A journal parser. Accumulates and returns a "ParsedJournal",
-- which should be finalised/validated before use.
--
-- >>> rejp (journalp <* eof) "2015/1/1\n a  0\n"
-- Right (Right Journal  with 1 transactions, 1 accounts)
--
journalp :: Parser Journal
journalp = Journal <$> many addJournalItemP

-- | A side-effecting parser; parses any kind of journal item
-- and updates the parse state accordingly.
addJournalItemP :: Parser Item
addJournalItemP = do
  -- all journal line types can be distinguished by the first
  -- character, can use choice without backtracking
  i <- choice [
      DirectiveItem            <$> directivep
    , TransactionItem          <$> transactionp
    , TransactionModifierItem  <$> transactionmodifierp
    , PeriodicTransactionItem  <$> periodictransactionp
    , MarketPriceDirectiveItem <$> marketpricedirectivep
    ] <?> "transaction or directive"
  many $ choice
    [ void (emptyorcommentlinep)
    , void (multilinecommentp)
    ]
  return i

--- ** directives

-- | Parse any journal directive and update the parse state accordingly.
-- Cf http://hledger.org/manual.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: Parser Directive
directivep = (do
  optional $ char '!'
  choice [
    includedirectivep
   ,aliasdirectivep
   ,endaliasesdirectivep
   ,accountdirectivep
   ,applyaccountdirectivep
   ,commoditydirectivep
   ,endapplyaccountdirectivep
   ,tagdirectivep
   ,endtagdirectivep
   ,defaultyeardirectivep
   ,defaultcommoditydirectivep
   ,commodityconversiondirectivep
   ,ignoredpricecommoditydirectivep
   ]
  ) <?> "directive"

includedirectivep :: Parser Directive
includedirectivep = do
  string "include"
  (skipSome P.spacenonewline)
  filename <- T.unpack <$> takeWhileP Nothing (/= '\n') -- don't consume newline yet

  void newline
  return $ IncludeDirective filename

-- Parse an account directive, adding its info to the journal's
-- list of account declarations.
accountdirectivep :: Parser Directive
accountdirectivep = do
  off <- getOffset -- XXX figure out a more precise position later

  string "account"
  (skipSome P.spacenonewline)

  -- the account name, UNMODIFIED by preceding alias or apply account directives
  acct <- accountnamep

  -- maybe an account type code (ALERX) after two or more spaces
  -- XXX added in 1.11, deprecated in 1.13, remove in 1.14
  mtypecode <- optional $ try $ do
    skipSome P.spacenonewline -- at least one more space in addition to the one consumed by modifiedaccountp
    choice $ map char "ALERX"

  -- maybe a comment, on this and/or following lines
  (cmt, tags) <- transactioncommentp

  -- maybe Ledger-style subdirectives (ignored)
  skipMany indentedlinep

  -- an account type may have been set by account type code or a tag;
  -- the latter takes precedence
  let
    mtypecode' = maybe
      (T.singleton <$> mtypecode)
      Just
      $ lookup accountTypeTagName tags
    metype = parseAccountTypeCode <$> mtypecode'

  metype' <- case metype of
    Nothing         -> return Nothing
    Just (Right t)  -> return $ Just t
    Just (Left err) -> customFailure $ parseErrorAt off err
  return $ AccountDirective acct metype' tags
-- The special tag used for declaring account type. XXX change to "class" ?
accountTypeTagName = "type"

parseAccountTypeCode :: Text -> Either String H.AccountType
parseAccountTypeCode s =
  case T.toLower s of
    "asset"     -> Right Asset
    "a"         -> Right Asset
    "liability" -> Right Liability
    "l"         -> Right Liability
    "equity"    -> Right Equity
    "e"         -> Right Equity
    "revenue"   -> Right Revenue
    "r"         -> Right Revenue
    "expense"   -> Right Expense
    "x"         -> Right Expense
    _           -> Left err
  where
    err = "invalid account type code "++T.unpack s++", should be one of " ++
          (intercalate ", " $ ["A","L","E","R","X","ASSET","LIABILITY","EQUITY","REVENUE","EXPENSE"])

indentedlinep :: Parser String
indentedlinep = (skipSome P.spacenonewline) >> (S.rstrip <$> restofline)

-- | Parse a one-line or multi-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n  format $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n\n" -- a commodity with no format
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00\n  format $1.00" -- both, what happens ?
commoditydirectivep :: Parser Directive
commoditydirectivep = commoditydirectiveonelinep <|> commoditydirectivemultilinep

-- | Parse a one-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00 ; blah\n"
commoditydirectiveonelinep :: Parser Directive
commoditydirectiveonelinep = do
  (off, H.Amount{H.acommodity,H.astyle}) <- try $ do
    string "commodity"
    (skipSome P.spacenonewline)
    off <- getOffset
    amount <- amountp
    pure $ (off, amount)
  (skipMany P.spacenonewline)
  _ <- followingcommentp
  let comm = H.Commodity{H.csymbol=acommodity, H.cformat=Just $ dbg2 "style from commodity directive" astyle}
  undefined
{-
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else modify' (\j -> j{jcommodities=M.insert acommodity comm $ jcommodities j})
-}

pleaseincludedecimalpoint :: String
pleaseincludedecimalpoint = "to avoid ambiguity, please include a decimal separator in commodity directives"

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rjp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: Parser Directive
commoditydirectivemultilinep = do
  string "commodity"
  (skipSome P.spacenonewline)
  sym <- commoditysymbolp
  _ <- followingcommentp
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = H.Commodity{H.csymbol=sym, H.cformat=mformat}
  undefined
{-
  modify' (\j -> j{jcommodities=M.insert sym comm $ jcommodities j})
-}
  where
    indented = ((skipSome P.spacenonewline) >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: H.CommoditySymbol -> Parser H.AmountStyle
formatdirectivep expectedsym = do
  string "format"
  (skipSome P.spacenonewline)
  off <- getOffset
  H.Amount{H.acommodity,H.astyle} <- amountp
  _ <- followingcommentp
  undefined
{-
  if acommodity==expectedsym
    then
      if asdecimalpoint astyle == Nothing
      then customFailure $ parseErrorAt off pleaseincludedecimalpoint
      else return $ dbg2 "style from format subdirective" astyle
    else customFailure $ parseErrorAt off $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity
-}

keywordp :: String -> Parser ()
keywordp = (() <$) . string . fromString

spacesp :: Parser ()
spacesp = () <$ (skipSome P.spacenonewline)

-- | Backtracking parser similar to string, but allows varying amount of space between words
keywordsp :: String -> Parser ()
keywordsp = try . sequence_ . intersperse spacesp . map keywordp . words

applyaccountdirectivep :: Parser Directive
applyaccountdirectivep = do
  keywordsp "apply account" <?> "apply account directive"
  (skipSome P.spacenonewline)
  parent <- accountnamep
  newline
  return $ ApplyAccountDirective parent

endapplyaccountdirectivep :: Parser Directive
endapplyaccountdirectivep = do
  keywordsp "end apply account" <?> "end apply account directive"
  return $ EndApplyAccountDirective

aliasdirectivep :: Parser Directive
aliasdirectivep = do
  string "alias"
  (skipSome P.spacenonewline)
  alias <- accountaliasp
  return $ AliasDirective alias

accountaliasp :: Parser H.AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: Parser H.AccountAlias
basicaliasp = do
  -- dbgparse 0 "basicaliasp"
  old <- S.rstrip <$> (some $ noneOf ("=" :: [Char]))
  char '='
  skipMany P.spacenonewline
  new <- S.rstrip <$> anySingle `manyTill` eolof  -- eol in journal, eof in command lines, normally
  return $ BasicAlias (T.pack old) (T.pack new)

regexaliasp :: Parser H.AccountAlias
regexaliasp = do
  -- dbgparse 0 "regexaliasp"
  char '/'
  re <- some $ noneOf ("/\n\r" :: [Char]) -- paranoid: don't try to read past line end
  char '/'
  skipMany P.spacenonewline
  char '='
  skipMany P.spacenonewline
  repl <- anySingle `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirectivep :: Parser Directive
endaliasesdirectivep = do
  keywordsp "end aliases" <?> "end aliases directive"
  return $ EndAliasesDirective

tagdirectivep :: Parser Directive
tagdirectivep = do
  string "tag" <?> "tag directive"
  (skipSome P.spacenonewline)
  _ <- some nonspace
  restofline
  return $ TagDirective

endtagdirectivep :: Parser Directive
endtagdirectivep = do
  (keywordsp "end tag" <|> keywordp "pop") <?> "end tag or pop directive"
  restofline
  return $ EndTagDirective

defaultyeardirectivep :: Parser Directive
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  (skipMany P.spacenonewline)
  y <- some digitChar
  let y' = read y
  failIfInvalidYear y
  return $ DefaultYearDirective y'

defaultcommoditydirectivep :: Parser Directive
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  (skipSome P.spacenonewline)
  off <- getOffset
  H.Amount{H.acommodity,H.astyle} <- amountp
  restofline
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else return $ DefaultCommodityDirective acommodity astyle

marketpricedirectivep :: Parser H.MarketPrice
marketpricedirectivep = do
  char 'P' <?> "market price"
  (skipMany P.spacenonewline)
  undefined
  date <- undefined -- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  (skipSome P.spacenonewline)
  symbol <- commoditysymbolp
  (skipMany P.spacenonewline)
  price <- amountp
  restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirectivep :: Parser Directive
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  (skipSome P.spacenonewline)
  c <- commoditysymbolp
  restofline
  return $ IgnoredPriceCommodityDirective c

commodityconversiondirectivep :: Parser Directive
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  (skipSome P.spacenonewline)
  a1 <- amountp
  (skipMany P.spacenonewline)
  char '='
  (skipMany P.spacenonewline)
  a2 <- amountp
  restofline
  return $ CommodityConversionDirective a1 a2

--- ** transactions

transactionmodifierp :: Parser H.TransactionModifier
transactionmodifierp = do
  char '=' <?> "modifier transaction"
  (skipMany P.spacenonewline)
  querytxt <- T.strip <$> descriptionp
  (_comment, _tags) <- transactioncommentp   -- TODO apply these to modified txns ?
  postings <- postingsp Nothing
  return $ H.TransactionModifier querytxt postings

-- | Parse a periodic transaction
--
-- This reuses periodexprp which parses period expressions on the command line.
-- This is awkward because periodexprp supports relative and partial dates,
-- which we don't really need here, and it doesn't support the notion of a
-- default year set by a Y directive, which we do need to consider here.
-- We resolve it as follows: in periodic transactions' period expressions,
-- if there is a default year Y in effect, partial/relative dates are calculated
-- relative to Y/1/1. If not, they are calculated related to today as usual.
periodictransactionp :: Parser H.PeriodicTransaction
periodictransactionp = do

  -- first line
  char '~' <?> "periodic transaction"
  skipMany P.spacenonewline
  -- a period expression
  off <- getOffset

  periodExcerpt <- excerpt_ $
                    singlespacedtextsatisfyingp (\c -> c /= ';' && c /= '\n')
  let periodtxt = T.strip $ getExcerptText periodExcerpt

  status <- statusp <?> "cleared status"
  code <- codep <?> "transaction code"
  description <- T.strip <$> descriptionp
  (comment, tags) <- transactioncommentp
  -- next lines; use same year determined above
  postings <- postingsp undefined -- TODO this normally gets a year

  return $ nullperiodictransaction{
     H.ptperiodexpr=periodtxt
    ,H.ptinterval=undefined
    ,H.ptspan=undefined
    ,H.ptstatus=status
    ,H.ptcode=code
    ,H.ptdescription=description
    ,H.ptcomment=comment
    ,H.pttags=tags
    ,H.ptpostings=postings
    }

-- | Parse a (possibly unbalanced) transaction.
transactionp :: Parser H.Transaction
transactionp = do
  -- dbgparse 0 "transactionp"
  startpos <- getSourcePos
  date <- undefined -- datep <?> "transaction"
  edate <- optional (secondarydatep date) <?> "secondary date"
  lookAhead (P.spacenonewline <|> newline) <?> "whitespace or newline"
  status <- statusp <?> "cleared status"
  code <- codep <?> "transaction code"
  description <- T.strip <$> descriptionp
  (comment, tags) <- transactioncommentp
  let year = first3 $ toGregorian date
  postings <- postingsp (Just year)
  endpos <- getSourcePos
  let sourcepos = journalSourcePos startpos endpos
  return $ txnTieKnot $ H.Transaction 0 "" sourcepos date edate status code description comment tags postings

--- ** postings

-- Parse the following whitespace-beginning lines as postings, posting
-- tags, and/or comments (inferring year, if needed, from the given date).
postingsp :: Maybe H.Year -> Parser [H.Posting]
postingsp mTransactionYear = many (postingp mTransactionYear) <?> "postings"

-- linebeginningwithspaces :: Parser String
-- linebeginningwithspaces = do
--   sp <- (skipSome P.spacenonewline)
--   c <- nonspace
--   cs <- restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe H.Year -> Parser H.Posting
postingp mTransactionYear = do
  -- $ dbgparse 0 "postingp"
  (status, account) <- try $ do
    (skipSome P.spacenonewline)
    status <- statusp
    (skipMany P.spacenonewline)
    account <- accountnamep -- TODO this is changed
    return (status, account)
  let (ptype, account') = (accountNamePostingType account, textUnbracket account)
  (skipMany P.spacenonewline)
  amount <- option missingmixedamt $ Mixed . (:[]) <$> amountp
  (skipMany P.spacenonewline)
  massertion <- optional $ balanceassertionp
  _ <- fixedlotpricep
  (skipMany P.spacenonewline)
  (comment,tags,mdate,mdate2) <- postingcommentp mTransactionYear
  return H.posting
   { H.pdate=mdate
   , H.pdate2=mdate2
   , H.pstatus=status
   , H.paccount=account'
   , H.pamount=amount
   , H.pcomment=comment
   , H.ptype=ptype
   , H.ptags=tags
   , H.pbalanceassertion=massertion
   }

--- * from Common.hs

-- | Parse a single-commodity amount, with optional symbol on the left or
-- right, optional unit or total price, and optional (ignored)
-- ledger-style balance assertion or fixed lot price declaration.
amountp :: Parser H.Amount
amountp = label "amount" $ do
  amount <- amountwithoutpricep
  skipMany spacenonewline
  price <- priceamountp
  pure $ amount { H.aprice = price }

amountwithoutpricep :: Parser H.Amount
amountwithoutpricep = do
  (mult, sign) <- (,) <$> multiplierp <*> signp
  leftsymbolamountp mult sign <|> rightornosymbolamountp mult sign

  where

  leftsymbolamountp :: Bool -> (Decimal -> Decimal) -> Parser H.Amount
  leftsymbolamountp mult sign = label "amount" $ do
    c <- commoditysymbolp
    suggestedStyle <- undefined -- getAmountStyle c
    commodityspaced <- skipMany' spacenonewline
    sign2 <- signp
    offBeforeNum <- getOffset
    ambiguousRawNum <- rawnumberp
    mExponent <- optional $ try exponentp
    offAfterNum <- getOffset
    let numRegion = (offBeforeNum, offAfterNum)
    (q,prec,mdec,mgrps) <- interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
    let s = amountstyle{H.ascommodityside=L, H.ascommodityspaced=commodityspaced, H.asprecision=prec, H.asdecimalpoint=mdec, H.asdigitgroups=mgrps}
    return $ nullamt{H.acommodity=c, H.aquantity=sign (sign2 q), H.aismultiplier=mult, H.astyle=s, H.aprice=NoPrice}

  rightornosymbolamountp :: Bool -> (Decimal -> Decimal) -> Parser H.Amount
  rightornosymbolamountp mult sign = label "amount" $ do
    offBeforeNum <- getOffset
    ambiguousRawNum <- rawnumberp
    mExponent <- optional $ try exponentp
    offAfterNum <- getOffset
    let numRegion = (offBeforeNum, offAfterNum)
    mSpaceAndCommodity <- optional $ try $ (,) <$> skipMany' spacenonewline <*> commoditysymbolp
    case mSpaceAndCommodity of
      -- right symbol amount
      Just (commodityspaced, c) -> do
        suggestedStyle <- undefined -- getAmountStyle c
        (q,prec,mdec,mgrps) <- interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
        let s = amountstyle{H.ascommodityside=R, H.ascommodityspaced=commodityspaced, H.asprecision=prec, H.asdecimalpoint=mdec, H.asdigitgroups=mgrps}
        return $ nullamt{H.acommodity=c, aquantity=sign q, aismultiplier=mult, astyle=s, aprice=NoPrice}
      -- no symbol amount
      Nothing -> do
        suggestedStyle <- undefined -- getDefaultAmountStyle
        (q,prec,mdec,mgrps) <- interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
        -- if a default commodity has been set, apply it and its style to this amount
        -- (unless it's a multiplier in an automated posting)
        defcs <- undefined -- getDefaultCommodityAndStyle
        let (c,s) = case (mult, defcs) of
              (False, Just (defc,defs)) -> (defc, defs{asprecision=max (asprecision defs) prec})
              _ -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
        return $ nullamt{H.acommodity=c, aquantity=sign q, aismultiplier=mult, astyle=s, aprice=NoPrice}

  -- For reducing code duplication. Doesn't parse anything. Has the type
  -- of a parser only in order to throw parse errors (for convenience).
  interpretNumber
    :: (Int, Int) -- offsets
    -> Maybe H.AmountStyle
    -> Either AmbiguousNumber RawNumber
    -> Maybe Int
    -> Parser (H.Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
  interpretNumber posRegion suggestedStyle ambiguousNum mExp =
    let rawNum = either (disambiguateNumber suggestedStyle) id ambiguousNum
    in  case fromRawNumber rawNum mExp of
          Left errMsg -> customFailure $
                           uncurry parseErrorAtRegion posRegion errMsg
          Right res -> pure res

balanceassertionp :: Parser BalanceAssertion
balanceassertionp = do
  sourcepos <- genericSourcePos <$> getSourcePos
  char '='
  istotal <- fmap isJust $ optional $ try $ char '='
  isinclusive <- fmap isJust $ optional $ try $ char '*'
  skipMany spacenonewline
  -- this amount can have a price; balance assertions ignore it,
  -- but balance assignments will use it
  a <- amountp <?> "amount (for a balance assertion or assignment)"
  return BalanceAssertion
    { baamount    = a
    , batotal     = istotal
    , bainclusive = isinclusive
    , baposition  = sourcepos
    }

fixedlotpricep :: Parser (Maybe Amount)
fixedlotpricep = optional $ do
  try $ do
    skipMany spacenonewline
    char '{'
  skipMany spacenonewline
  char '='
  skipMany spacenonewline
  a <- amountwithoutpricep <?> "unpriced amount (for an ignored ledger-style fixed lot price)"
  skipMany spacenonewline
  char '}'
  return a

priceamountp :: Parser Price
priceamountp = option NoPrice $ do
  char '@'
  priceConstructor <- char '@' *> pure TotalPrice <|> pure UnitPrice

  skipMany spacenonewline
  priceAmount <- amountwithoutpricep <?> "unpriced amount (specifying a price)"

  pure $ priceConstructor priceAmount


--- * tests

{-
tests_JournalReader = tests "JournalReader" [

   let p = accountnamep :: JournalParser IO AccountName in
   tests "accountnamep" [
     test "basic" $ expectParse p "a:b:c"
    ,_test "empty inner component" $ expectParseError p "a::c" ""  -- TODO
    ,_test "empty leading component" $ expectParseError p ":b:c" "x"
    ,_test "empty trailing component" $ expectParseError p "a:b:" "x"
    ]

  -- "Parse a date in YYYY/MM/DD format.
  -- Hyphen (-) and period (.) are also allowed as separators.
  -- The year may be omitted if a default year has been set.
  -- Leading zeroes may be omitted."
  ,test "datep" $ do
    test "YYYY/MM/DD" $ expectParseEq datep "2018/01/01" (fromGregorian 2018 1 1)
    test "YYYY-MM-DD" $ expectParse datep "2018-01-01"
    test "YYYY.MM.DD" $ expectParse datep "2018.01.01"
    test "yearless date with no default year" $ expectParseError datep "1/1" "current year is unknown"
    test "yearless date with default year" $ do
      let s = "1/1"
      ep <- parseWithState mempty{jparsedefaultyear=Just 2018} datep s
      either (fail.("parse error at "++).customErrorBundlePretty) (const ok) ep
    test "no leading zero" $ expectParse datep "2018/1/1"

  ,test "datetimep" $ do
      let
        good = expectParse datetimep
        bad = (\t -> expectParseError datetimep t "")
      good "2011/1/1 00:00"
      good "2011/1/1 23:59:59"
      bad "2011/1/1"
      bad "2011/1/1 24:00:00"
      bad "2011/1/1 00:60:00"
      bad "2011/1/1 00:00:60"
      bad "2011/1/1 3:5:7"
      test "timezone is parsed but ignored" $ do
        let t = LocalTime (fromGregorian 2018 1 1) (TimeOfDay 0 0 (fromIntegral 0))
        expectParseEq datetimep "2018/1/1 00:00-0800" t
        expectParseEq datetimep "2018/1/1 00:00+1234" t

  ,tests "periodictransactionp" [

    test "more period text in comment after one space" $ expectParseEq periodictransactionp
      "~ monthly from 2018/6 ;In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = ""
        ,ptcomment     = "In 2019 we will change this\n"
        }

    ,test "more period text in description after two spaces" $ expectParseEq periodictransactionp
      "~ monthly from 2018/6   In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = "In 2019 we will change this"
        ,ptcomment     = ""
        }

    ,test "Next year in description" $ expectParseEq periodictransactionp
      "~ monthly  Next year blah blah\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan Nothing Nothing
        ,ptdescription = "Next year blah blah"
        ,ptcomment     = ""
        }

    ,test "Just date, no description" $ expectParseEq periodictransactionp
      "~ 2019-01-04\n"
      nullperiodictransaction {
         ptperiodexpr  = "2019-01-04"
        ,ptinterval    = NoInterval
        ,ptspan        = DateSpan (Just $ fromGregorian 2019 1 4) (Just $ fromGregorian 2019 1 5)
        ,ptdescription = ""
        ,ptcomment     = ""
        }

    ,test "Just date, no description + empty transaction comment" $ expectParse periodictransactionp
      "~ 2019-01-04\n  ;\n  a  1\n  b\n"

    ]

  ,tests "postingp" [
     test "basic" $ expectParseEq (postingp Nothing)
      "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n"
      posting{
        paccount="expenses:food:dining",
        pamount=Mixed [usd 10],
        pcomment="a: a a\nb: b b\n",
        ptags=[("a","a a"), ("b","b b")]
        }

    ,test "posting dates" $ expectParseEq (postingp Nothing)
      " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
      nullposting{
         paccount="a"
        ,pamount=Mixed [num 1]
        ,pcomment="date:2012/11/28, date2=2012/11/29,b:b\n"
        ,ptags=[("date", "2012/11/28"), ("date2=2012/11/29,b", "b")] -- TODO tag name parsed too greedily
        ,pdate=Just $ fromGregorian 2012 11 28
        ,pdate2=Nothing  -- Just $ fromGregorian 2012 11 29
        }

    ,test "posting dates bracket syntax" $ expectParseEq (postingp Nothing)
      " a  1. ; [2012/11/28=2012/11/29]\n"
      nullposting{
         paccount="a"
        ,pamount=Mixed [num 1]
        ,pcomment="[2012/11/28=2012/11/29]\n"
        ,ptags=[]
        ,pdate= Just $ fromGregorian 2012 11 28
        ,pdate2=Just $ fromGregorian 2012 11 29
        }

    ,test "quoted commodity symbol with digits" $ expectParse (postingp Nothing) "  a  1 \"DE123\"\n"

    ,test "balance assertion and fixed lot price" $ expectParse (postingp Nothing) "  a  1 \"DE123\" =$1 { =2.2 EUR} \n"

    ,test "balance assertion over entire contents of account" $ expectParse (postingp Nothing) "  a  $1 == $1\n"
    ]

  ,tests "transactionmodifierp" [

    test "basic" $ expectParseEq transactionmodifierp
      "= (some value expr)\n some:postings  1.\n"
      nulltransactionmodifier {
        tmquerytxt = "(some value expr)"
       ,tmpostingrules = [nullposting{paccount="some:postings", pamount=Mixed[num 1]}]
      }
    ]

  ,tests "transactionp" [

     test "just a date" $ expectParseEq transactionp "2015/1/1\n" nulltransaction{tdate=fromGregorian 2015 1 1}

    ,test "more complex" $ expectParseEq transactionp
      (T.unlines [
        "2012/05/14=2012/05/15 (code) desc  ; tcomment1",
        "    ; tcomment2",
        "    ; ttag1: val1",
        "    * a         $1.00  ; pcomment1",
        "    ; pcomment2",
        "    ; ptag1: val1",
        "    ; ptag2: val2"
        ])
      nulltransaction{
        tsourcepos=JournalSourcePos "" (1,7),  -- XXX why 7 here ?
        tprecedingcomment="",
        tdate=fromGregorian 2012 5 14,
        tdate2=Just $ fromGregorian 2012 5 15,
        tstatus=Unmarked,
        tcode="code",
        tdescription="desc",
        tcomment="tcomment1\ntcomment2\nttag1: val1\n",
        ttags=[("ttag1","val1")],
        tpostings=[
          nullposting{
            pdate=Nothing,
            pstatus=Cleared,
            paccount="a",
            pamount=Mixed [usd 1],
            pcomment="pcomment1\npcomment2\nptag1: val1\nptag2: val2\n",
            ptype=RegularPosting,
            ptags=[("ptag1","val1"),("ptag2","val2")],
            ptransaction=Nothing
            }
          ]
      }

    ,test "parses a well-formed transaction" $
      expect $ isRight $ rjp transactionp $ T.unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries                   $47.18"
        ,"    assets:checking                          $-47.18"
        ,""
        ]

    ,test "does not parse a following comment as part of the description" $
      expectParseEqOn transactionp "2009/1/1 a ;comment\n b 1\n" tdescription "a"

    ,test "transactionp parses a following whitespace line" $
      expect $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,test "transactionp parses an empty transaction comment following whitespace line" $
      expect $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  ;"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,test "comments everywhere, two postings parsed" $
      expectParseEqOn transactionp
        (T.unlines
          ["2009/1/1 x  ; transaction comment"
          ," a  1  ; posting 1 comment"
          ," ; posting 1 comment 2"
          ," b"
          ," ; posting 2 comment"
          ])
        (length . tpostings)
        2

    ]

  -- directives

  ,tests "directivep" [
    test "supports !" $ do
      expectParseE directivep "!account a\n"
      expectParseE directivep "!D 1.0\n"
    ]

  ,test "accountdirectivep" $ do
    test "with-comment"       $ expectParse accountdirectivep "account a:b  ; a comment\n"
    test "does-not-support-!" $ expectParseError accountdirectivep "!account a:b\n" ""
    test "account-type-code"  $ expectParse accountdirectivep "account a:b  A\n"
    test "account-type-tag"   $ expectParseStateOn accountdirectivep "account a:b  ; type:asset\n"
      jdeclaredaccounts
      [("a:b", AccountDeclarationInfo{adicomment          = "type:asset\n"
                                     ,aditags             = [("type","asset")]
                                     ,adideclarationorder = 1
                                     })
      ]

  ,test "commodityconversiondirectivep" $ do
     expectParse commodityconversiondirectivep "C 1h = $50.00\n"

  ,test "defaultcommoditydirectivep" $ do
     expectParse defaultcommoditydirectivep "D $1,000.0\n"
     expectParseError defaultcommoditydirectivep "D $1000\n" "please include a decimal separator"

  ,test "defaultyeardirectivep" $ do
    test "1000" $ expectParse defaultyeardirectivep "Y 1000" -- XXX no \n like the others
    test "999" $ expectParseError defaultyeardirectivep "Y 999" "bad year number"
    test "12345" $ expectParse defaultyeardirectivep "Y 12345"

  ,test "ignoredpricecommoditydirectivep" $ do
     expectParse ignoredpricecommoditydirectivep "N $\n"

  ,test "includedirectivep" $ do
    test "include" $ expectParseErrorE includedirectivep "include nosuchfile\n" "No existing files match pattern: nosuchfile"
    test "glob" $ expectParseErrorE includedirectivep "include nosuchfile*\n" "No existing files match pattern: nosuchfile*"

  ,test "marketpricedirectivep" $ expectParseEq marketpricedirectivep
    "P 2017/01/30 BTC $922.83\n"
    MarketPrice{
      mpdate      = fromGregorian 2017 1 30,
      mpcommodity = "BTC",
      mpamount    = usd 922.83
      }

  ,test "tagdirectivep" $ do
     expectParse tagdirectivep "tag foo \n"

  ,test "endtagdirectivep" $ do
     expectParse endtagdirectivep "end tag \n"
     expectParse endtagdirectivep "pop \n"


  ,tests "journalp" [
    test "empty file" $ expectParseEqE journalp "" nulljournal
    ]

   -- these are defined here rather than in Common so they can use journalp
  ,tests "parseAndFinaliseJournal" [
    test "basic" $ do
        ej <- io $ runExceptT $ parseAndFinaliseJournal journalp definputopts "" "2019-1-1\n"
        let Right j = ej
        expectEqPP [""] $ journalFilePaths j
   ]

  ]
-}
