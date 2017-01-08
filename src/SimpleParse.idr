module SimpleParse

export
data Parser a = P (List Char -> (List (a, List Char)))

bind' : Parser a -> (a -> Parser b) -> Parser b
bind' (P ma) f = P ( \ s => do
  (a, s') <- ma s
  let (P mb) = f a
  mb s')

pure' : a -> Parser a
pure' a = P (\s => [(a, s)])

void : Monad m => m a -> m ()
void m = m >>= (\_ => pure ())

Functor Parser where
  map f m = bind' m (\a => pure' (f a))

Applicative Parser where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad Parser where
  (>>=) = bind'

get : Parser (List Char)
get = P (\ cs => [(cs, cs)])

getc : Parser Char
getc = P getc'
  where getc' : List Char -> (List (Char, List Char))
        getc' [] = []
        getc' (x::xs) = [(x, xs)]

export
reject : Parser a
reject = P (\ _ => [])

export
char : Char -> Parser Char
char c = do
  c' <- getc
  if c == c' then pure c else reject

export
string : String -> Parser String
string s = map pack (string' (unpack s))
  where string' : (List Char) -> Parser (List Char)
        string' [] = pure []
        string' (c::cs) = do
          char c
          string' cs
          pure (c::cs)

export
chars : List Char -> Parser Char
chars cs = do
  c <- getc
  if c `elem` cs then pure c else reject

export
wild : Parser ()
wild = void getc

export
eof : Parser ()
eof = do
  cs <- get
  case cs of
    []  => pure ()
    _   => reject

export
nonempty : Parser (List a) -> Parser (List a)
nonempty p = do
  as <- p
  case as of
    [] => reject
    _ => pure as

export
satisfy : (Char -> Bool) -> Parser Char
satisfy p = do
  c <- getc
  if p c then pure c else reject

export
parse : Parser a -> String -> (List (a, String))
parse (P p) s =
  let rs = p (unpack s)
      as = map fst rs
      ss = map pack (map snd rs)
  in zip as ss

export
fullParse : Parser a -> String -> List a
fullParse p s = map fst $ parse (p <* eof) s

Alternative Parser where
  empty = reject
  (P p) <|> (P q) = P $ \cs => p cs <|> q cs

mutual
  export
  some : Alternative f => f a -> f (List a)
  some p = (map (::) p) <*> many p

  export
  many : Alternative f => f a -> f (List a)
  many p = some p <|> pure []

export
option : a -> Parser a -> Parser a
option v p = p <|> pure v

export
choice : List (Parser a) -> Parser a
choice [] = reject
choice (p::ps) = p <|> choice ps

export
between : Parser open -> Parser close
           -> Parser a -> Parser a
between open close p = (void $ open) *> p <* (void $ close)
