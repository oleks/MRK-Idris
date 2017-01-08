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

Functor Parser where
  map f m = bind' m (\a => pure' (f a))

Applicative Parser where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad Parser where
  (>>=) = bind'

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
parse : Parser a -> String -> (List (a, String))
parse (P p) s =
  let rs = p (unpack s)
      as = map fst rs
      ss = map pack (map snd rs)
  in zip as ss
