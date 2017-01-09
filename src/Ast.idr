module Ast

export
record Header where
  constructor MkHeader
  title : String
  points, maxPoints : Double

export
Show Header where
  show (MkHeader title points maxPoints) =
    "MkHeader " ++ show title ++ " " ++ show points ++ " " ++ show maxPoints

public export
data Mood = Positive | Negative | Neutral | Impartial

export
Show Mood where
  show Positive = "Positive"
  show Negative = "Negative"
  show Neutral = "Neutral"
  show Impartial = "Impartial"

export
record Comment where
  constructor MkComment
  mood : Mood
  comment : List String
  subcs : List Comment

export
Show Comment where
  show (MkComment mood comment subcs) =
    "MkComment " ++ show mood ++ " " ++ show comment ++ " " ++ show subcs

export
data Judgement = JM Header (List Comment) (List Judgement)
