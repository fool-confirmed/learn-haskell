{-
  Algebraic data types: various constructors and arguments
  Pattern matching is about taking apart a value by finding out which constructor it was build with, then decide what to do.
-}

data Ticket = TrainTicket String Double
  | BusTicket Double
  | MovieTicket String Int Int Double
  deriving Show

ticket1, ticket2, ticket3 :: Ticket
ticket1 = TrainTicket "City A to City B" 123.45
ticket2 = BusTicket 0.99
ticket3 = MovieTicket "Movie Title" 10 10 2.99

-- recursive data type
data Tree = Node Tree Int Tree
  | Leaf Char
  deriving Show
tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
