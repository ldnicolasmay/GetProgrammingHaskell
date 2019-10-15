-- lesson16.hs

data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving (Show)
data BreakfastMeat = Sausage | Bacon | Hame deriving (Show)
data BreakfastMain = Egg | Pancacke | Waffle deriving (Show)

data BreakfastSpecial =
  KidsBreakfast BreakfastMain BreakfastSide |
  BasicBreakfast BreakfastMain BreakfastMeat BreakfastSide |
  Lumberjack
    BreakfastMain BreakfastMain
    BreakfastMeat BreakfastMeat
    BreakfastSide BreakfastSide BreakfastSide
  deriving (Show)


-- 16.1 Product Types - Combining Types with "And"

-- -- Not using record syntax:
-- data AuthorName = AuthorName String String
-- data Book = Book Author String String Int Double

-- Using record syntax:
data AuthorName = AuthorName { firstName :: String
                             , lastName  :: String
                             }

-- data Book = Book { author :: AuthorName
--                  , isbn   :: String
--                  , title  :: String
--                  , year   :: Int
--                  , price  :: Double
--                  }

-- data SportsCar = SportsCar Car Spoiler


-- 16.2 Sum Types - Combining Types with "Or"

-- data Bool = False | True  -- <= is an example

type FirstName = String
type LastName = String
type MiddleName = String

data Name = NameFL   FirstName LastName
          | NameFML  FirstName MiddleName LastName
          | NameFFML FirstName FirstName MiddleName LastName
          | NameFMML FirstName MiddleName MiddleName LastName
          | NameFMLL FirstName MiddleName LastName LastName
          | NameIIL  Char Char LastName -- H.P. Lovecraft, D.H. Lawrence
          | NameFII  FirstName Char Char
          deriving (Show)

data Creator = AuthorCreator Author
             | ArtistCreator Artist
             deriving (Show)
-- data Author = Author Name -- hlinter complains... says use `newtype`
-- newtype Author = Author { authorName :: Name } deriving (Show)
newtype Author = Author Name deriving (Show)
data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (NameIIL 'H' 'P' "Lovecraft"))

andrewWK :: Creator
andrewWK = ArtistCreator (Person (NameFII "Andrew" 'W' 'K'))


-- 16.3

data Book = Book { bookAuthor :: Creator
                 , bookIsbn   :: String
                 , bookTitle  :: String
                 , bookYear   :: Int
                 , bookPrice  :: Double
                 } deriving (Show)

myBook :: Book
myBook = Book (AuthorCreator (Author (NameFL "Aldous" "Huxley")))
              "1234567890"
              "The Doors of Perception"
              1954
              12.95

-- Free accessor methods thanks to record syntax for defining classes
-- λ> bookAuthor myBook
-- AuthorCreator (Author (NameFL "Aldous" "Huxley"))
-- λ> bookIsbn myBook
-- "1234567890"

data VinylRecord = VinylRecord { recordArtist :: Creator
                               , recordTitle  :: String
                               , recordYear   :: Int
                               , recordPrice  :: Double
                               } deriving (Show)

myRecord = VinylRecord (ArtistCreator (Person (NameFL "David" "Bowie")))
                       "Ziggy Stardust"
                       1972
                       13.94

-- λ> recordArtist myRecord 
-- ArtistCreator (Person (NameFL "David" "Bowie"))
-- λ> recordTitle myRecord 
-- "Ziggy Stardust"  

-- data StoreItem = BookItem Book | RecordItem VinylRecord deriving (Show)

-- forgotten collectible toy
data CollectibleToy = CollectibleToy { toyName     :: String
                                     , description :: String
                                     , toyPrice    :: Double
                                     } deriving (Show)

-- now fix StoreItem type
data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet
               deriving (Show)

-- myBook2 :: StoreItem
myBook2 = BookItem (Book (AuthorCreator (Author (NameFL "Aldous" "Huxley")))
                         "1234567891"
                         "Brave New World"
                         1931
                         14.93)

myRecord2 = 
  RecordItem (VinylRecord (ArtistCreator (Person (NameFL "David" "Bowie")))
             "Diamond Dogs"
             1974
             15.92)

getPrice :: StoreItem -> Double
getPrice (BookItem     b) = bookPrice b
getPrice (RecordItem   r) = recordPrice r
getPrice (ToyItem      t) = toyPrice t
getPrice (PamphletItem p) = 0.00

madeBy :: StoreItem -> String
madeBy (BookItem book )    = show (bookAuthor book)
madeBy (RecordItem record) = show (recordArtist record)
madeBy _                   = "unknown"



-- Q16.1 

data Pamphlet = Pamphlet { pamphletTitle :: String
                         , pamphletDescr :: String
                         , contact       :: String
                         } deriving (Show)

              
-- Q16.2

data Shape = CircleShape Circle -- { radius :: Double }
           | SquareShape Square -- { side :: Double }
           | RectangleShape Rectangle -- { rectWidth :: Double, rectLength :: Double }
           deriving (Show)

newtype Circle = Circle { radius :: Double } deriving (Show)

newtype Square = Square { side :: Double } deriving (Show)

data Rectangle = Rectangle { rectWidth :: Double
                           , rectLength :: Double 
                           } deriving (Show)

myCircle :: Shape
myCircle = CircleShape (Circle 1.0)

mySquare :: Shape
mySquare = SquareShape (Square 2.0)

myRectangle :: Shape
myRectangle = RectangleShape (Rectangle 2.0 3.0)

getPerimeter :: Shape -> Double
getPerimeter (CircleShape c) = 2 * pi * radius c
getPerimeter (SquareShape s) = 4 * side s
getPerimeter (RectangleShape r) = 2 * rectWidth r + 2 * rectLength r 

getArea :: Shape -> Double
getArea (CircleShape c)    = pi * radius c ^ 2
getArea (SquareShape s)    = side s ^ 2
getArea (RectangleShape r) = rectWidth r * rectLength r
