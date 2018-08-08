import RainbowAssign
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8           -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40          -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

 --------------------------------------pwReduce (maps a hash value to an arbitary password)-----------------

--myHash is convered to list of integers and then it is converted to a string.
pwReduce :: Hash -> Passwd
pwReduce myHash = cvtToLetter (baseConvert myHash)

--Generating a list of mod values.
generateModList :: Hash -> Int -> Int -> [Int]
generateModList val acc base
    | (acc == pwLength)= []
    | (otherwise)= [mod (fromEnum val) base] ++ (generateModList (div val (toEnum (base))) (acc+1) base)

--Reversing the mod value list generated in generateModList.
baseConvert :: Hash -> [Int]
baseConvert numb= reverse $ generateModList numb 0 nLetters

--convert to letters from base list
cvtToLetter :: [Int] -> [Char]
cvtToLetter []= []
cvtToLetter (x:thisList)= [toLetter x] ++ (cvtToLetter thisList)


---------------------------------------The Map Data Structure------------------------------------

--creates a list of tuples by zipping Hash and Password.
rainbowTable ::  Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable myWidth initPasswds= Map.fromList (zip (generateHashes myWidth initPasswds) initPasswds)

generateHashes :: Int -> [Passwd] -> [Hash]
generateHashes givenWidth initPasswds       --Generating a hash value for each given password.
  | (givenWidth ==0)= [pwHash x | x <-initPasswds]      --Just hash once (Base case)
  | (otherwise)= generateHashes (givenWidth-1) (map pwReduce listOfHash)    --Get new set of reduced passwords to be hashed again.
  where listOfHash= (map pwHash initPasswds)      --apply pwHash to all the passwords.

-----------------------------------Creating, Reading, and Writing Tables --------------------------

-- Given code

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

test1 = do
  table <- readTable filename
  return (Map.lookup 0 table)

-------------------------------------- Reversing Hashes ------------------------------------------

--calls another function performSearch with widthTable in a list to retain original value.
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword rTable widthTable hashVal= performSearch rTable [widthTable,widthTable] [hashVal] count
  where count=0

--additional args: [Hash]: Will maintain history of hashes, count: to keep track of index.
performSearch :: Map.Map Hash Passwd -> [Int] -> [Hash] -> Int -> Maybe Passwd
performSearch rTable widthTable myHashList count
  | (valueFound /= Nothing)= searchRow (Maybe.fromJust valueFound) (myHashList !! 0) (widthTable !! 0) (myHashList !! count) rTable rowCounter --hash value found in the table, search the potential row.
  | (valueFound==Nothing && (widthTable !! 1) ==0)= Nothing      --hash not found (even after re-hashing)
  | otherwise= performSearch rTable [widthTable !! 0, ((widthTable !! 1)-1)] (myHashList ++ newHash) (count+1)  --not found but search again after rehash
  where newHash= [pwHash (pwReduce p) | p <-myHashList, (p == myHashList !! count)]     --add new hashed value to the list
        valueFound= Map.lookup (myHashList !! count) rTable
        rowCounter=0


--Hash is found, and particular row will be searched.
searchRow  :: Passwd -> Hash ->  Int -> Hash -> Map.Map Hash Passwd -> Int -> Maybe Passwd
searchRow orgPasswd hashVal widthTable matchedHash rTable counter
    | (counter == (widthTable+1))= findPassword newMapTable widthTable hashVal    --false positive: search again but on a new map excluding false positive rows
    | ((pwHash orgPasswd)==hashVal)= Just orgPasswd       --Hash the given password and compare it with given hash value.
    | otherwise= searchRow (pwReduce (pwHash orgPasswd)) hashVal widthTable matchedHash rTable (counter+1)    --hash and reduce, and compare again.
    where newMapTable= (Map.fromList (filter (\(a, b) -> a /= matchedHash) (Map.toList rTable)))  --New Map without false positive row

---------------------------------------- Testing ------------------------------------------------

--Given code

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)
