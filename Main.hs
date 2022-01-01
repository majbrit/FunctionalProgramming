{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import Graphics.Vega.VegaLite

-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Person
instance ToJSON Person

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "pizza.json"

-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--
-- Read the local copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
--}

{--}
-- Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL
--}

-- Text for heading
st = putStrLn "\nWelcome to the pizza project \n"  

-- Output all data of the persons in the form: "Rose" "Red" is 39 years old and doesn't like pizza.
introduction :: [Person] -> IO()
introduction [] = putStrLn "NOTHING"
introduction [p] = introduction2 p
introduction (p:pl) = do
  introduction2 (p) 
  introduction (pl)
introduction2 :: Person -> IO ()
introduction2 (Person firstName lastName age likesPizza) 
  | likesPizza==True = putStrLn(show firstName ++ " " ++ show lastName ++ " is " ++ show age ++ " years old and likes pizza.")
  | otherwise = putStrLn(show firstName ++ " " ++ show lastName ++ " is " ++ show age ++ " years old and doesn't like pizza.")

-- Reactions to the user's answers about whether he or she likes pizza
doeslike "yes" = putStrLn ("Ahhh\n")
doeslike "Yes" = putStrLn ("Ahhh\n")
doeslike a = putStrLn ("Wrong answer\n")

-- Main
main = do
  st
  -- Get JSON data and decode it
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. 
  case d of
    Left err -> putStrLn err
    Right ps -> do

      -- Write heading
      print ps 
      putStrLn ""
      -- -- Output all data of the persons in the form: "Rose" "Red" is 39 years old and doesn't like pizza.
      introduction ps

      -- Ask user questions
      putStrLn "\nHello! Please write your name: "  
      name <- getLine  
      putStrLn ("\n" ++ name ++ ", do you like pizza? ")  
      like <- getLine
      doeslike like

      -- adoption
      putStrLn ("Bye\n")
