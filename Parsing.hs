module Parsing (getOptions) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import InterComp
import Fs

data Options = Options
 { function_id :: Int
 , precision  :: Double
 , initial_x  :: Maybe (Double, Double)
 , show_stuff :: Bool
 } deriving Show

type OptionsTuple = (Int, Double, Maybe (Double, Double), Bool)

defaultOpts = Options
 { function_id = 0
 , precision = 0.0
 , initial_x = Nothing
 , show_stuff = False
 }


toTuple :: Options -> OptionsTuple
toTuple (Options f p i s) = (f,p,i,s)


options :: [OptDescr (Options -> Options)]
options = 
 [ Option ['f'] ["function-id"]
    (ReqArg ((\n opts -> opts {function_id = n}) . read) "id")
    "function identification number"
 , Option ['p'] ["precision"]
    (ReqArg ((\p opts -> opts {precision = p}) . read) "presicion")
    "precision tolerance"
 , Option ['i'] ["initial-x"]
    (ReqArg ((\i opts -> opts {initial_x = Just i}) . read) "interval")
    "precision tolerance"
 , Option ['s'] ["show-stuff"]
    (NoArg (\opts -> opts {show_stuff = True}))
    "show the calculations"
 ]

parse :: [String] -> IO OptionsTuple
parse argv = case getOpt Permute options argv of
 (o,_,[]) -> return $ toTuple $ foldl (&) defaultOpts o
 (_,_,errors) -> ioError $ userError $ concat errors

getOptions :: IO OptionsTuple
getOptions = getArgs >>= parse
