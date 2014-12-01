{-# LANGUAGE DeriveGeneric #-}
module Main(main) where

import Prelude hiding (lookup)

import Control.Arrow((>>>), (&&&))
import Control.Exception(try)

import Data.Aeson(decode, encode, FromJSON)
import Data.ByteString.Lazy.Char8(pack, unpack)
import Data.Foldable(Foldable, toList)
import Data.Function(on)
import Data.List(groupBy, intercalate, sort, sortBy)
import Data.Map(lookup, Map)
import Data.Maybe(fromMaybe)
import Data.Set(difference, Set)

import GHC.Generics(Generic)

import Network.HTTP(getRequest, getResponseBody, simpleHTTP)

import qualified Data.Map as Map
import qualified Data.Set as Set

type URL    = String
type Models = Set String

data Status = Status { status :: String } deriving (Generic, Show)

instance FromJSON Status

main :: IO ()
main =
  do
    (newGoods, newBads) <- findGoodsAndBads
    (oldGoods, oldBads) <- findOldGoodsAndBads
    printDiffs newGoods newBads oldGoods oldBads
    modernize newGoods newBads

findGoodsAndBads :: IO (Models, Models)
findGoodsAndBads =
  do
    jsonStr <- simpleHTTP (getRequest statusOracleURL) >>= getResponseBody
    let modelToStatusMap = jsonStr |> (decodeAsMap >>> (fmap $ status >>> (== goodStatus)))
    let statusToModelMap = flipMap modelToStatusMap
    let extractFromBool  = (flip lookup statusToModelMap) >>> (fromMaybe Set.empty)
    return (extractFromBool True, extractFromBool False)
  where
    flipMap          = Map.toList >>> (scalaGroupBy snd) >>> groupedToMapPair >>> Map.fromList
    groupedToMapPair = fmap (\(a, bs) -> (a, bs |> ((fmap fst) >>> Set.fromList)))
    tee f            = fmap $ f &&& id
    decodeAsMap :: String -> Map String Status
    decodeAsMap    = pack >>> decode >>> (fromMaybe Map.empty)
    scalaGroupBy f = sort >>> group >>> pair
      where
        sort  = sortBy (compare `on` f)
        group = groupBy ((==) `on` f)
        pair  = tee $ head >>> f

findOldGoodsAndBads :: IO (Models, Models)
findOldGoodsAndBads =
  do
    goods <- slurpSetFrom goodsFile
    bads  <- slurpSetFrom badsFile
    return (goods, bads)
  where
    safeReadFile :: String -> IO (Either IOError String)
    safeReadFile = readFile >>> try
    decodeAsStrArr :: String -> Maybe [String]
    decodeAsStrArr = pack >>> decode
    slurpSetFrom filepath =
      do
        jsonStrEither <- safeReadFile filepath
        let jsonStr  = either (const "[]") id jsonStrEither
        let modelSet = jsonStr |> (decodeAsStrArr >>> maybeFToSet)
        return modelSet

printDiffs :: Models -> Models -> Models -> Models -> IO ()
printDiffs newGoods newBads oldGoods oldBads =
  do
    printUpdate "Newly working models" newGoods oldGoods
    putStrLn ""
    printUpdate "Newly broken models" newBads oldBads
  where
    printUpdate label news olds = putStrLn entry
      where
        entry = label ++ ":\n" ++ diffs
        diffs = olds |> ((difference news) >>> Set.toList >>> sort >>> (intercalate "\n"))

modernize :: Models -> Models -> IO ()
modernize newGoods newBads =
  do
    writeSetToFile newGoods goodsFile
    writeSetToFile newBads  badsFile
  where
    writeSetToFile set file = writeFile file $ toJSON set
    toJSON = Set.toList >>> encode >>> unpack

statusOracleURL :: URL
statusOracleURL = "http://localhost:9000/model/statuses.json"

goodsFile :: FilePath
goodsFile = "goods.json"

badsFile :: FilePath
badsFile = "bads.json"

goodStatus :: String
goodStatus = "compiling"

maybeFToSet :: (Foldable f, Ord a) => Maybe (f a) -> Set a
maybeFToSet = (fmap toList) >>> (fromMaybe []) >>> Set.fromList

a |> f = f a
