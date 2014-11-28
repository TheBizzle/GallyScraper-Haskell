{-# LANGUAGE DeriveGeneric #-}
module Main(main) where

import Prelude hiding (lookup)

import Control.Arrow((>>>), (&&&))
import Control.Exception(try)

import Data.Aeson(decode, encode, FromJSON)
import Data.ByteString.Lazy.Char8(pack, unpack)
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

a |> f = f a

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
    let statusToModelMap = modelToStatusMap |> (Map.toList >>> (scalaGroupBy snd) >>> Map.fromList >>> (fmap $ fmap fst))
    let extractFromBool  = (flip lookup statusToModelMap) >>> (fromMaybe []) >>> Set.fromList
    return (extractFromBool True, extractFromBool False)
  where
    decodeAsMap :: String -> Map String Status
    decodeAsMap = pack >>> decode >>> (fromMaybe Map.empty)

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
        let modelSet = jsonStr |> (decodeAsStrArr >>> (fromMaybe []) >>> Set.fromList)
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
    writeFile goodsFile $ toJSON newGoods
    writeFile badsFile  $ toJSON newBads
  where
    toJSON = Set.toList >>> encode >>> unpack

statusOracleURL :: URL
statusOracleURL = "http://localhost:9000/model/statuses.json"

goodsFile :: FilePath
goodsFile = "goods.json"

badsFile :: FilePath
badsFile = "bads.json"

goodStatus :: String
goodStatus = "compiling"

scalaGroupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
scalaGroupBy f = ((sortBy (compare `on` f)) >>> (groupBy ((==) `on` f)) >>> (fmap ((head >>> f) &&& id)))
