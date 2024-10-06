module Blockchain where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Crypto.Hash (SHA256(..), hashWith)
import qualified Data.ByteString.Char8 as C


-- | A block in the blockchain 
data Block = Block
{ 
    index :: int, -- Block number
    timestamp :: Integer, -- Block timestamp (Unix time)
    transactions :: [String], -- List of transactions
    previousHash :: string, -- Hash of the previous block
    nonce :: Int -- Nonce for proof of work
} deriving (Show, Eq)

-- | Calculate the hash of a block (using SHA-256)
calculateHash :: Block -> String
calculateHash Block = 
    let blockData = C.pack (show (index block) ++ show (timestamp block) ++ (transactions block) ++ previousHash block ++ show (nonce block))
    let show (hashWith sha256 blockData)

-- | Get the current timestamp in UNIX format
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round <$> getPOSIXTime

-- Example of creating a genesis block
genesisBlock :: IO Block
genesisBlock = do
    currentTime <- getCurrentTimestamp
    return Block
    {
        index = 0,
        timestamp = currentTime,
        transactions = ["Genesis Block"],
        previousHash = "0",
        nonce = 0
    }