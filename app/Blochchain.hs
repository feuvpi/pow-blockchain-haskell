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

-- | Check if the block's hash is valid (Proof of Work)
isValidHash :: Block -> Int -> Bool
isValidHash block difficulty =
    let target = replicate difficulty '0' -- Difficulty is the number of leading zeros required in the hash
        blockHash = calculateHash block
    in take difficulty blockHash == target


-- | Mine a block by finding a valid nonce
mineBlock :: Block -> Int -> Block
mineBlock block difficulty =
    let tryNonce blk nonce =
            let newBlock = blk { nonce = nonce }
            in if isValidHash newBlock difficulty
               then newBlock
               else tryNonce blk (nonce + 1)
    in tryNonce block (nonce block)
