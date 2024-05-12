-- | Checkpoints in the blockchain (every 1000th blocks)

{-# LANGUAGE CPP #-}
module Bitcoin.BlockChain.Checkpoint where

--6-----------------------------------------------------------------------------

import Bitcoin.Protocol.Hash

--10----------------------------------------------------------------------------

#ifndef WITH_TESTNET

--14----------------------------------------------------------------------------
-- * the real network

theGenesisBlock :: Hash256
theGenesisBlock = hash256FromTextBE "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"

theCheckpoints :: [(Int,Hash256)]
theCheckpoints =
  [ (0     , hash256FromTextBE "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f" )
  , (248000 , hash256FromTextBE "000000000000004d945017c14b75a3a58a2aa6772cacbfcaf907b3bee6d7f344" )
  ]

--273---------------------------------------------------------------------------

#else

--277---------------------------------------------------------------------------
-- * testnet3

theGenesisBlock :: Hash256
theGenesisBlock = hash256FromTextBE "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"

theCheckpoints :: [(Int,Hash256)]
theCheckpoints =
  [ (0     , hash256FromTextBE "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943" )
  , (97000 , hash256FromTextBE "0000000000096581c7c0fe4d15ea0212f1087fc79f3735a892ee262c384b3741" )
  ]

#endif

--387---------------------------------------------------------------------------

