-- | Checkpoints in the blockchain (every 1000th blocks)

{-# LANGUAGE CPP #-}
module CommentPlacement6 where

#ifndef WITH_TESTNET

--8-----------------------------------------------------------------------------
-- * the real network

theGenesisBlock :: Hash256
theGenesisBlock = 1

--14----------------------------------------------------------------------------

#else

--18----------------------------------------------------------------------------
-- * testnet3

theGenesisBlock :: Hash256
theGenesisBlock = 2

#endif

--26----------------------------------------------------------------------------

