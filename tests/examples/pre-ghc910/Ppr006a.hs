{-# LANGUAGE QuasiQuotes #-}
module Ppr006a where

commands :: [Command]
commands = [
    command "novisual" "cancel visual selection" $
      sendEventCurrent EvNoVisual

  -- insert a song right after the current song
  , command "insert" [help|
      inserts a song to the playlist. The song is inserted after the currently
      playing song.
      |] $ do
      st <- MPD.status
      case MPD.stSongPos st of
        Just n -> do
          -- there is a current song, insert after
          sendEventCurrent (EvInsert (n + 1))
        _ -> do
          -- there is no current song, just add
          sendEventCurrent EvAdd

  ]

