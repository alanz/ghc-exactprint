module CommentPlacement where

liftToTopLevel' modName pn@(L _ n) = do
  liftToMod
  --step0: comment
  liftToMod
    where
       --step1: comment
       liftToMod = return ()

c04 r@Resource{..} = do
    let result = do
            acceptStr <- lookup HTTP.hAccept reqHeaders
            where
                -- this is so that in addition to getting back the resource
                provided' = map dupContentType provided
                dupContentType (a, b) = (a, (a, b))
