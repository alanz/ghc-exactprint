-- From ./hackage-roundtrip-work/ghcjs-promise-0.1.0.3/src/Data/JSVal/Promise.hs
{-# LANGUAGE JavaScriptFFI
           , OverloadedStrings
           #-}

{-|

Module      : Data.JSVal.Promise
Copyright   : (c) Alejandro Durán Pallarés, 2016
License     : BSD3
Maintainer  : vwwv@correo.ugr.es
Stability   : experimental


Data.JSVal.Promise defines `Promise`, a direct bind to javascript promise objects.

- You can import/export them from javascript code using its `FromJSVal` and `ToJSVal` instances.

- You can extract its value, blocking till computation has finished, using `await`. (you can safely call
  it several time from different threads, the  associated computation will run once, and then memorized)

- You can create new promise (to possible use js side) containing arbitrary haskell code using `promise`.

For some usage example, checkout this [blog entry](http://the.spaghetticodeball.xyz/haskell/javascript/2016/10/10/new-library-ghcjs-promise.html).

-}


module Data.JSVal.Promise( Promise()
                         , await
                         , promise
                         ) where

import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign
import Control.Exception
import Control.Concurrent


newtype Promise = Promise {fromPromise :: JSVal}

instance FromJSVal Promise where
      fromJSVal x = do is_promise <- js_check_if_promise x
                       if is_promise
                        then return . Just $ Promise x
                        else return Nothing

instance ToJSVal Promise where
      toJSVal   =  return . fromPromise

-- | If the promise is return through "then", it will return `Right`;
--   if it return through "catch", then it will return `Left`
await   :: Promise -> IO (Either JSVal JSVal)
await (Promise jsval) = do result <- js_await     jsval
                           x      <- js_attribute "result" result
                           ok     <- isTruthy <$> js_attribute "ok" result
                           if ok
                            then return (Right x)
                            else return (Left  x)

-- | A `Right` value will be sent as a normal value through "then", a left
--   value will be sent through "catch" (by javascript convention, representing
--   an exception).
--
--   The block will start executing immediately, no mater if there's something waiting
--   for it or not.
--
--   If the execution block launches an exception, then the promise will be receive
--   as "reject", the javascript value "new Error('Haskell side error')"
promise :: IO (Either JSVal JSVal) -> IO Promise
promise action = do ref     <- js_book_promise
                    promise <- js_set_promise ref
                    myid    <- myThreadId
                    forkIO $ do val_ <- try action
                                case val_ of

                                 Right (Right x) -> js_do_resolve ref x

                                 Right (Left  x) -> js_do_reject  ref x

                                 Left  exc       -> do throwTo myid (exc::SomeException)
                                                       js_do_reject  ref =<< create_error
                    return $ Promise promise
-----------------------------------------------------------------------
-----------------------------------------------------------------------


-- This works because the [algorithm](http://www.ecma-international.org/ecma-262/6.0/#sec-promise.resolve)
-- explicitly demands that Promise.resolve must return the exact object passed in if and only if
-- it is a promise by the definition of the spec.
-- (from stackoverflow http://stackoverflow.com/questions/27746304/how-do-i-tell-if-an-object-is-a-promise)
foreign import javascript safe
    "Promise.resolve($1) == $1"
    js_check_if_promise :: JSVal -> IO Bool

foreign import javascript safe
    "$2[$1]"
    js_attribute        :: JSString -> JSVal -> IO JSVal

foreign import javascript safe
    "new Error('Haskell side error')"
    create_error        :: IO JSVal


foreign import javascript safe
    "__js_book_promise()"
    js_book_promise     :: IO JSVal

foreign import javascript safe
    "__js_set_promise($1)"
    js_set_promise      :: JSVal -> IO JSVal

foreign import javascript safe
    "__js_do_reject($1,$2);"
    js_do_reject        :: JSVal -> JSVal -> IO ()

foreign import javascript safe
    "__js_do_resolve($1, $2);"
    js_do_resolve       :: JSVal -> JSVal -> IO ()

foreign import javascript interruptible
    "__js_await($1,$c);"
    js_await            :: JSVal -> IO JSVal

