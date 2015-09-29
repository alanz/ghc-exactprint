{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Maid.ApacheMimeTypes where


import qualified Data.Text as T
import Air.TH



apache_mime_types :: T.Text
apache_mime_types = [here|

# This file maps Internet media types to unique file extension(s).
# Although created for httpd, this file is used by many software systems
# and has been placed in the public domain for unlimited redisribution.
#
# The table below contains both registered and (common) unregistered types.
# A type that has no unique extension can be ignored -- they are listed
# here to guide configurations toward known types and to make it easier to
# identify "new" types.  File extensions are also commonly used to indicate
# content languages and encodings, so choose them carefully.
#
# Internet media types should be registered as described in RFC 4288.
# The registry is at .
#
# MIME type (lowercased)      Extensions
# ============================================  ==========
# application/1d-interleaved-parityfec
# application/3gpp-ims+xml
# application/activemessage
application/andrew-inset      ez |]


testComplex    = assertBool "" ([$istr|
        ok
#{Foo 4 "Great!" : [Foo 3 "Scott!"]}
        then
|] == ("\n" ++
    "        ok\n" ++
    "[Foo 4 \"Great!\",Foo 3 \"Scott!\"]\n" ++
    "        then\n"))

