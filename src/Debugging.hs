{-# LANGUAGE NoMonomorphismRestriction #-}

import HAppS.Server
import HAppS.State
import Misc
import StateStuff
import Safe


-- trying to debug faulty implementation of cookies; there's a thread in happs googlegroup.
-- req1,req2 :: Monad m => am Request
mreq1 = safeRead "Request {rqMethod = GET, rqPaths = [], rqQuery = \"\", rqInputs = [], rqCookies = [], rqVersion = Version 1 1, rqHeaders = fromList [(\"accept\",HeaderPair {hName = \"accept\", hValue = [\"text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\"]}),(\"accept-charset\",HeaderPair {hName = \"accept-charset\", hValue = [\"ISO-8859-1,utf-8;q=0.7,*;q=0.7\"]}),(\"accept-encoding\",HeaderPair {hName = \"accept-encoding\", hValue = [\"gzip,deflate\"]}),(\"accept-language\",HeaderPair {hName = \"accept-language\", hValue = [\"en-us,en;q=0.5\"]}),(\"connection\",HeaderPair {hName = \"connection\", hValue = [\"keep-alive\"]}),(\"host\",HeaderPair {hName = \"host\", hValue = [\"localhost:5001\"]}),(\"keep-alive\",HeaderPair {hName = \"keep-alive\", hValue = [\"300\"]}),(\"user-agent\",HeaderPair {hName = \"user-agent\", hValue = [\"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.16) Gecko/20080718 Ubuntu/8.04 (hardy) Firefox/2.0.0.16\"]})], rqBody = Body Empty, rqPeer = (\"localhost\",50453)}"

mreq2 = safeRead "Request {rqMethod = GET, rqPaths = [\"start-happs-on-boot\"], rqQuery = \"\", rqInputs = [], rqCookies = [(\"sid\",Cookie {cookieVersion = \"\", cookiePath = \"\", cookieDomain = \"\", cookieName = \"sid\", cookieValue = \"-1184419469\"})], rqVersion = Version 1 1, rqHeaders = fromList [(\"accept\",HeaderPair {hName = \"accept\", hValue = [\"text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\"]}),(\"accept-charset\",HeaderPair {hName = \"accept-charset\", hValue = [\"ISO-8859-1,utf-8;q=0.7,*;q=0.7\"]}),(\"accept-encoding\",HeaderPair {hName = \"accept-encoding\", hValue = [\"gzip,deflate\"]}),(\"accept-language\",HeaderPair {hName = \"accept-language\", hValue = [\"en-us,en;q=0.5\"]}),(\"connection\",HeaderPair {hName = \"connection\", hValue = [\"keep-alive\"]}),(\"cookie\",HeaderPair {hName = \"cookie\", hValue = [\"sid=\\\"-1184419469\\\"\"]}),(\"host\",HeaderPair {hName = \"host\", hValue = [\"localhost:5001\"]}),(\"keep-alive\",HeaderPair {hName = \"keep-alive\", hValue = [\"300\"]}),(\"referer\",HeaderPair {hName = \"referer\", hValue = [\"http://localhost:5001/tutorial/stringtemplate-basics\"]}),(\"user-agent\",HeaderPair {hName = \"user-agent\", hValue = [\"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.16) Gecko/20080718 Ubuntu/8.04 (hardy) Firefox/2.0.0.16\"]})], rqBody = Body Empty, rqPeer = (\"localhost\",42390)}"

t1 :: Maybe SessionKey
t1 = do
  req1 <- mreq1
  readData (readCookieValue "sid" ) req1

t2 :: Maybe SessionKey
t2 = do
  req2 <- mreq2
  readData (readCookieValue "sid" ) req2