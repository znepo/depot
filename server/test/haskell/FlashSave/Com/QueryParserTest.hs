module QueryParserTest
    where
      
import Test.HUnit
import Test.QuickCheck
import Data.Map as Map
import FlashSave.Com.QueryParser
import FlashSave.Com.Protocol
import Text.ParserCombinators.Parsec
    
fixture = runTestTT $ TestList [--TestLabel "Req Line Parsing" testRequestLineParsing,
                                TestLabel "Request Handling" testHeaderParsing]
          
reqline    = "GET /test/this/out?var=val HTTP/1.1" 
reqline'   = "GET /test/this/out HTTP/1.1" 
reqline''  = "GET /test/this/out/ HTTP/1.1" 
testreq  = [
            "Host: localhost:2323\r",
            "User-Agent: Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8) Gecko/20051025 Firefox/1.5",
            "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5",
            "Accept-Language: en-us,en;q=0.5\r",
            "Accept-Encoding : gzip,deflate\r",
            "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r",
            "Keep-Alive: 300\r",
            "Connection: keep-alive\r"]
expectedHeaders = Map.fromList  [("Host", "localhost:2323"),
            ("User-Agent", "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8) Gecko/20051025 Firefox/1.5"),
            ("Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),
            ("Accept-Language", "en-us,en;q=0.5"),
            ("Accept-Encoding", "gzip,deflate"),
            ("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7"),
            ("Keep-Alive", "300"),
            ("Connection", "keep-alive")]



testHeaderParsing = TestCase $ do
                      let result = parseHeader testreq
                      case result of 
                        Right hdrs -> assertEqual "headers" expectedHeaders hdrs
                        Left err   -> assertFailure ((show err) ++ (unlines testreq))
    where
      parseHeader tr = parse headers "headers" (unlines tr)

-- testRequestLineParsing = TestCase $ do
--                            let result   = parse requestLine "rline" reqline 
--                            let result'  = parse requestLine "rline" reqline' 
--                            let result'' = parse requestLine "rline" reqline'' 
--                            case result of
--                              Right (Hline GET (QueryWhere url vars) "HTTP/1.1") -> do
--                                          assertEqual "url" "test/this/out" url
--                                          assertEqual "vars" (Map.fromList [("var","val")]) vars
--                              Left err -> assertFailure ((show err) ++ "\n" ++ reqline)
--                            case result' of
--                              Right (Hline GET (QueryAll url) "HTTP/1.1") -> do
--                                          assertEqual "url" "test/this/out" url
--                              Left err -> assertFailure ((show err) ++ "\n" ++ reqline)
--                            case result'' of
--                              Right (Hline GET (QueryAll url) "HTTP/1.1") -> do
--                                          assertEqual "url" "test/this/out" url
--                              Left err -> assertFailure ((show err) ++ "\n" ++ reqline)
                           

-- testRequestHandling = TestCase $ do 
--                        result <- handleRequest (reqline:testreq) dorequest
--                        case result of
--                          Right resp -> do
--                                      resp' <- resp
--                                      assertEqual "dummy response" "blah" resp'
--                          Left err   -> assertFailure ((show err) ++ "\n" ++ reqline) 
--     where
--       dorequest GET url vars hvars = do
--                                    assertEqual "url eq" "test/this/out" url
--                                    return "blah"
