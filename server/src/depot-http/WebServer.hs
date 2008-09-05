{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module WebServer (startHTTP, wwwroot, depotport,httpport)
    where
    
import Prelude hiding (catch)      
    
import Network hiding (accept)
import Control.Monad
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Directory
import System.IO
import System.IO.Unsafe

import Text.ParserCombinators.Parsec

import Data.IORef
import qualified Data.Map as Map

#if  !(mingw32_HOST_OS)
import System.Posix
#endif


import Depot.Error
import Depot.IO.Binary hiding (get,put)
import Depot.Base
import Depot.Com.QueryParser    
import Depot.Com.Protocol
import Depot.Util.Utils
    
import qualified Depot.Logging as LOG

-- TODO:

-- Add directory index listing and indexfiles

startHTTP dport = withSocketsDo $! do
#if !(mingw32_HOST_OS)              
    installHandler sigPIPE Ignore Nothing
#endif
    writeIORef depotport dport
    hport     <- readIORef httpport
    openConns <- atomically $ newTMVar 0
    bracket (listenOn hport)
            (\s -> sClose s)
            (\s -> acceptLoop s openConns)

maxConnections = 32

wwwroot   = unsafePerformIO $ newIORef "web"
depotport = unsafePerformIO $ newIORef (PortNumber 2323)
httpport  = unsafePerformIO $ newIORef (PortNumber 8080)

acceptLoop sock openConns = do
  open         <- atomically $! readTMVar openConns
  LOG.infoIO $ "There are now " ++ show open ++ " HTTP connections"
  (chandle, _) <- accept sock
  atomically $! addConnection openConns maxConnections
  forkIO (catch (acceptConnection chandle `finally` cleanUp chandle)
                (\e -> LOG.errorIO $ "HTTP Error" ++ show e))
  acceptLoop sock openConns 
      where
        cleanUp chandle = do 
          LOG.infoIO "closing client handle"
          isOpen <- hIsOpen chandle
          when isOpen (catch (hClose chandle)
                            (handleCloseError))
          atomically $ removeConnection openConns
          open <- atomically $! readTMVar openConns
          LOG.infoIO $ "removed HTTP connection, " ++ show open ++ " left"
        handleCloseError e = LOG.errorIO $ "error closing client handle" ++ show e
                             
acceptConnection chandle = do
  header <- readHeader chandle
  (catch (dispatchRequest header chandle)
         (badRequest header chandle))
  return ()
      where
        badRequest header chandle e = 
            let mess = "Unexpected exception " ++ show e ++ "\nRequest was:\n" ++ header
                err  = ServerError mess []
            in
              do
                LOG.errorIO $ show e
                hPutStr chandle $! defaultErrResponse err
  

dispatchRequest header handle = 
    if isDepotRequest header 
       then proxyRequestToDepot handle header
       else do
  LOG.info $ "http request: \n" ++ header   
  (result,_) <- runStateErrorT (handleRequest reqdelegate header) (AppState 0 Map.empty)
  case result of
    Right status -> LOG.infoIO $ show status
    Left err     -> 
        let code     = responseErr err
            body     = messageERR err ++ " \nRequest was:\n" ++ header
            response = defaultResponse' code body (headersERR err)
        in do
            LOG.errorIO (show err) 
            LOG.errorIO response
            hPutStr handle $! response
    where
      reqdelegate request = 
            let 
                hline = hdrLine request
                hvars = hdrVars request
                uvars = hdrUVars hline
                -- meth  = hdrMethod hline
                path  = hdrSubject hline
                -- unsup = ServerError ("Method " ++ show meth ++ " not supported") []
            in filehandler handle path uvars hvars
               

proxyRequestToDepot handle header = do
  let hdr    = stripPrefix header
      isbody = isRequestWithBody header
  LOG.infoIO $ "proxying: \n" ++ header
  LOG.infoIO $ "proxying(raw): \n" ++ show header
  dp       <- readIORef depotport
  depothdl <- connectTo "localhost" dp
  LOG.infoIO $ "sending request through (with body: " ++ show isbody ++ ")"
  hPutStr depothdl $ hdr ++ "\r\n"
  -- TODO verify length of read... (now forced to read too long...)
  case isbody of Nothing -> return (); Just len -> rawHandleCopyNB len handle depothdl
  contents <- hGetContents depothdl
  hFlush depothdl
  LOG.infoIO $ "got contents of length: " ++ show (length contents)
  hPutStr handle contents
  hFlush handle
  hClose depothdl

-- removes the "/depot" prefix from the request path
stripPrefix header = let (meth,url) = span (\c -> c /= '/') header
                    in meth ++ drop 6 url
                         
isDepotRequest header = let url = dropWhile (\c -> c /= '/') header
                        in prefix "/depot" url

isRequestWithBody hdr =
    let hasContentLen  = Map.lookup "content-length" hdrs 
        snepoPutKludge = Map.lookup "x-snepo-request-kludge" hdrs
        putReq   = case hdr of
                      'P':'U':'T':_     -> True
                      'P':'O':'S':'T':_ -> True
                      _                 -> False
        hdrs     = case parse headers "headers" hdr of
                     Left err -> error $ "completely unexpected parse error: " ++ show err
                     Right hs -> hdrVars hs
    in if putReq
           then hasContentLen >>= return . read
           else snepoPutKludge >> hasContentLen >>= return . read

prefix pfix str = take (length pfix) str == pfix

ok = Ok "Ok" []

filehandler handle path uvars hvars = do
  -- TODO fix me up, add directory index searching etc...
  resp <- liftIO $ readFile `catch` (\e -> LOG.errorIO (show e) >> return Nothing)
  case resp of 
    Nothing -> failure $ NotFound ("File " ++ path ++ " not found") []
    Just r  -> success r
    where
      readFile = do
        p    <- readIORef wwwroot
        file <- normalizePath (p /// path)
        bracket (openBinaryFile file ReadMode)
                (\h -> hClose h)
                (sendFileToClient file)
      sendFileToClient file fh = do
        sz <- hFileSize fh
        let cl = ("Content-Length", show sz)
            ct = ("Content-Type", mimeType file)
            sv = ("Server","Depot-HTTP-Proxy")
            rg = ("Accept-Ranges","none")            
            re = responseLine ok ++ showHeaders (stdHeaders [cl,ct,sv,rg])
        hPutStr handle $! re
        LOG.infoIO $ "response: \n" ++ re
        rawHandleCopy sz fh handle
        return $ Just ok
               
      normalizePath path = do
              exists <- doesFileExist path
              if exists 
                 then return path
                 else checkIndexes path indexfiles
                      
      checkIndexes path []         = error "Not found"
      checkIndexes path (i:ndexes) = do
              exists <- doesFileExist (path /// i)
              if exists
                 then return $ path /// i
                 else checkIndexes path ndexes

indexfiles = ["index.html", "index.snepo", "index.htm", "default.htm", "Default.htm"]

mimeType path = let suff = reverse . takeWhile (\c -> c /= '.') . reverse
                    ext  = '.' : suff path
                in
                  case ext of
                    "" -> "text/html"
                    ".xhtml" -> "text/html"
                    ".snepo" -> "text/html"
                    ".skd" -> "application/x-koan"
                    ".el" -> "text/x-script"
                    ".iges" -> "model/iges"
                    ".iefs" -> "image/ief"
                    ".omcr" -> "application/x-omcregerator"
                    ".spc" -> "text/x-speech"
                    ".mzz" -> "application/x-vnd"
                    ".csh" -> "text/x-script"
                    ".lhx" -> "application/octet-stream"
                    ".xls" -> "application/x-msexcel"
                    ".wsc" -> "text/scriplet"
                    ".tsp" -> "audio/tsplayer"
                    ".mpga" -> "audio/mpeg"
                    ".ccad" -> "application/clariscad"
                    ".pps" -> "application/vnd"
                    ".htt" -> "text/webviewhtml"
                    ".texinfo" -> "application/x-texinfo"
                    ".p10" -> "application/x-pkcs10"
                    ".xlt" -> "application/x-excel"
                    ".ppt" -> "application/x-mspowerpoint"
                    ".es" -> "application/x-esrehber"
                    ".fdf" -> "application/vnd"
                    ".def" -> "text/plain"
                    ".p12" -> "application/x-pkcs12"
                    ".pdb" -> "chemical/x-pdb"
                    ".fpx" -> "image/vnd"
                    ".xlv" -> "application/x-excel"
                    ".xlw" -> "application/x-msexcel"
                    ".xgz" -> "xgl/drawing"
                    ".wsrc" -> "application/x-wais-source"
                    ".htx" -> "text/html"
                    ".vrt" -> "x-world/x-vrt"
                    ".js" -> "application/x-javascript"
                    ".fif" -> "image/fif"
                    ".pic" -> "image/pict"
                    ".doc" -> "application/msword"
                    ".skm" -> "application/x-koan"
                    ".pdf" -> "application/pdf"
                    ".tsv" -> "text/tab-separated-values"
                    ".3dm" -> "x-world/x-3dmf"
                    "tbnl" -> "image/jpeg"
                    ".ppz" -> "application/mspowerpoint"
                    ".spl" -> "application/futuresplash"
                    ".txt" -> "text/plain"
                    ".aos" -> "application/x-nokia-9000-communicator-add-on-software"
                    ".rexx" -> "text/x-script"
                    ".skp" -> "application/x-koan"
                    ".rtx" -> "text/richtext"
                    ".jpeg" -> "image/pjpeg"
                    ".css" -> "text/css"
                    ".tr" -> "application/x-troff"
                    ".lam" -> "audio/x-liveaudio"
                    ".sv4cpio" -> "application/x-sv4cpio"
                    ".cc" -> "text/x-c"
                    ".tbk" -> "application/x-tbook"
                    ".class" -> "application/x-java-class"
                    ".acgi" -> "text/html"
                    ".psd" -> "application/octet-stream"
                    ".skt" -> "application/x-koan"
                    ".spr" -> "application/x-sprite"
                    ".idc" -> "text/plain"
                    ".gtar" -> "application/x-gtar"
                    ".bcpio" -> "application/x-bcpio"
                    ".der" -> "application/x-x509-ca-cert"
                    ".mbd" -> "application/mbedlet"
                    ".3dmf" -> "x-world/x-3dmf"
                    ".rmi" -> "audio/mid"
                    ".turbot" -> "image/florian"
                    ".ra" -> "audio/x-realaudio"
                    ".xpix" -> "application/x-vnd"
                    ".cxx" -> "text/plain"
                    ".wiz" -> "application/msword"
                    ".me" -> "application/x-troff-me"
                    ".lzh" -> "application/x-lzh"
                    ".hh" -> "text/x-h"
                    ".arc" -> "application/octet-stream"
                    ".rmm" -> "audio/x-pn-realaudio"
                    ".pnm" -> "image/x-portable-anymap"
                    ".zsh" -> "text/x-script"
                    ".sid" -> "audio/x-psid"
                    ".pfunk" -> "audio/make"
                    ".zip" -> "multipart/x-zip"
                    ".dot" -> "application/msword"
                    ".inf" -> "application/inf"
                    ".wbmp" -> "image/vnd"
                    ".rmp" -> "audio/x-pn-realaudio-plugin"
                    ".snd" -> "audio/x-adpcm"
                    ".rf" -> "image/vnd"
                    ".qtc" -> "video/x-qtc"
                    ".rast" -> "image/cmu-raster"
                    ".arj" -> "application/octet-stream"
                    ".mm" -> "application/x-meme"
                    ".funk" -> "audio/make"
                    ".smil" -> "application/smil"
                    ".f90" -> "text/x-fortran"
                    ".tgz" -> "application/x-compressed"
                    ".vda" -> "application/vda"
                    ".word" -> "application/msword"
                    ".rm" -> "audio/x-pn-realaudio"
                    ".sdp" -> "application/x-sdp"
                    ".latex" -> "application/x-latex"
                    ".uri" -> "text/uri-list"
                    ".mpeg" -> "video/mpeg"
                    ".ssi" -> "text/x-server-parsed-html"
                    ".qti" -> "image/x-quicktime"
                    ".m3u" -> "audio/x-mpequrl"
                    ".sdr" -> "application/sounder"
                    ".rp" -> "image/vnd"
                    ".ms" -> "application/x-troff-ms"
                    ".deepv" -> "application/x-deepv"
                    ".aab" -> "application/x-authorware-bin"
                    ".eps" -> "application/postscript"
                    ".ssm" -> "application/streamingmedia"
                    ".lzx" -> "application/x-lzx"
                    ".wp" -> "application/wordperfect"
                    ".ins" -> "application/x-internett-signup"
                    ".art" -> "image/x-jg"
                    ".mv" -> "video/x-sgi-movie"
                    ".fli" -> "video/x-fli"
                    ".sit" -> "application/x-stuffit"
                    ".rt" -> "text/vnd"
                    ".movie" -> "video/x-sgi-movie"
                    ".hpg" -> "application/vnd"
                    ".htmls" -> "text/html"
                    ".sprite" -> "application/x-sprite"
                    ".ram" -> "audio/x-pn-realaudio"
                    ".pbm" -> "image/x-portable-bitmap"
                    ".ncm" -> "application/vnd"
                    ".bin" -> "application/x-macbinary"
                    ".pm4" -> "application/x-pagemaker"
                    ".bsh" -> "application/x-bsh"
                    ".dwf" -> "model/vnd"
                    ".oda" -> "application/oda"
                    ".rv" -> "video/vnd"
                    ".pm5" -> "application/x-pagemaker"
                    ".my" -> "audio/make"
                    ".dwg" -> "image/x-dwg"
                    ".vsd" -> "application/x-visio"
                    ".dcr" -> "application/x-director"
                    ".isu" -> "video/x-isvideo"
                    ".xyz" -> "chemical/x-pdb"
                    ".texi" -> "application/x-texinfo"
                    ".part" -> "application/pro"
                    ".pgm" -> "image/x-portable-greymap"
                    ".g3" -> "image/g3fax"
                    ".ai" -> "application/postscript"
                    ".flo" -> "image/florian"
                    ".a" -> "application/octet-stream"
                    ".xwd" -> "image/x-xwindowdump"
                    ".sst" -> "application/vnd"
                    ".vdo" -> "video/vdo"
                    ".wmlsc" -> "application/vnd"
                    ".ras" -> "image/x-cmu-raster"
                    ".uris" -> "text/uri-list"
                    ".web" -> "application/vnd"
                    ".c" -> "text/x-c"
                    ".aam" -> "application/x-authorware-map"
                    ".qd3" -> "x-world/x-3dmf"
                    ".xml" -> "text/xml"
                    ".hpgl" -> "application/vnd"
                    ".afl" -> "video/animaflex"
                    ".rpm" -> "audio/x-pn-realaudio-plugin"
                    ".mjf" -> "audio/x-vnd"
                    ".f" -> "text/x-fortran"
                    ".java" -> "text/x-java-source"
                    ".mod" -> "audio/x-mod"
                    ".g" -> "text/plain"
                    ".wk1" -> "application/x-123"
                    ".h" -> "text/x-h"
                    ".lsp" -> "text/x-script"
                    ".sbk" -> "application/x-tbook"
                    ".mp2" -> "video/x-mpeq2a"
                    ".wmlc" -> "application/vnd"
                    ".s3m" -> "audio/s3m"
                    ".mp3" -> "video/x-mpeg"
                    ".exe" -> "application/octet-stream"
                    ".aas" -> "application/x-authorware-seg"
                    ".list" -> "text/plain"
                    ".tex" -> "application/x-tex"
                    ".flx" -> "text/vnd"
                    ".text" -> "text/plain"
                    ".viv" -> "video/vnd"
                    ".niff" -> "image/x-niff"
                    ".drw" -> "application/drafting"
                    ".lst" -> "text/plain"
                    ".kar" -> "music/x-karaoke"
                    ".cer" -> "application/x-x509-ca-cert"
                    ".m" -> "text/x-m"
                    ".com" -> "text/plain"
                    ".pl" -> "text/x-script"
                    ".qcp" -> "audio/vnd"
                    ".au" -> "audio/x-au"
                    ".plx" -> "application/x-pixclscript"
                    ".uue" -> "text/x-uuencode"
                    ".sgm" -> "text/x-sgml"
                    ".hdf" -> "application/x-hdf"
                    ".pm" -> "text/x-script"
                    ".wp5" -> "application/wordperfect6"
                    ".saveme" -> "application/octet-stream"
                    ".svf" -> "image/x-dwg"
                    ".wp6" -> "application/wordperfect"
                    ".vst" -> "application/x-visio"
                    ".o" -> "application/octet-stream"
                    ".igs" -> "model/iges"
                    ".pvu" -> "paleovu/x-pv"
                    ".mhtml" -> "message/rfc822"
                    ".p" -> "text/x-pascal"
                    ".m1v" -> "video/mpeg"
                    ".lsx" -> "text/x-la-asf"
                    ".f77" -> "text/x-fortran"
                    ".xmz" -> "xgl/movie"
                    ".vsw" -> "application/x-visio"
                    ".book" -> "application/book"
                    ".aps" -> "application/mime"
                    ".cpio" -> "application/x-cpio"
                    ".s" -> "text/x-asm"
                    ".ps" -> "application/postscript"
                    ".wtk" -> "application/x-wintalk"
                    ".mjpg" -> "video/x-motion-jpeg"
                    ".t" -> "application/x-troff"
                    ".dump" -> "application/octet-stream"
                    ".jcm" -> "application/x-java-commerce"
                    ".env" -> "application/x-envoy"
                    ".cha" -> "application/x-chat"
                    ".sdml" -> "text/plain"
                    ".tcl" -> "text/x-script"
                    ".rng" -> "application/vnd"
                    ".w6w" -> "application/msword"
                    ".ivr" -> "i-world/i-vrml"
                    ".uu" -> "text/x-uuencode"
                    ".nap" -> "image/naplps"
                    ".qd3d" -> "x-world/x-3dmf"
                    ".shar" -> "application/x-shar"
                    ".vqe" -> "audio/x-twinvq-plugin"
                    ".mov" -> "video/quicktime"
                    ".pyc" -> "applicaiton/x-bytecode"
                    ".py" -> "text/x-script"
                    ".vqf" -> "audio/x-twinvq"
                    ".z" -> "application/x-compressed"
                    ".wmls" -> "text/vnd"
                    ".sea" -> "application/x-sea"
                    ".mcd" -> "application/x-mathcad"
                    ".svr" -> "x-world/x-svr"
                    ".elc" -> "application/x-elc"
                    ".unis" -> "text/uri-list"
                    ".ief" -> "image/ief"
                    ".aif" -> "audio/x-aiff"
                    ".gzip" -> "multipart/x-gzip"
                    ".nc" -> "application/x-netcdf"
                    ".mcf" -> "text/mcf"
                    ".step" -> "application/step"
                    ".imap" -> "application/x-httpd-imap"
                    ".mime" -> "www/mime"
                    ".ivy" -> "application/x-livescreen"
                    ".dl" -> "video/x-dl"
                    ".for" -> "text/x-fortran"
                    ".vql" -> "audio/x-twinvq-plugin"
                    ".mme" -> "application/base64"
                    ".xpm" -> "image/xpm"
                    ".ani" -> "application/x-navi-animation"
                    ".mrc" -> "application/marc"
                    ".asf" -> "video/x-ms-asf"
                    ".cco" -> "application/x-cocoa"
                    ".aim" -> "application/x-aim"
                    ".zoo" -> "application/octet-stream"
                    ".dp" -> "application/commonground"
                    ".vrml" -> "x-world/x-vrml"
                    ".html" -> "text/html"
                    ".qtif" -> "image/x-quicktime"
                    ".sh" -> "text/x-script"
                    ".pot" -> "application/vnd"
                    ".wmf" -> "windows/metafile"
                    ".aip" -> "text/x-audiosoft-intra"
                    ".ksh" -> "text/x-script"
                    ".mcp" -> "application/netmc"
                    ".ip" -> "application/x-ip2"
                    ".sgml" -> "text/x-sgml"
                    ".pov" -> "model/x-pov"
                    ".crl" -> "application/pkix-crl"
                    ".rgb" -> "image/x-rgb"
                    ".uni" -> "text/uri-list"
                    ".hlb" -> "text/x-script"
                    ".uil" -> "text/x-uil"
                    ".asm" -> "text/x-asm"
                    ".sl" -> "application/x-seelogo"
                    ".rnx" -> "application/vnd"
                    ".dif" -> "video/x-dv"
                    ".dv" -> "video/x-dv"
                    ".chat" -> "application/x-chat"
                    ".it" -> "audio/it"
                    ".xl" -> "application/excel"
                    ".wri" -> "application/x-wri"
                    ".wml" -> "text/vnd"
                    ".sol" -> "application/solids"
                    ".asp" -> "text/asp"
                    ".xm" -> "audio/xm"
                    ".jfif" -> "image/pjpeg"
                    ".nif" -> "image/x-niff"
                    ".fmf" -> "video/x-atomic3d-feature"
                    ".iv" -> "application/x-inventor"
                    ".mht" -> "message/rfc822"
                    ".roff" -> "application/x-troff"
                    ".stl" -> "application/x-navistyle"
                    ".talk" -> "text/x-speech"
                    ".set" -> "application/set"
                    ".wrl" -> "x-world/x-vrml"
                    ".jam" -> "audio/x-jam"
                    ".abc" -> "text/vnd"
                    ".jpe" -> "image/pjpeg"
                    ".sv4crc" -> "application/x-sv4crc"
                    ".voc" -> "audio/x-voc"
                    ".m2a" -> "audio/mpeg"
                    ".nsc" -> "application/x-conference"
                    ".help" -> "application/x-helpfile"
                    ".hgl" -> "application/vnd"
                    ".crt" -> "application/x-x509-user-cert"
                    ".xif" -> "image/vnd"
                    ".pcl" -> "application/x-pcl"
                    ".log" -> "text/plain"
                    ".jpg" -> "image/pjpeg"
                    ".pre" -> "application/x-freelance"
                    ".ustar" -> "multipart/x-ustar"
                    ".bz2" -> "application/x-bzip2"
                    ".moov" -> "video/quicktime"
                    ".dxf" -> "image/x-dwg"
                    ".stp" -> "application/step"
                    ".conf" -> "text/plain"
                    ".ice" -> "x-conference/x-cooltalk"
                    ".asx" -> "video/x-ms-asf-plugin"
                    ".la" -> "audio/x-nspaudio"
                    ".unv" -> "application/i-deas"
                    ".boo" -> "application/book"
                    ".ima" -> "application/x-ima"
                    ".dir" -> "application/x-director"
                    ".frl" -> "application/freeloader"
                    ".hlp" -> "application/x-winhelp"
                    ".tar" -> "application/x-tar"
                    ".wb1" -> "application/x-qpro"
                    ".evy" -> "application/x-envoy"
                    ".gif" -> "image/gif"
                    ".jav" -> "text/x-java-source"
                    ".pct" -> "image/x-pict"
                    ".mpa" -> "video/mpeg"
                    ".xdr" -> "video/x-amt-demorun"
                    ".bm" -> "image/bmp"
                    ".mpc" -> "application/x-project"
                    ".qif" -> "image/x-quicktime"
                    ".gl" -> "video/x-gl"
                    ".gsd" -> "audio/x-gsm"
                    ".wrz" -> "x-world/x-vrml"
                    ".pcx" -> "image/x-pcx"
                    ".jps" -> "image/x-jps"
                    ".mpe" -> "video/mpeg"
                    ".src" -> "application/x-wais-source"
                    ".man" -> "application/x-troff-man"
                    ".wpd" -> "application/x-wpwin"
                    ".dxr" -> "application/x-director"
                    ".mpg" -> "video/mpeg"
                    ".vew" -> "application/groupwise"
                    ".ico" -> "image/x-icon"
                    ".wq1" -> "application/x-lotus"
                    ".scm" -> "video/x-scm"
                    ".map" -> "application/x-navimap"
                    ".nix" -> "application/x-mix-transfer"
                    ".vos" -> "video/vosaic"
                    ".boz" -> "application/x-bzip2"
                    ".jut" -> "image/jutvision"
                    ".cat" -> "application/vnd"
                    ".lha" -> "application/x-lha"
                    ".p7a" -> "application/x-pkcs7-signature"
                    ".avi" -> "video/x-msvideo"
                    ".prt" -> "application/pro"
                    ".smi" -> "application/smil"
                    ".mar" -> "text/plain"
                    ".xsr" -> "video/x-amt-showrun"
                    ".tiff" -> "image/x-tiff"
                    ".hqx" -> "application/x-mac-binhex40"
                    ".p7c" -> "application/x-pkcs7-mime"
                    ".lma" -> "audio/x-nspaudio"
                    ".swf" -> "application/x-shockwave-flash"
                    ".cpp" -> "text/x-c"
                    ".vcd" -> "application/x-cdlink"
                    ".gsm" -> "audio/x-gsm"
                    ".m2v" -> "video/mpeg"
                    ".ltx" -> "application/x-latex"
                    ".vox" -> "audio/voxware"
                    ".xla" -> "application/x-msexcel"
                    ".ppa" -> "application/vnd"
                    ".aifc" -> "audio/x-aiff"
                    ".hta" -> "application/hta"
                    ".bz" -> "application/x-bzip"
                    ".omcd" -> "application/x-omcdatamaker"
                    ".mpp" -> "application/vnd"
                    ".pict" -> "image/pict"
                    ".gsp" -> "application/x-gsp"
                    ".xlb" -> "application/x-excel"
                    ".cpt" -> "application/x-cpt"
                    ".wav" -> "audio/x-wav"
                    ".naplps" -> "image/naplps"
                    ".htc" -> "text/x-component"
                    ".xlc" -> "application/x-excel"
                    ".tif" -> "image/x-tiff"
                    ".pkg" -> "application/x-newton-compatible-pkg"
                    ".qt" -> "video/quicktime"
                    ".tcsh" -> "text/x-script"
                    ".vmd" -> "application/vocaltec-media-desc"
                    ".aiff" -> "audio/x-aiff"
                    ".xld" -> "application/x-excel"
                    ".pwz" -> "application/vnd"
                    ".gss" -> "application/x-gss"
                    ".avs" -> "video/avs-video"
                    ".gz" -> "application/x-gzip"
                    ".nvd" -> "application/x-navidoc"
                    ".mpt" -> "application/x-project"
                    ".vmf" -> "application/vocaltec-media-file"
                    ".p7m" -> "application/x-pkcs7-mime"
                    ".xbm" -> "image/xbm"
                    ".cdf" -> "application/x-netcdf"
                    ".rtf" -> "text/richtext"
                    ".mpv" -> "application/x-project"
                    ".dvi" -> "application/x-dvi"
                    ".shtml" -> "text/x-server-parsed-html"
                    ".vivo" -> "video/vnd"
                    ".etx" -> "text/x-setext"
                    ".bmp" -> "image/x-windows-bmp"
                    ".png" -> "image/png"
                    ".mpx" -> "application/x-project"
                    ".midi" -> "x-music/x-midi"
                    ".pas" -> "text/pascal"
                    ".xlk" -> "application/x-excel"
                    ".tsi" -> "audio/tsp-audio"
                    ".mid" -> "x-music/x-midi"
                    ".p7r" -> "application/x-pkcs7-certreqresp"
                    ".w60" -> "application/wordperfect6"
                    ".xll" -> "application/x-excel"
                    ".ppm" -> "image/x-portable-pixmap"
                    ".p7s" -> "application/pkcs7-signature"
                    ".htm" -> "text/html"
                    ".w61" -> "application/wordperfect6"
                    ".pko" -> "application/vnd"
                    ".xlm" -> "application/x-excel"
                    ".omc" -> "application/x-omc"
                    ".vcs" -> "text/x-vcalendar"
                    ".mif" -> "application/x-mif"
                    _ -> "text/plain"