module Depot.Util.CmdLineParser 
    (cmdOpts, getHelp, CmdFlag(..))
    where
      
import Distribution.GetOpt      
      
import Data.Maybe (fromMaybe)      
    
data CmdFlag = WorkingDir String
             | LogTo String
             | HelpMsg
             | LogLevel String
             | Quiet
             | FlagPortNum String
             | HttpPortNum String --hack
               deriving (Eq, Ord, Show)

header app = "Usage: " ++ app ++ " [OPTIONS ...]"
             
options app = 
    let base = [ Option ['h','?','H'] ["help"]  (NoArg HelpMsg) "Prints this message",
                 Option ['l']         ["logto"] (ReqArg LogTo "FILE") ("Name of log file (default `"++ app ++ ".log') "),
                 Option ['q']         ["quiet"] (NoArg Quiet) "No log messages are printed to standard out",
                 Option ['d','D']     ["wd"]    (ReqArg WorkingDir "DIR") ("Name of directory to run "++ app ++ " from (default `./')"),
                 Option ['p','P']     ["port"]  (ReqArg FlagPortNum "NUMBER") ("Port number to run " ++ app ++ " on (default 2323)")
               ]
        extra = Option [] ["http-port"] (ReqArg HttpPortNum "NUMBER") "Port number to run the HTTP server on (default 8080)" --hack
    in if app == "depot-http" --ugly as SIN.
          then extra:base
          else base

                   
             

cmdOpts app argv = let opts = options app in
    case getOpt RequireOrder opts argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo (header app) opts))
    
                   

getHelp app = usageInfo (header app) (options app)
          
 