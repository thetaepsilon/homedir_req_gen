module Main where
-- A systemd.generator(1) which parses /etc/passwd,
-- and (for select users) creates drop-in fragments for their systemd user services,
-- causing the user's home directory to be required and ordered before the user manager.
-- useful if a user's home requires special mounting,
-- and it is desired this happens asynchronously on-demand.
-- this program is released into the public domain;
-- please see the LICENSE file for details.

import User.Ds2.Systemd.DependsOnHomedir (createHomedirDropIn)
import User.Ds2.ConfigFiles.Passwd (parseEtcPasswd, PasswdEntry, filterUsername)
import User.Ds2.Systemd.Types (DropInFragment)
import User.Ds2.SetMisc (isInSet)
import User.Ds2.Systemd.Generator (writeDropInFragment)
import Data.Set (Set, fromList)
import Data.Foldable (sequence_)
import System.IO (FilePath, stderr, hPutStrLn)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.Environment (getArgs)


-- we don't want to apply the dependency to all users,
-- just those we configure in a config file;
-- "homedir" dependencies for system services might mess things up.
-- So, we get a list of users to apply to
-- (currently /etc/passwd.homedirgen.lst),
-- and turn that into a map predicate - "is this user in the set?"
-- we then use that predicate with the PasswdEntry structures
-- produced in a list from parsing /etc/passwd,
-- to filter out the entries we do not want.
-- then, we produce the systemd fragments for just those entries,
-- and right at the end is the imperative logic to write them out.
createSetPredicate :: Ord a => [a] -> a -> Bool
createSetPredicate srclist item =
  let set = fromList srclist
  in isInSet set item

-- running the filter over usernames...
filterUsers :: (String -> Bool) -> [PasswdEntry] -> [PasswdEntry]
filterUsers userpred input =
  let predicate = filterUsername userpred
  in filter predicate input

filterUsersFromList :: [String] -> [PasswdEntry] -> [PasswdEntry]
filterUsersFromList userlist entries =
  let setp = createSetPredicate userlist
  in filterUsers setp entries




-- IO action to read /etc/passwd once user list is obtained,
-- then extract the entries we care about, and apply fragment transformation
getActiveEntries :: [String] -> IO [DropInFragment]
getActiveEntries userlist = do
  file <- readFile "/etc/passwd"
  let parsed = parseEtcPasswd file
  let active = filterUsersFromList userlist parsed
  let fragments = fmap createHomedirDropIn active
  return fragments

-- run through a list of drop-in fragments and write them all out.
-- also needs the prefix directory FilePath.
writeActiveFragments :: FilePath -> [DropInFragment] -> IO ()
writeActiveFragments basedir fragments = do
  let f = writeDropInFragment basedir
  sequence_ (fmap f fragments)

-- obtain the list of users to apply the dependency to.
-- for now, this is just a hardcoded location - TODO environment variable?
getActiveUsers :: IO [String]
getActiveUsers = do
  file <- readFile "/etc/passwd.homedirgen.lst"
  return (lines file)

-- run the whole show when we know the directory to target.
run :: FilePath -> IO ExitCode
run basedir = do
  users <- getActiveUsers
  entries <- getActiveEntries users
  writeActiveFragments basedir entries
  return ExitSuccess

-- handle argument count checking for ease of use's sake.
-- will print an error message to stderr in the non-matching case,
-- otherwise will run the main logic.
debug :: String -> IO ()
debug msg = hPutStrLn stderr ("# " ++ msg)
checkArgsAndRun :: [String] -> IO ExitCode
checkArgsAndRun (normal:early:late) = run normal
checkArgsAndRun lst = do
  debug "usage: homedir-req-generator normaldir earlydir latedir"
  debug "earlydir and latedir are not used but are required for systemd.generator."
  return (ExitFailure 1)

-- and finally...
main :: IO ()
main = do
  args <- getArgs
  code <- checkArgsAndRun args
  exitWith code

