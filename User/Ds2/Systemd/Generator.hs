module User.Ds2.Systemd.Generator where
-- functions and IO actions useful for systemd generator programs

import User.Ds2.Systemd.Types (DropInFragment(DropInFragment))
import System.IO (FilePath)
import System.Directory (createDirectoryIfMissing)


-- IO action to write out a drop-in fragment to a generator directory.
-- the appropriate foo.service.d directory is created if it doesn't exist,
-- then a .conf fragment is created with the unit file contents.
writeDropInFragment :: FilePath -> DropInFragment -> IO ()
writeDropInFragment basedir (DropInFragment sv label udata) = do
  let targetdir = basedir ++ "/" ++ sv ++ ".d"
  createDirectoryIfMissing False targetdir
  let targetf = targetdir ++ "/" ++ label ++ ".conf"
  writeFile targetf udata

