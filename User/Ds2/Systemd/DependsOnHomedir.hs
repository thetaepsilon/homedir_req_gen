module User.Ds2.Systemd.DependsOnHomedir where

-- creates a drop-in config fragment for systemd user managers,
-- giving them RequiresMountsFor= the home directory for that user.

-- the data structure we act upon
import User.Ds2.ConfigFiles.Passwd (
  PasswdEntry(PasswdEntry),
  HomeDirectory(HomeDirectory),
  Username(Username),
  unUid,
  toUid)

-- data structure to create
import User.Ds2.Systemd.Types (
  DropInFragment(DropInFragment))

-- create the body of the config drop-in
createHomedirFragment :: PasswdEntry -> String
createHomedirFragment (PasswdEntry (Username un) ph uid gid comm (HomeDirectory home) shell) =
  "# home directory dependency for " ++ un ++ "\n" ++
  "[Unit]\n" ++
  "RequiresMountsFor=" ++ home ++ "\n"

-- create the DropInFragment describing the user service this fragment is for
createHomedirDropIn :: PasswdEntry -> DropInFragment
createHomedirDropIn entry =
  let
    uid = show (unUid (toUid entry))
    service = "user@" ++ uid ++ ".service"
    label = "20-user-" ++ uid ++ "-homedir"
  in DropInFragment service label (createHomedirFragment entry)


