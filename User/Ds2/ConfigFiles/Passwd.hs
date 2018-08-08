module User.Ds2.ConfigFiles.Passwd where

import User.Ds2.Split (split)



-- the representation of a passwd record.
newtype Username = Username String
  deriving Show

-- this is often "x" for /etc/shadow, we just ignore it
newtype PassHash = PassHash String
  deriving Show

newtype Uid = Uid Int
  deriving Show
unUid :: Uid -> Int
unUid (Uid n) = n

newtype Gid = Gid Int
  deriving Show
-- user comment and command interpreter can be empty, but we ignore these
newtype UserComment = UserComment String
  deriving Show
newtype HomeDirectory = HomeDirectory String
  deriving Show
newtype UserShell = UserShell String
  deriving Show
data PasswdEntry = PasswdEntry Username PassHash Uid Gid UserComment HomeDirectory UserShell

-- extract individiual fields as needed elsewhere
toUid :: PasswdEntry -> Uid
toUid (PasswdEntry _ _ un _ _ _ _) = un

-- rudimentary Show instance for debugging
instance Show PasswdEntry where
  show (PasswdEntry un ph uid gid comm home shell) =
    "{PasswdEntry " ++ show un ++ ", " ++ show ph ++
    ", " ++ show uid ++ ", " ++ show gid ++ ", " ++  show comm ++
    ", " ++ show home ++ ", " ++ show shell ++ "}"

-- filter predicate helper:
-- match passwd entries by their username.
filterUsername :: (String -> Bool) -> PasswdEntry -> Bool
filterUsername f (PasswdEntry (Username un) _ _ _ _ _ _) =
  f un



-- in a valid passwd file, these fields should always be present, even if blank.
-- XXX: we don't check if required fields are non-zero in size here!
parseEtcPasswdSplit :: [String] -> PasswdEntry
parseEtcPasswdSplit [_un,_ph,_uid,_gid,_comm,_home,_shell] =
  let
    un = Username _un
    ph = PassHash _ph
    uid = Uid (read _uid)
    gid = Gid (read _gid)
    comm = UserComment (_comm)
    home = HomeDirectory (_home)
    shell = UserShell (_shell)
  in PasswdEntry un ph uid gid comm home shell

parseEtcPasswdLine :: String -> PasswdEntry
parseEtcPasswdLine str = parseEtcPasswdSplit (split ':' str)

parseEtcPasswd :: String -> [PasswdEntry]
parseEtcPasswd file = map (parseEtcPasswdLine) (lines file)

