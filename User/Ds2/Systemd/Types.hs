module User.Ds2.Systemd.Types where
-- various types representing concepts related to systemd

-- Service name: we do not enforce properties on strings here
type ServiceName = String

-- Data of a unit file or fragment.
-- For now, we do not care about it's contents.
type UnitData = String
-- A drop-in fragment for a service.
-- the fragment goes in a directory so ideally it should have a unique label.
type DropInFragmentLabel = String
data DropInFragment = DropInFragment ServiceName DropInFragmentLabel UnitData

instance Show DropInFragment where
  show (DropInFragment sv label udata) =
    "{DropInFragment servicename=" ++ show sv ++
    ", dropinlabel=" ++ show label ++
    ", data=" ++ show udata ++ "}"

