-----------------------------------------------------------------------------------
--  Persistence using either the registry or a configuration file, if available  --
-----------------------------------------------------------------------------------

with AZip_Common.User_options;

package AZip_GWin.Persistence is

  function Cfg_file_available return Boolean;
  --  ^ When True, we are in Stealth Mode and don't want to
  --    leave any trace in the registry!

  procedure Load (opt : out AZip_Common.User_options.Option_Pack_Type);
  procedure Save (opt : in  AZip_Common.User_options.Option_Pack_Type);

end AZip_GWin.Persistence;
