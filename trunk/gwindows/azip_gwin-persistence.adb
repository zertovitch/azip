with Config, GWindows.Registry, Zip;

package body AZip_GWin.Persistence is

  ----------------------------
  --  Registry persistence  --
  ----------------------------

  kname: constant GString:= "Software\AZip";
  use GWindows.Registry;

  function Read_reg_key (topic: Wide_String) return Wide_String is
  begin
    return Get_Value (kname, topic, HKEY_CURRENT_USER);
  end Read_reg_key;

  procedure Write_reg_key (topic: Wide_String; value: Wide_String) is
  begin
    Register (kname, topic, value, HKEY_CURRENT_USER);
  end Write_reg_key;

  package Registry_persistence is new
    AZip_Common.User_options.Persistence (Read_reg_key, Write_reg_key);

  --------------------------------------
  --  Configuration file persistence  --
  --------------------------------------

  cfg_file_name: constant String := "azip.cfg";  --  !! Add path

  procedure Create_new_config is
  begin
    null; --!!
  end;

  function Read_cfg_key (topic: Wide_String) return Wide_String is
    cfg: Config.Configuration;
  begin
    cfg.Init (cfg_file_name);
    return To_GString_From_String (cfg.Value_Of ("*", To_String (topic)));
  end Read_cfg_key;

  procedure Write_cfg_key (topic: Wide_String; value: Wide_String) is
    cfg: Config.Configuration;
  begin
    cfg.Init (cfg_file_name);
    for attempt in 1 .. 2 loop
      begin
        cfg.Replace_Value ("AZip user options", To_String (topic), To_String (value));
      exception
        when Config.Location_Not_Found =>
          Create_new_config;
      end;
    end loop;
    --  !! handle read-only
  end Write_cfg_key;

  package Configuration_persistence is new
    AZip_Common.User_options.Persistence (Read_cfg_key, Write_cfg_key);

  ---------------------------------------------------------------------
  --  Persistence using either the registry or a configuration file  --
  ---------------------------------------------------------------------

  function Cfg_file_available return Boolean is
  begin
    return Zip.Exists (cfg_file_name);
  end;

  procedure Load (opt: out AZip_Common.User_options.Option_Pack_Type) is
  begin
    if Cfg_file_available then
      Configuration_persistence.Load (opt);
    else
      Registry_persistence.Load (opt);
    end if;
  end Load;

  procedure Save (opt: in  AZip_Common.User_options.Option_Pack_Type) is
  begin
    if Cfg_file_available then
      Configuration_persistence.Save (opt);
    else
      Registry_persistence.Save (opt);
    end if;
  end Save;

end AZip_GWin.Persistence;
