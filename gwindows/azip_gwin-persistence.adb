with Config, GWindows.Registry, Zip;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

package body AZip_GWin.Persistence is

  ----------------------------
  --  Registry persistence  --
  ----------------------------

  kname : constant GString := "Software\AZip";
  use GWindows.Registry;

  function Read_reg_key (topic : Wide_String) return Wide_String is
  begin
    return Get_Value (kname, topic, HKEY_CURRENT_USER);
  end Read_reg_key;

  procedure Write_reg_key (topic : Wide_String; value : Wide_String) is
  begin
    Register (kname, topic, value, HKEY_CURRENT_USER);
  end Write_reg_key;

  package Registry_persistence is new
    AZip_Common.User_options.Persistence (Read_reg_key, Write_reg_key);

  --------------------------------------
  --  Configuration file persistence  --
  --------------------------------------

  function Config_name return String is
    full : constant String := Ada.Command_Line.Command_Name;
    last : Natural := full'First - 1;
  begin
    for i in full'Range loop
      if full (i) = '\' or full (i) = '/' then
        last := i;
      end if;
    end loop;
    return full (full'First .. last) & "azip.cfg";
  end Config_name;

  azip_section : constant String := "AZip user options";

  procedure Create_new_config;

  function Read_cfg_key (topic : Wide_String) return Wide_String is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_name);
    return To_GString_From_String (cfg.Value_Of ("*", To_String (topic)));
  end Read_cfg_key;

  procedure Write_cfg_key (topic : Wide_String; value : Wide_String) is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_name);
    for attempt in 1 .. 2 loop
      begin
        cfg.Replace_Value (azip_section, To_String (topic), To_String (value));
      exception
        when Config.Location_Not_Found =>
          Create_new_config;
      end;
    end loop;
  exception
    when Ada.IO_Exceptions.Use_Error =>  --  Read-only
      null;  --  Do nothing, azip.exe and azip.cfg may be on a read-only device.
  end Write_cfg_key;

  package Configuration_persistence is new
    AZip_Common.User_options.Persistence (Read_cfg_key, Write_cfg_key);

  procedure Create_new_config is
    nf : File_Type;
  begin
    Create (nf, Out_File, Config_name);
    Put_Line(nf, ";  This is the configuration file for AZip, in the ""no trace in registry"" mode.");
    Put_Line(nf, ";  Delete this file for using the registry again.");
    Put_Line(nf, ";  NB: the settings are the same for all users.");
    Put_Line(nf, ";  AZip Web site: https://azip.sourceforge.io/");
    Put_Line(nf, ";");
    Put_Line(nf, '[' & azip_section & ']');
    for k in Configuration_persistence.Key loop
      Put_Line (nf, Configuration_persistence.Key'Image (k) & '=');
    end loop;
    Close (nf);
  end Create_new_config;

  ---------------------------------------------------------------------
  --  Persistence using either the registry or a configuration file  --
  ---------------------------------------------------------------------

  function Cfg_file_available return Boolean is
  begin
    return Zip.Exists (Config_name);
  end Cfg_file_available;

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
