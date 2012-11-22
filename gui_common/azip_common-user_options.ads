package AZip_Common.User_options is

  type Column_width_array is array(Entry_topic) of Natural;

  -- The GUI-agnostic part of user options are stored in this record

  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Flat;
    column_width : Column_width_array:=
      -- Defaults for GWindows. May be scaled for different GUI metrics.
      (Name => 150, Modified => 120, others => 70);
    MDI_childen_maximized: Boolean:= True;
    MDI_main_maximized   : Boolean:= True;
  end record;

  -----------------
  -- Persistence --
  -----------------

  -- On Windows "vanilla", it is done through the registry
  -- On Linux or Gtk (any platform) it is done usually in a config file

  generic
    with function Read_key(topic: String) return String;
    with procedure Write_key(topic: String; value: String);
  package Persistence is
    procedure Load(opt: out Option_Pack_Type);
    procedure Save(opt: in  Option_Pack_Type);
  end;

end AZip_Common.User_options;
