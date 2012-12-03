package AZip_Common.User_options is

  type Column_integer_array is array(Entry_topic) of Natural;

  use_default: constant:= -1;

  -- MRU (Most Recently Used) files names:
  type MRU_List is array(1..9) of UTF_16_Unbounded_String;

 type Sort_Direction_Type is (Up, Down, Auto);

  -- The GUI-agnostic part of user options are stored in this record
  --
  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Flat;
    column_width : Column_integer_array:=
      -- Defaults for GWindows. May be scaled for different GUI metrics.
      (Name => 150, Modified => 120, others => 70);
    column_index : Column_integer_array:=
      (Name       => 1, -- This should never change
       FType      => 2,
       Modified   => 3,
       Attributes => 4,
       Size       => 5,
       Packed     => 6,
       Ratio      => 7,
       Format     => 8,
       CRC32      => 9,
       Path       => 10,
       Encoding   => 11,
       Result     => 12
      );
    win_left,
    win_top,
    win_width,
    win_height            : Integer := use_default;
    MDI_childen_maximized : Boolean := True;
    MDI_main_maximized    : Boolean := False;
    mru                   : MRU_List:= (others => Null_Unbounded_Wide_String);
    show_passwords        : Boolean := False;
    sort_column           : Integer := -1; -- 0-based; -1 if none
    sort_direction        : Sort_Direction_Type:= Up;
  end record;

  -----------------
  -- Persistence --
  -----------------

  -- On Windows "vanilla", it is done through the registry
  -- On Linux or Gtk (any platform) it is done usually in a config file

  generic
    with function Read_key(topic: Wide_String) return Wide_String;
    with procedure Write_key(topic: Wide_String; value: Wide_String);
  package Persistence is
    procedure Load(opt: out Option_Pack_Type);
    procedure Save(opt: in  Option_Pack_Type);
  end;

end AZip_Common.User_options;
