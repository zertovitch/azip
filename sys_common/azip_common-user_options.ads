package AZip_Common.User_options is

  type Column_Integer_Array is array (Entry_topic) of Natural;
  type Column_Boolean_Array is array (Entry_topic) of Boolean;

  use_default : constant := -1;

  --  MRU (Most Recently Used) files names:
  type MRU_List is array (1 .. 9) of UTF_16_Unbounded_String;

  type Sort_Direction_Type is (Up, Down, Auto);

  no_sorting : constant := -1;  --  Column index is 0-based; -1 if no sorting column

  --  The GUI-agnostic part of user options are stored in this record
  --
  type Option_Pack_Type is record
    view_mode      : View_Mode_Type := Tree;
    tree_portion   : Float          := 0.25;
      --  Horizontal portion of the window for the tree, when view_mode = Tree
    column_width   : Column_Integer_Array :=
      --  Defaults for GWindows. May be scaled for other GUI metrics.
      (Name => 150, Modified => 120, others => 70);
    visible_column : Column_Boolean_Array :=
      (others => True);
    column_index   : Column_Integer_Array :=
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
    win_height            : Integer                 := use_default;
    MDI_childen_maximized : Boolean                 := True;
    MDI_main_maximized    : Boolean                 := False;
    mru                   : MRU_List                := (others => Null_UTF_16_Unbounded_String);
    show_passwords        : Boolean                 := False;
    sort_column           : Integer                 := no_sorting;
    sort_direction        : Sort_Direction_Type     := Up;
    ignore_extract_path   : Boolean                 := False;
    extract_directory     : UTF_16_Unbounded_String := Null_UTF_16_Unbounded_String;
    first_visit           : Boolean                 := True;
  end record;

  -------------------
  --  Persistence  --
  -------------------

  --  On Windows "vanilla", persistence is done through the registry.
  --  On Linux or GTK (any platform) it is done usually in a config file.

  generic
    with function Read_Key (topic : Wide_String) return Wide_String;
    with procedure Write_Key (topic : Wide_String; value : Wide_String);
  package Persistence is
    procedure Load (opt : out Option_Pack_Type);
    procedure Save (opt : in  Option_Pack_Type);
  end Persistence;

  generic
    with procedure String_Output (key_name : String);
  procedure Show_Persistence_Keys;

end AZip_Common.User_options;
