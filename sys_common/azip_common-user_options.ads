package AZip_Common.User_options is

  type Column_integer_array is array(Entry_topic) of Natural;
  type Column_boolean_array is array(Entry_topic) of Boolean;

  use_default: constant:= -1;

  -- MRU (Most Recently Used) files names:
  type MRU_List is array(1..9) of UTF_16_Unbounded_String;

  type Sort_Direction_Type is (Up, Down, Auto);

  no_sorting : constant := -1;  --  Column index is 0-based; -1 if no sorting column

  -- The GUI-agnostic part of user options are stored in this record
  --
  type Option_Pack_Type is record
    view_mode      : View_Mode_Type:= Tree;
    tree_portion   : Float:= 0.25;
      -- Horizontal portion of the window for the tree, when view_mode = Tree
    column_width   : Column_integer_array:=
      -- Defaults for GWindows. May be scaled for other GUI metrics.
      (Name => 150, Modified => 120, others => 70);
    visible_column : Column_boolean_array :=
      (others => True);
    column_index   : Column_integer_array:=
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
    mru                   : MRU_List                := (others => Null_Unbounded_Wide_String);
    show_passwords        : Boolean                 := False;
    sort_column           : Integer                 := no_sorting;
    sort_direction        : Sort_Direction_Type     := Up;
    ignore_extract_path   : Boolean                 := False;
    extract_directory     : UTF_16_Unbounded_String := Null_Unbounded_Wide_String;
    first_visit           : Boolean                 := True;
  end record;

  -------------------
  --  Persistence  --
  -------------------

  --  On Windows "vanilla", persistence is done through the registry.
  --  On Linux or GTK (any platform) it is done usually in a config file.

  generic
    with function Read_key (topic: Wide_String) return Wide_String;
    with procedure Write_key (topic: Wide_String; value: Wide_String);
  package Persistence is
    procedure Load (opt: out Option_Pack_Type);
    procedure Save (opt: in  Option_Pack_Type);
    --
    --  This is not really needed in the specification for loading & saving,
    --  but useful for creating a fresh configuration file.
    --
    type Key is
      ( view_mode,
        col_width,
        col_visible,
        sort_column,
        sort_direction,
        win_left, win_top, win_width, win_height,
        maximized, children_maximized,
        tree_portion,
        mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9,
        show_passwords,
        ignore_extract_path,
        extract_directory,
        first_visit
      );
  end;

end AZip_Common.User_options;
