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
      --  ^ Horizontal portion of the window for the tree, when view_mode = Tree

    column_width   : Column_Integer_Array :=
      --  Defaults for GWindows. May be scaled for other GUI metrics.
      (Name => 150, Modified => 120, others => 70);

    visible_column : Column_Boolean_Array :=
      (others => True);

    column_index   : Column_Integer_Array :=
      (Name       => 1,  --  This should never change
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
       Result     => 12);

    win_left,
    win_top,
    win_width,
    win_height                  : Integer                 := use_default;
    MDI_childen_maximized       : Boolean                 := True;
    MDI_main_maximized          : Boolean                 := False;
    mru                         : MRU_List                := (others => Null_UTF_16_Unbounded_String);
    show_passwords              : Boolean                 := False;
    sort_column                 : Integer                 := no_sorting;
    sort_direction              : Sort_Direction_Type     := Up;
    ignore_extract_path         : Boolean                 := False;
    suggested_extract_directory : UTF_16_Unbounded_String := Null_UTF_16_Unbounded_String;
    temp_directory              : UTF_16_Unbounded_String := Null_UTF_16_Unbounded_String;
    first_visit                 : Boolean                 := True;
    backup_recomp               : Boolean                 := False;
    backup_update               : Boolean                 := False;
  end record;

  function Is_Temp_Directory_Valid (opt : Option_Pack_Type) return Boolean;

  -------------------
  --  Persistence  --
  -------------------

  type Persistence_Key is
    (view_mode,
     --
     col_width_name,
     col_width_ftype,
     col_width_modified,
     col_width_attributes,
     col_width_size,
     col_width_packed,
     col_width_ratio,
     col_width_format,
     col_width_crc32,
     col_width_path,
     col_width_encoding,
     col_width_result,
     --
     col_visible_name,
     col_visible_ftype,
     col_visible_modified,
     col_visible_attributes,
     col_visible_size,
     col_visible_packed,
     col_visible_ratio,
     col_visible_format,
     col_visible_crc32,
     col_visible_path,
     col_visible_encoding,
     col_visible_result,
     --
     sort_column,
     sort_direction,
     win_left, win_top, win_width, win_height,
     maximized, children_maximized,
     tree_portion,
     mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9,
     show_passwords,
     ignore_extract_path,
     extract_directory,
     temp_directory,
     first_visit,
     backup_recomp,
     backup_update);

  subtype Col_Width_Key   is Persistence_Key range col_width_name .. col_width_result;
  subtype Col_Visible_Key is Persistence_Key range col_visible_name .. col_visible_result;

  --  On Windows "vanilla", persistence is done through the registry.
  --  On Linux or GTK (any platform) it is done usually in a config file.

  generic
    with function Read_Key (key : Persistence_Key) return Wide_String;
    with procedure Write_Key (key : Persistence_Key; value : Wide_String);
  package Persistence is
    procedure Load (opt : out Option_Pack_Type);
    procedure Save (opt : in  Option_Pack_Type);
  end Persistence;

  generic
    with procedure String_Output (key_name : String);
  procedure Show_Persistence_Keys;

end AZip_Common.User_options;
