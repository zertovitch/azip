with Zip, UnZip;

package AZip_Common.Operations is

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (
    Add, Update, Recompress, Remove,
    Test, Extract, Search
  );

  --  Two kinds of operations: those that modify an archive (a copy needs
  --  to be created and replaces the original in the end); those that only
  --  read informations form the archive.
  --
  subtype Modifying_Operation is Archive_Operation range Add .. Remove;
  subtype Read_Only_Operation is Archive_Operation range Test .. Search;

  function Img is new Enum_Img_Mixed(Archive_Operation);

  appended    : constant:=  2;
  updated     : constant:=  2;
  success     : constant:=  1;
  only_archive: constant:=  1;
  replaced    : constant:=  1;
  nothing     : constant:=  0;
  bad_crc     : constant:= -1;
  wrong_pwd   : constant:= -2; -- After N attempts, password was still wrong
  corrupt     : constant:= -3;
  unsupported : constant:= -4;

  -- Convention for operation results set Zip_info's user_code:
  -- Add          : 0 = preserved; 1 = replaced; 2 = appended
  -- Update       : 0 = preserved; 1 = file only in archive; 2 = updated
  -- Search       : number of strings found, or 1 for file
  --                     name found (no text search)
  -- Compare      : 0 = same; 1 = different; 2 = missing in the other archive

  function Result_message(op: Archive_Operation; code: Integer) return String;
  function Result_value(s: UTF_16_String) return Integer; -- can be a non-number

  type Color_range is new Integer range 0 .. 255;

  type RGB_type is
    record
      Red    : Color_range;
      Green  : Color_range;
      Blue   : Color_range;
    end record;

  white: constant RGB_type:= (others => Color_range'Last);
  green: constant RGB_type:=
    (Red => 0, Green => (Color_range'Last * 3) / 4, Blue => 0);
  yellow: constant RGB_type:=
    (Red   => (Color_range'Last * 4) / 4,
     Green => (Color_range'Last * 4) / 5,
     Blue  => 0);

  procedure Result_color(
    op        : Archive_Operation;
    code      : Integer;
    max_code  : Integer;
    color     : out RGB_type;
    intensity : out Float      --  Useful for setting a font black or white given the background
  );

  type Entry_Operation is (
    -- Operations related to "Add"
    Append,  -- file is not in original archive and has to be added to new one
    Replace, -- file replaces an entry in original archive
    Recompress,
    -- Operations related to "Remove"
    Skip,    -- file is in original archive but won't be copied (-> "deleted")
    -- Neutral (happens with both "Add" and "Remove")
    Copy,    -- file is in original archive and copied into new one
    -- Read-Only operations
    Test,
    Extract,
    Search
  );

  function Description(
    e_op      : Entry_Operation;
    a_op      : Archive_Operation;
    skip_hint : Boolean
  )
  return UTF_16_String;

  type Name_descriptor is record
    str : UTF_16_Unbounded_String;
    sep : Natural:= 0;
    -- if sep > 0, it indicates, for an external file name, where to put
    -- the separation for the portion of the path to be matched with zip
    -- entries.
    -- Example:
    --
    -- I drop the folder "jaja" into an AZip archive window.
    -- jaja is located here: d:\ada\azip\gwindows\test .
    -- then the function Expand_folders will give many names, like:
    -- d:\ada\azip\gwindows\test\jaja\javax\swing\JProgressBar.class
    -- sep indicates this point ^
    -- the Zip entry to be replaced or appended will be:
    -- jaja\javax\swing\JProgressBar.class
  end record;

  type Name_list is array(Positive range <>) of Name_descriptor;

  -- Replace any folder name by the names of files it contains
  -- including those of subfolders, recursively.
  --
  function Expand_folders(l: Name_list) return Name_list;

  generic
    with procedure Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : UTF_16_String;
      operation             : Entry_Operation;
      comment_1, comment_2  : String; -- e.g. #found so far, time elpased,...
      skip_hint             : Boolean;
      user_abort            : out Boolean
    );
    with procedure Change_password(
      entry_name : in     UTF_16_String;
      password   : in out Unbounded_Wide_String;
      cancelled  :    out Boolean
    );
  procedure Process_archive(
    zif             :        Zip.Zip_info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    base_folder     :        UTF_16_String;
    search_pattern  :        UTF_16_String;
    output_folder   :        UTF_16_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String;
    ignore_path     :        Boolean; -- ignore directories upon extraction
    encrypt         :        Boolean;
    max_code        :    out Integer;
    abort_operation :    out Boolean
  );

  ------------------
  -- Some goodies --
  ------------------

  procedure Copy_user_codes(from, to: Zip.Zip_info);

  procedure Set_user_codes(info: Zip.Zip_info; code: Integer);

  procedure Count_test_totals(
    archive: Zip.Zip_info;
    count_ok, count_ko, count_nt: out Natural
  );

end AZip_Common.Operations;