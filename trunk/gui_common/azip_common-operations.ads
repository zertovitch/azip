with Zip, UnZip;

package AZip_Common.Operations is

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (Add, Remove, Test, Extract, Search);

  success     : constant:=  1;
  nothing     : constant:=  0;
  bad_crc     : constant:= -1;
  wrong_pwd   : constant:= -2; -- After N attempts, password was still wrong
  corrupt     : constant:= -3;
  unsupported : constant:= -4;

  -- Convention for operation results set Zip_info's user_code:
  -- Add / Update : 1 if entry was replaced or appended, 0 otherwise
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

  procedure Result_color(
    op        : Archive_Operation;
    code      : Integer;
    max_code  : Integer;
    color     : out RGB_type;
    intensity : out Float
  );

  subtype Modifying_Operation is Archive_Operation range Add .. Remove;
  subtype Read_Only_Operation is Archive_Operation range Test .. Search;

  type Entry_Operation is (
    -- Operations related to "Add"
    Append,  -- file is not in original archive and has to be added to new one
    Replace, -- file replaces an entry in original archive
    -- Operations related to "Remove"
    Skip,    -- file is in original archive but won't be copied (-> "deleted")
    -- Neutral (happens with both "Add" and "Remove")
    Copy,    -- file is in original archive and copied into new one
    -- Read-Only operations
    Test,
    Extract,
    Search
  );

  function Description(op: Entry_Operation; skip_hint: Boolean) return UTF_16_String;

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
    -- then the function Expand_folder will give many names, like:
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
      skip_hint             : Boolean;
      user_abort            : out Boolean
    );
    with procedure Change_password(
      entry_name : in UTF_16_String;
      password   : in out Unbounded_Wide_String
    );
  procedure Process_archive(
    zif             :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    base_folder     :        UTF_16_String;
    search_pattern  :        UTF_16_String;
    output_folder   :        UTF_16_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String;
    max_code        :    out Integer
  );

  ------------------
  -- Some goodies --
  ------------------

  procedure Copy_user_codes(from, to: Zip.Zip_info);

end AZip_Common.Operations;
