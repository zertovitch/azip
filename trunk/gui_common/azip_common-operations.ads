with Zip, UnZip;

package AZip_Common.Operations is

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (Add, Remove, Test, Extract, Search);

  success   : constant:=  1;
  nothing   : constant:=  0;
  bad_crc   : constant:= -1;
  wrong_pwd : constant:= -2; -- After N attempts, password was still wrong
  corrupt   : constant:= -3;

  -- Convention for operation results set Zip_info's user_code:
  -- Add / Update : 1 if entry was replaced or appended, 0 otherwise
  -- Search       : number of strings found, or 1 for file
  --                     name found (no text search)
  -- Compare      : 0 = same; 1 = different; 2 = missing in the other archive

  function Result_message(op: Archive_Operation; code: Integer) return String;

  type Color_range is range 0 .. 255;

  type RGB_type is
    record
      Red    : Color_range;
      Green  : Color_range;
      Blue   : Color_range;
    end record;

  function Result_color(
    op       : Archive_Operation;
    code     : Integer;
    max_code : Integer
  )
  return RGB_type;

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

  function Description(op: Entry_Operation) return String;

  type Name_list is array(Positive range <>) of UTF_16_Unbounded_String;

  type Name_matching_mode is (Exact, Substring);

  generic
    with procedure Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : String;
      entry_name_encoding   : Zip_name_encoding;
      operation             : Entry_Operation;
      user_abort            : out Boolean
    );
    with procedure Change_password(
      entry_name : in String;
      password   : in out Unbounded_Wide_String
    );
  procedure Process_archive(
    zif             :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    name_match      :        Name_matching_mode;
    base_folder     :        String;
    search_pattern  :        Wide_String;
    output_folder   :        Wide_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String;
    max_code        :    out Integer
  );

  ------------------
  -- Some goodies --
  ------------------

  function Remove_path(s: String) return String;

  procedure Copy_user_codes(from, to: Zip.Zip_info);

end AZip_Common.Operations;
