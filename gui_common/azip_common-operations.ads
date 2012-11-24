with Zip, UnZip;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AZip_Common.Operations is

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (Add, Remove, Test, Extract, Search);

  success : constant:=  1;
  nothing : constant:=  0;
  bad_crc : constant:= -1;

  -- Convention for operation results set Zip_info's user_code:
  -- Add / Update : 1 if entry was replaced or appended, 0 otherwise
  -- Search       : number of strings found, or 1 for file
  --                     name found (no text search)
  -- Compare      : 0 = same; 1 = different; 2 = missing in the other archive

  function Result_message(op: Archive_Operation; code: Integer) return String;

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

  type Zip_entry_name is record
    name : Unbounded_String;
    utf_8: Boolean;
  end record;

  type Name_list is array(Positive range <>) of Zip_entry_name;

  type Name_matching_mode is (Exact, Substring);

  generic
    with procedure Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : String;
      is_UTF_8              : Boolean;
      operation             : Entry_Operation;
      user_abort            : out Boolean
    );
  procedure Process_archive(
    zif            :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation      :        Archive_Operation;
    entry_name     : in out Name_list;
    name_match     :        Name_matching_mode;
    base_folder    :        String;
    search_pattern :        Wide_String;
    output_folder  :        String;
    Set_Time_Stamp :        UnZip.Set_Time_Stamp_proc;
    new_temp_name  :        String;
    Name_conflict  :        UnZip.Resolve_conflict_proc;
    Change_password:        UnZip.Get_password_proc
  );

  ------------------
  -- Some goodies --
  ------------------

  function Remove_path(s: String) return String;

  procedure Copy_user_codes(from, to: Zip.Zip_info);

end AZip_Common.Operations;
