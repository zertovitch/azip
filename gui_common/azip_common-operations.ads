with Zip, UnZip;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AZip_Common.Operations is

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (Add, Remove, Test, Extract, Search);

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

  type Zip_entry_name is record
    name : Unbounded_String;
    utf_8: Boolean;
    match: Natural;
    -- Search: number of strings found;
    -- Add / Update: 1 if entry was replaced, 0 otherwise
  end record;

  type Name_list is array(Positive range <>) of Zip_entry_name;

  type Name_matching_mode is (Exact, Substring);

  generic
    with procedure Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : String;
      is_UTF_8              : Boolean;
      operation             : Entry_Operation
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
    new_temp_name  :        String
  );

end AZip_Common.Operations;
