with Zip;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

------------------------------------------------------------------
-- AZip elements that are common to all GUI systems / toolkits. --
------------------------------------------------------------------

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is (
    Name, FType, Modified, Attributes,
    Size, Packed, Ratio, Format, CRC32, Path,
    Result -- Result of search, for instance
  );

  --------------------------
  -- Text display helpers --
  --------------------------

  function Image(topic: Entry_topic) return String;
  function Hexadecimal(x: Interfaces.Unsigned_32) return String;
  function Pretty_file_size(x: Zip.File_size_type) return String;
  function Ratio_pct(n,d: Zip.File_size_type) return String;

  type Column_width_array is array(Entry_topic) of Natural;

  -- The GUI-agnostic part of user options are stored in this record

  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Flat;
    column_width : Column_width_array:=
      -- Defaults for GWindows. May be scaled for different GUI metrics.
      (Name => 150, Modified => 120, others => 70);
  end record;

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  type Archive_Operation is (Add, Remove, Test, Extract, Search);

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
    zif            :        Zip.Zip_Info;
    operation      :        Archive_Operation;
    entry_name     : in out Name_list;
    name_match     :        Name_matching_mode;
    base_folder    :        String;
    search_pattern :        Wide_String
  );

end AZip_Common;
