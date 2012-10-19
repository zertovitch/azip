with Zip;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

-- AZip elements that are common to all GUI systems / toolkits.

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is (
    Name, FType, Modified, Attributes,
    Size, Packed, Ratio, Format, CRC32, Path
  );

  -- Text display helpers

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

  -- Add or remove entries to an archive

  type Archive_Operation is (Add, Remove);
  type Entry_Operation is (
    -- Operations related to "Add"
    Append,  -- file is not in original archive and has to be added to new one
    Replace, -- file replaces an entry in original archive
    -- Operations related to "Remove"
    Skip,    -- file is in original archive but won't be copied (-> "deleted")
    -- Neutral (happens with both "Add" and "Remove")
    Copy     -- file is in original archive and copied into new one
  );

  type Zip_entry_name is record
    name : Unbounded_String;
    utf_8: Boolean;
  end record;

  type Name_list is array(Natural range <>) of Zip_entry_name;
  -- !! should use hashed maps for quick search

  generic
    with procedure Feedback(
      percents_done         : Natural;
      entry_being_processed : String;
      is_UTF_8              : Boolean;
      operation             : Entry_Operation
    );
  procedure Modify_Archive(
    zif         : Zip.Zip_Info;
    operation   : Archive_Operation;
    file_names  : Name_list;
    base_folder : String
  );

end AZip_Common;
