--  The Zip* and UnZip* packages from the Zip-Ada open-source project
--  need to be visible to the compiler.
--  See installation instructions in the header part of the azip_gwindows.gpr file.
--
with Zip;                               use Zip;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Unbounded.Wide_Hash;

with Interfaces;

------------------------------------------------------------------
-- AZip elements that are common to all GUI systems / toolkits. --
------------------------------------------------------------------

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is (
    Name, FType, Modified, Attributes,
    Size, Packed, Ratio, Format, CRC32,
    Path, Encoding,
    Result
  );

  -------------
  -- Strings --
  -------------

  -- Internal format for AZip: UTF-16
  -- Format for file names on Open / Create operations: UTF-8
  -- In zip archives, names can be either UTF-8 or IBM 437.

  subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  function To_UTF_16(s: String; encoding: Zip_name_encoding) return UTF_16_String;

  function To_UTF_8(s: UTF_16_String) return UTF_8_String;

  function To_UTF_8(s: String; encoding: Zip_name_encoding) return UTF_8_String;

  function To_IBM_437(s: UTF_16_String) return String;
  Cannot_encode_to_IBM_437: exception;

  --------------------------------------------------------------------------
  -- Maps of paths and nodes, for navigating through paths in a tree view --
  --------------------------------------------------------------------------

  -- Find quickly a node number given a path name.

  package Path_Catalogues is new Ada.Containers.Hashed_Maps
    (Key_Type        => UTF_16_Unbounded_String,
     Element_Type    => Integer,                  -- an Item ID in any GUI system
     Hash            => Ada.Strings.Wide_Unbounded.Wide_Hash,
     Equivalent_Keys => Ada.Strings.Wide_Unbounded."="
    );

  root_key: constant UTF_16_Unbounded_String:= To_Unbounded_Wide_String("");

  -- Find quickly a path name given a node number.

  package Node_Catalogues is new Ada.Containers.Ordered_Maps
    (Key_Type        => Integer,
     Element_Type    => UTF_16_Unbounded_String
    );

  --------------------------
  -- Text display helpers --
  --------------------------

  function Image(topic: Entry_topic) return UTF_16_String;
  function Hexadecimal(x: Interfaces.Unsigned_32) return UTF_16_String;
  -- File sizes (in GB, MB, KB or bytes)
  function File_Size_Image(x: Zip.Zip_32_Data_Size_Type) return UTF_16_String;
  function File_Size_Value(s: UTF_16_String) return Zip.Zip_32_Data_Size_Type;
  -- Image with thousands separator
  function Image_1000(r: Zip.Zip_32_Data_Size_Type; separator: Wide_Character) return Wide_String;
  -- Long format: e.g. "321 MB (337'477'113 bytes)"
  function Long_file_size_image(x: Zip.Zip_32_Data_Size_Type; separator: Wide_Character) return UTF_16_String;
  function Long_file_size_image(x: Interfaces.Unsigned_64; separator: Wide_Character) return UTF_16_String;
  -- Percentages
  function Ratio_pct_Image(nom, den: Zip.Zip_32_Data_Size_Type) return UTF_16_String;
  function Ratio_pct_Image(nom, den: Interfaces.Unsigned_64) return UTF_16_String;
  function Pct_Value(s: UTF_16_String) return Natural; -- 0..100
  -- Results: see AZip_Common.Operations.

  --  "Correct" casing for <Enum>'Image
  generic
    type Enum is (<>);
  function Enum_Img_Mixed(e: Enum) return UTF_16_String;

  ---------------------
  -- Various helpers --
  ---------------------

  function Remove_path(s: UTF_16_String) return UTF_16_String;
  function Give_path(s: UTF_16_String) return UTF_16_String;
  -- s is always equal to: Give_path & Remove_path

  procedure Load_insensitive_if_possible(info: out Zip_info; from: String);

  -- This function will tell if a file is actually a Zip file.
  -- It is useful for instance when files are dropped onto AZip,
  -- to determine whether AZip has to open the files as archives,
  -- or it is meant to add the files into an archive.

  type Archive_validity is
    (valid,
     with_case_sensitive_duplicates,
     invalid,
     file_doesnt_exist
     );
  function Is_valid_Zip_archive(file_name: String) return Archive_validity;

  function Has_Zip_archive_encrypted_entries(info: Zip_info) return Boolean;

end AZip_Common;
