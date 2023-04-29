---------------------------------------------------------------------
--  This package contains elements for AZip applications that are  --
--  common to all GUI systems / toolkits.                          --
---------------------------------------------------------------------

--  The Zip* and UnZip* packages from the Zip-Ada open-source project
--  need to be visible to the compiler.
--  See for instance installation instructions in the header
--  part of the azip_gwindows.gpr file.
--
with Zip;

with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Strings.UTF_Encoding,
     Ada.Strings.Wide_Unbounded,
     Ada.Strings.Wide_Hash;

with Interfaces;

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is
    (Name, FType, Modified, Attributes,
     Size, Packed, Ratio, Format, CRC32,
     Path, Encoding,
     Result);

  -------------
  -- Strings --
  -------------

  --  Internal format for AZip: UTF-16
  --  Format for file names on Open / Create operations: UTF-8
  --  In zip archives, names can be either UTF-8 or IBM 437.

  subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;

  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
  Null_UTF_16_Unbounded_String : UTF_16_Unbounded_String renames
    Ada.Strings.Wide_Unbounded.Null_Unbounded_Wide_String;

  function To_UTF_16 (s : String; name_encoding : Zip.Zip_name_encoding) return UTF_16_String;

  function To_UTF_8 (s : UTF_16_String) return UTF_8_String;

  function To_UTF_8 (s : String; encoding : Zip.Zip_name_encoding) return UTF_8_String;

  function To_IBM_437 (s : UTF_16_String) return String;
  Cannot_encode_to_IBM_437 : exception;

  --------------------------------------------------------------------------
  -- Maps of paths and nodes, for navigating through paths in a tree view --
  --------------------------------------------------------------------------

  --  Find quickly a node number given a path name.

  type Node_ID_Type is mod 2 ** Standard'Address_Size;  --  a Node ID in any GUI system.

  package Path_Catalogues is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => UTF_16_String,
     Element_Type    => Node_ID_Type,
     Hash            => Ada.Strings.Wide_Hash,
     Equivalent_Keys => "="
    );

  root_key : constant UTF_16_String := "";

  --  Find quickly a path name given a node number.

  package Node_Catalogues is new Ada.Containers.Indefinite_Ordered_Maps
    (Key_Type        => Node_ID_Type,
     Element_Type    => UTF_16_String
    );

  --------------------------
  -- Text display helpers --
  --------------------------

  function Image (topic : Entry_topic) return UTF_16_String;
  function Hexadecimal_32 (x : Interfaces.Unsigned_32) return UTF_16_String with Inline;
  --  File sizes (in GB, MB, KB or bytes)
  function File_Size_Image (x : Zip.Zip_64_Data_Size_Type) return UTF_16_String;
  --  Image with thousands separator
  function Image_1000 (r : Zip.Zip_64_Data_Size_Type; separator : Wide_Character) return Wide_String;
  --  Long format: e.g. "321 MB (337'477'113 bytes)"
  function Long_file_size_image (x : Interfaces.Unsigned_64; separator : Wide_Character) return UTF_16_String;
  --  Percentages
  function Ratio_pct_Image (nom, den : Interfaces.Unsigned_64) return UTF_16_String;
  --  Results: see AZip_Common.Operations.

  --  "Correct" casing for <Enum>'Image
  generic
    type Enum is (<>);
  function Enum_Img_Mixed (e : Enum) return UTF_16_String;

  ---------------------
  -- Various helpers --
  ---------------------

  function Remove_path (s : UTF_16_String) return UTF_16_String;
  function Give_path (s : UTF_16_String) return UTF_16_String;
  --  s is always equal to: Give_path & Remove_path

  procedure Load_insensitive_if_possible (info : out Zip.Zip_info; from : String);

  --  This function will tell if a file is actually a Zip file.
  --  It is useful for instance when files are dropped onto AZip,
  --  to determine whether AZip has to open the files as archives,
  --  or it is meant to add the files into an archive.

  type Archive_validity is
    (valid,
     with_case_sensitive_duplicates,
     invalid,
     file_doesnt_exist
     );
  function Is_valid_Zip_archive (file_name : String) return Archive_validity;

  function Has_Zip_archive_encrypted_entries (info : Zip.Zip_info) return Boolean;

end AZip_Common;
