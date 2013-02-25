with Zip;                               use Zip;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
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

  --------------------------
  -- Text display helpers --
  --------------------------

  function Image(topic: Entry_topic) return UTF_16_String;
  function Hexadecimal(x: Interfaces.Unsigned_32) return UTF_16_String;
  -- File sizes
  function File_Size_Image(x: Zip.File_size_type) return UTF_16_String;
  function File_Size_Value(s: UTF_16_String) return Zip.File_size_type;
  -- Percentages
  function Ratio_pct_Image(n,d: Zip.File_size_type) return UTF_16_String;
  function Pct_Value(s: UTF_16_String) return Natural; -- 0..100
  -- Results: see AZip_Common.Operations.

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

  function Is_valid_Zip_archive(file_name: String) return Boolean;

  function Has_Zip_archive_encrypted_entries(info: Zip_info) return Boolean;

end AZip_Common;
