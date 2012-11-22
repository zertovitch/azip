with Zip;

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

  ---------------------
  -- Various helpers --
  ---------------------

  -- This function will tell if a file is actually a Zip file.
  -- It is useful for instance when files are dropped onto AZip,
  -- to determine whether AZip has to open the files as archives,
  -- or it is meant to add the files into an archive.

  function Is_valid_Zip_archive(file_name: String) return Boolean;

end AZip_Common;
