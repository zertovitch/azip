with Zip;
with Interfaces;

-- AZip elements that are common to all GUI systems / toolkits.

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is (
    Name, FType, Modified, Attributes,
    Size, Packed, Ratio, Format, CRC32, Path
  );

  function Image(topic: Entry_topic) return String;
  function Hexadecimal(x: Interfaces.Unsigned_32) return String;
  function Pretty_file_size(x: Zip.File_size_type) return String;
  function Ratio_pct(n,d: Zip.File_size_type) return String;

  type Column_width_array is array(Entry_topic) of Natural;

  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Flat;
    column_width : Column_width_array:=
      -- Defaults for GWindows. May be scaled for different GUI metrics.
      (Name => 150, Modified => 120, others => 70);
  end record;

end AZip_Common;
