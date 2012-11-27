with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;

with Ada.Strings.UTF_Encoding.Conversions;

package body AZip_Common is

  function To_UTF_16(s: String; encoding: Zip_name_encoding) return Wide_String
  is
  begin
    case encoding is
      when IBM_437 =>
        return To_Wide_String(s); -- !!
      when UTF_8 =>
        return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
    end case;
  end To_UTF_16;

  function To_UTF_16(s: String; is_UTF_8: Boolean) return Wide_String is
  begin
    if is_UTF_8 then
      return To_UTF_16(s, UTF_8);
    else
      return To_UTF_16(s, IBM_437);
    end if;
  end To_UTF_16;

  function To_UTF_8(s: UTF_16_String) return UTF_8_String is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
  end To_UTF_8;

  function Image(topic: Entry_topic) return String is
    u: constant String:= Entry_topic'Image(topic);
    l: constant String:= To_Lower(u);
  begin
    case topic is
      when FType =>
        return "Type";
      when CRC32 =>
        return "CRC 32";
      when others =>
        return u(u'First) & l(l'First+1..l'Last);
    end case;
  end Image;

  function Hexadecimal(x: Interfaces.Unsigned_32) return String
  is
    package MIO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);
    str: String(1..12);
  begin
    MIO.Put(str, x, 16);
    return str(Index(str,"#")+1..11);
  end Hexadecimal;

  function Pretty_file_size(x: Zip.File_size_type) return String is
    use type Zip.File_size_type;
  begin
    if x >= 1024 ** 3 then
      return Pretty_file_size(x / (1024 ** 3)) & " GB";
    elsif x >= 1024 ** 2 then
      return Pretty_file_size(x / (1024 ** 2)) & " MB";
    elsif x >= 1024 then
      return Pretty_file_size(x / 1024) & " KB";
    else
      return Trim(Zip.File_size_type'Image(x), Left);
    end if;
  end Pretty_file_size;

  function Ratio_pct(n,d: Zip.File_size_type) return String is
    use type Zip.File_size_type;
  begin
    if d = 0 then
      return "--";
    else
      return Trim(Integer'Image(Integer(100.0 * Float(n) / Float(d))), Left) & '%';
    end if;
  end Ratio_pct;

  function Is_valid_Zip_archive(file_name: String) return Boolean is
    info: Zip.Zip_info;
  begin
    Zip.Load(info, file_name);
    Zip.Delete(info);
    return True;
  exception
    when others =>
      return False;
  end Is_valid_Zip_archive;

end AZip_Common;
