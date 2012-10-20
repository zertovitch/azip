with Zip.Create, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;

package body AZip_Common is

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

  function Remove_path(s: String) return String is
    i: Positive;
  begin
    if s = "" then
      return "";
    else
      i:= s'First;
      for j in s'Range loop
        if s(j)= '/' or s(j)= '\' then
          i:= j+1;
        end if;
      end loop;
    end if;
    return s(i..s'Last);
  end Remove_path;

  -- Add or remove entries to an archive

  procedure Modify_Archive(
    zif         : Zip.Zip_Info;
    operation   : Archive_Operation;
    file_names  : Name_list;
    base_folder : String
  )
  is
    new_zip: Zip.Create.Zip_Create_info;
    fzs: aliased Zip_Streams.File_Zipstream;
    percents_done: Natural:= 0;
    --
    use Zip.Create;
    --
    procedure Action(
      name             : String; -- 'name' is compressed entry's name
      file_index       : Positive;
      comp_size        : Zip.File_size_type;
      uncomp_size      : Zip.File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Zip.Time;
      method           : Zip.PKZip_method;
      unicode_file_name: Boolean;
      read_only        : Boolean
    )
    is
      match: Boolean:= False;
    begin
      for i in file_names'Range loop -- !! use hashed maps either
        if file_names(i).name = name and
           file_names(i).utf_8 = unicode_file_name
        then
          match:= True;
        end if;
      end loop;
      if match then
        case operation is
          when Add =>
            Feedback(percents_done, name, unicode_file_name, Replace);
            Add_File(
              Info               => new_zip,
              Name               => name,
              Name_in_archive    => Remove_path(name),
              Delete_file_after  => False,
              Name_UTF_8_encoded => unicode_file_name
            );
          when Remove =>
            Feedback(percents_done, name, unicode_file_name, Skip);
        end case;
      else
        Feedback(percents_done, name, unicode_file_name, Copy);
        null; -- !! copy compressed entry (preserve) !!
      end if;
    end Action;

  procedure Morph_archive is new Zip.Traverse_verbose(Action);

  begin
    Create(new_zip, fzs'Unchecked_Access, "!!temp!!.zip");
    Morph_archive(zif);
    -- Almost done...
    case operation is
      when Add =>
        for i in file_names'Range loop -- !! use hashed maps either
          if not Zip.Exists(
            zif,
            To_String(file_names(i).name),
            case_sensitive => True -- !! system-dependent!...
          )
          then
            Feedback(percents_done, To_String(file_names(i).name), file_names(i).utf_8, Append);
            Add_File(
              Info               => new_zip,
              Name               => To_String(file_names(i).name),
              Name_in_archive    => Remove_path(To_String(file_names(i).name)),
              Delete_file_after  => False,
              Name_UTF_8_encoded => file_names(i).utf_8
            );
          end if;
        end loop;
      when Remove =>
        null;
        -- There should be no file to be removed which is
        -- not in original archive.
    end case;
    Finish(new_zip);
    -- !! replace old archive file by new one
  end Modify_Archive;


end AZip_Common;
