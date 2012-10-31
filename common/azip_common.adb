with Zip.Create, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

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

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;


  -- Blocking, visible processing of an archive

  procedure Process_archive(
    zif         : Zip.Zip_Info;
    operation   : Archive_Operation;
    file_names  : Name_list;
    base_folder : String
  )
  is
    new_zip: Zip.Create.Zip_Create_info;
    new_fzs: aliased Zip_Streams.File_Zipstream;
    old_fzs: aliased Zip_Streams.File_Zipstream;
    file_percents_done: Natural:= 0;
    archive_percents_done: Natural:= 0;
    processed_entries, total_entries: Natural:= 0;
    current_entry_name: Unbounded_String;
    is_unicode: Boolean;
    current_operation: Entry_operation;
    --
    procedure Entry_feedback(
      percents_done:  in Natural;  -- %'s completed
      entry_skipped:  in Boolean;  -- indicates one can show "skipped", no %'s
      user_abort   : out Boolean   -- e.g. transmit a "click on Cancel" here
    )
    is
    begin
      if entry_skipped then
        file_percents_done:= 0;
      else
        file_percents_done:= percents_done;
      end if;
      Feedback(
        file_percents_done,
        archive_percents_done,
        S(current_entry_name),
        is_unicode,
        current_operation
      );
      user_abort:= False; -- !!
    end Entry_feedback;

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
      pragma Unreferenced
        (file_index, comp_size, uncomp_size,
         crc_32, date_time, method, read_only);
      match: Boolean:= False;
      short_name: constant String:= Remove_path(name);
    begin
      processed_entries:= processed_entries + 1;
      archive_percents_done:= (100 * processed_entries) / total_entries;
      file_percents_done:= 0;
      --
      for i in file_names'Range loop -- !! use hashed maps either for searching
        if file_names(i).name = name and
           file_names(i).utf_8 = unicode_file_name
        then
          match:= True;
        end if;
      end loop;
      --
      if match then
        case operation is
          when Add =>
            current_operation:= Replace;
            current_entry_name:= U(short_Name);
            is_unicode:= unicode_file_name;
            Add_File(
              Info               => new_zip,
              Name               => name,
              Name_in_archive    => short_name,
              Delete_file_after  => False,
              Name_UTF_8_encoded => unicode_file_name,
              Modification_time  => Zip.Convert(Modification_Time(name)),
              Is_read_only       => False, -- !!
              Feedback           => Entry_feedback'Unrestricted_Access
            );
          when Remove =>
            Feedback(
              file_percents_done,
              archive_percents_done,
              short_name,
              unicode_file_name,
              Skip
            );
        end case;
      else -- archive entry name is not matched by a file name in the list
        case operation is
          when Add | Remove =>
            current_operation:= Copy;
            current_entry_name:= U(short_Name);
            is_unicode:= unicode_file_name;
            Zip.Create.Add_Compressed_Stream(
              Info     => new_zip,
              Stream   => old_fzs,
              Feedback => Entry_feedback'Unrestricted_Access
            );
        end case;
      end if;
    end Action;

  procedure Traverse_archive is new Zip.Traverse_verbose(Action);

  begin
    case operation is
      when Add =>
        total_entries:= Zip.Entries(zif) + file_names'Length;
      when Remove =>
        total_entries:= Zip.Entries(zif);
    end case;
    Zip_Streams.Set_Name(old_fzs, Zip.Zip_Name(zif));
    Zip_Streams.Open(old_fzs, Ada.Streams.Stream_IO.In_File);
    case operation is
      when Add | Remove =>
        Create(new_zip, new_fzs'Unchecked_Access, "!!.zip");
    end case;
    -- Main job:
    Traverse_archive(zif);
    -- Almost done...
    case operation is
      when Add =>
        for i in file_names'Range loop -- !! use hashed maps either
          processed_entries:= processed_entries + 1;
          archive_percents_done:= (100 * processed_entries) / total_entries;
          if not Zip.Exists(
            zif,
            To_String(file_names(i).name),
            case_sensitive => True -- !! system-dependent!...
          )
          then
            current_operation:= Append;
            current_entry_name:= U(Remove_path(To_String(file_names(i).name)));
            is_unicode:= file_names(i).utf_8;
            Add_File(
              Info               => new_zip,
              Name               => To_String(file_names(i).name),
              Name_in_archive    => Remove_path(To_String(file_names(i).name)),
              Delete_file_after  => False,
              Name_UTF_8_encoded => file_names(i).utf_8,
              Modification_time  => Zip.Convert(Modification_Time(To_String(file_names(i).name))),
              Is_read_only       => False, -- !!
              Feedback           => Entry_feedback'Unrestricted_Access
            );
          end if;
        end loop;
      when Remove =>
        null;
        -- There should be no file to be removed which is
        -- not in original archive.
    end case;
    Zip_Streams.Close(old_fzs);
    case operation is
      when Add | Remove =>
        Finish(new_zip);
        -- !! replace old archive file by new one
    end case;
  end Process_archive;


end AZip_Common;
