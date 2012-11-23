with Zip.Create, UnZip.Streams, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;

with Interfaces;

package body AZip_Common.Operations is

  function Result_message(op: Archive_Operation; code: Integer) return String
  is
  begin
    case op is
      when Add =>
        if code = 1 then
          return "Replaced";
        end if;
      when Remove =>
        null;
      when Test =>
        if code = -1 then
          return "Test failed";
        else
          return "OK";
        end if;
      when Extract =>
        case code is
          when -1 =>
            return "Extraction failed";
          when 1 =>
            return "Extracted";
          when others =>
            null;
        end case;
      when Search =>
        return Trim(Integer'Image(code), Left);
    end case;
    return "";
  end Result_message;

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

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

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  procedure Process_archive(
    zif            :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation      :        Archive_Operation;
    entry_name     : in out Name_list;
    name_match     :        Name_matching_mode;
    base_folder    :        String;
    search_pattern :        Wide_String;
    output_folder  :        String;
    Set_Time_Stamp :        UnZip.Set_Time_Stamp_proc;
    new_temp_name  :        String
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
      -- Call the given feedback (Windows GUI, Gtk, Lumen, iOS, console, ...)
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
    -- Taken from Find_zip
    --
    procedure Search_1_file( name: String; occ: out Natural ) is
      max: constant:= 2**10;
      str: String(1..max);  -- str(1..stl) = string to search
      stl: Natural; -- string length
      l: Character; -- last character of the search string
      use UnZip.Streams;
      f: Zipped_File_Type;
      s: Stream_Access;
      c: Character;
      -- Define a circular buffer
      siz: constant:= max;
      type Buffer_range is mod siz;
      buf: array(Buffer_range) of Character:= (others => ' ');
      i, bup: Buffer_range:= 0;
      j: Natural;
      ignore_case: constant Boolean:= True;
    begin
      stl:= 0;
      for w in search_pattern'Range loop
        str(stl):= To_Character(search_pattern(w)); -- !! lazy conversion
        stl:= stl + 1;
      end loop;
      l:= str(stl);
      occ:= 0;
      Open( f, zif, name );
      s:= Stream(f);
      while not End_of_file(f) loop
        Character'Read(s,c);
        if ignore_case then
          c:= To_Upper(c);
        end if;
        if c = l then -- last character do match, search further...
          i:= bup;
          j:= stl;
          match: loop
            i:= i-1;
            j:= j-1;
            if j = 0 then -- we survived the whole search string
              occ:= occ+1;
              exit match;
            end if;
            exit match when str(j) /= buf(i);
          end loop match;
        end if;
        buf(bup):= c;
        bup:= bup+1;
      end loop;
      Close(f);
    end Search_1_file;
    --
    -- Taken from UnZipAda
    --
    function Add_extract_directory(File_Name : String) return String is
      Directory_Separator: constant Character:= '/';
      -- '/' is also accepted by Windows
    begin
      if output_folder = "" then
        return File_Name;
      elsif output_folder(output_folder'Last) = '\' or
            output_folder(output_folder'Last) = '/'
      then
        return output_folder & File_Name;
      else
        return output_folder & Directory_Separator & File_Name;
      end if;
    end Add_extract_directory;

    Extract_FS_routines: constant UnZip.FS_routines_type:=
      ( Create_Path         => Ada.Directories.Create_Path'Access,
        Set_Time_Stamp      => Set_Time_Stamp,
        Compose_File_Name   => Add_extract_directory'Unrestricted_Access,
        others              => null
    );
    --
    use Zip.Create;
    --
    procedure Action(
      name             : String; -- 'name' is compressed entry's full name
      file_index       : Positive;
      comp_size        : Zip.File_size_type;
      uncomp_size      : Zip.File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Zip.Time;
      method           : Zip.PKZip_method;
      unicode_file_name: Boolean;
      read_only        : Boolean;
      user_code        : in out Integer
    )
    is
      pragma Unreferenced
        (comp_size, uncomp_size, crc_32, date_time, method, read_only);
      match: Boolean:= False;
      short_name: constant String:= Remove_path(name);
    begin
      user_code:= 0;
      processed_entries:= processed_entries + 1;
      archive_percents_done:= (100 * processed_entries) / total_entries;
      file_percents_done:= 0;
      --
      case name_match is
        when Exact =>
          -- !! use hashed maps either for searching !!
          for i in entry_name'Range loop
            if entry_name(i).name = name and then
              entry_name(i).utf_8 = unicode_file_name
            then
              match:= True;
              exit;
            end if;
          end loop;
        when Substring =>
          for i in entry_name'Range loop
            if Index(entry_name(i).name, name) > 0 and then
              entry_name(i).utf_8 = unicode_file_name
            then
              match:= True;
              exit;
            end if;
          end loop;
      end case;
      if operation in Read_Only_Operation and then entry_name'Length = 0 then
        match:= True; -- empty name list -> we process the whole archive
      end if;
      --
      if match then
        case operation is
          when Add =>
            current_operation:= Replace;
            current_entry_name:= U(short_name);
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
            user_code:= 1;
          when Remove =>
            Feedback(
              file_percents_done,
              archive_percents_done,
              short_name,
              unicode_file_name,
              Skip
            );
          when Test =>
            current_operation:= Test;
            current_entry_name:= U(short_name);
            is_unicode:= unicode_file_name;
            begin
              UnZip.Extract(
                from                 => zif,
                what                 => name,
                feedback             => Entry_feedback'Unrestricted_Access,
                help_the_file_exists => null,
                tell_data            => null,
                get_pwd              => null, -- !!
                options              => (UnZip.test_only => True, others => False)
              );
            exception
              when others =>
                user_code:= -1;
            end;
          when Extract =>
            current_operation:= Extract;
            current_entry_name:= U(short_name);
            is_unicode:= unicode_file_name;
            begin
              UnZip.Extract(
                from                 => zif,
                what                 => name,
                feedback             => Entry_feedback'Unrestricted_Access,
                help_the_file_exists => null, -- !!
                tell_data            => null,
                get_pwd              => null, -- !!
                options              => (others => False),
                file_system_routines => Extract_FS_routines
              );
              user_code:= 1;
            exception
              when others =>
                user_code:= -1;
            end;
          when Search =>
            if search_pattern = "" then -- just mark entries with matching names
              user_code:= 1;
            else
              -- We need to search the string in the compressed entry...
              Search_1_file(name => name, occ  => user_code);
            end if;
        end case;
      else -- archive entry name is not matched by a file name in the list
        case operation is
          when Modifying_Operation =>
            current_operation:= Copy;
            current_entry_name:= U(short_Name);
            is_unicode:= unicode_file_name;
            Zip_Streams.Set_Index(old_fzs, file_index);
            Zip.Create.Add_Compressed_Stream(
              Info     => new_zip,
              Stream   => old_fzs,
              Feedback => Entry_feedback'Unrestricted_Access
            );
          when Read_Only_Operation =>
            null; -- Nothing to do here
        end case;
      end if;
    end Action;

  procedure Traverse_archive is new Zip.Traverse_verbose(Action);

  begin -- Process_archive
    if not Zip.Is_loaded(zif) then
      return; -- we have an "null" archive (not even a file with 0 entries)
    end if;
    case operation is
      when Add =>
        total_entries:= Zip.Entries(zif) + entry_name'Length;
      when Remove | Read_Only_Operation =>
        total_entries:= Zip.Entries(zif);
    end case;
    case operation is
      when Modifying_Operation =>
        Zip_Streams.Set_Name(old_fzs, Zip.Zip_Name(zif));
        Zip_Streams.Open(old_fzs, Ada.Streams.Stream_IO.In_File);
        Create(new_zip, new_fzs'Unchecked_Access, new_temp_name);
      when Read_Only_Operation =>
        null;
        -- Read-only operation, doesn't need a new archive file;
        -- current archive opened only at entry testing / extracting
    end case;
    --------------------------------
    -- The main job is done here: --
    --------------------------------
    Traverse_archive(zif);
    --
    -- Almost done, we only need to process new entries
    --
    case operation is
      when Add =>
        for i in entry_name'Range loop -- !! use hashed maps either
          processed_entries:= processed_entries + 1;
          archive_percents_done:= (100 * processed_entries) / total_entries;
          if not Zip.Exists(zif, To_String(entry_name(i).name)) then
            -- !! name: Wide to UTF-8 !!
            current_operation:= Append;
            current_entry_name:= U(Remove_path(To_String(entry_name(i).name)));
            is_unicode:= entry_name(i).utf_8;
            Add_File(
              Info               => new_zip,
              Name               => To_String(entry_name(i).name),
              Name_in_archive    => Remove_path(To_String(entry_name(i).name)),
              Delete_file_after  => False,
              Name_UTF_8_encoded => entry_name(i).utf_8,
              Modification_time  => Zip.Convert(Modification_Time(To_String(entry_name(i).name))),
              Is_read_only       => False, -- !!
              Feedback           => Entry_feedback'Unrestricted_Access
            );
          end if;
        end loop;
      when Remove =>
        null;
        -- There should be no file to be removed which is
        -- not in original archive.
      when Read_Only_Operation =>
        null;
        -- Nothing to do after archive traversal.
    end case;
    case operation is
      when Modifying_Operation =>
        Zip_Streams.Close(old_fzs);
        Finish(new_zip);
        Delete_File(Zip.Zip_Name(zif));
        Rename(new_temp_name, Zip.Zip_Name(zif));
      when Read_Only_Operation =>
        null;
    end case;
  end Process_archive;

end AZip_Common.Operations;
