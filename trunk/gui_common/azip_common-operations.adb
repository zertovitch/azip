with Zip.Create, Zip.Compress, UnZip.Streams, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with Interfaces;

package body AZip_Common.Operations is

  function Result_message(op: Archive_Operation; code: Integer) return String
  is
  begin
    -- Following codes have a single explanation over all operations
    case code is
      when wrong_pwd =>
        return "Password wrong" & Integer'Image(UnZip.tolerance_wrong_password) & " times";
      when others =>
        null;
    end case;
    --
    case op is
      when Add =>
        if code = success then
          return "Replaced";
        end if;
      when Remove =>
        null;
      when Test =>
        case code is
          when success =>
            return "OK";
          when bad_crc =>
            return "CRC test failed";
          when nothing =>
            return "not tested";
          when others =>
            null;
        end case;
      when Extract =>
        case code is
          when bad_crc =>
            return "CRC test failed on extraction";
          when success =>
            return "Extracted";
          when others =>
            null;
        end case;
      when Search =>
        return Trim(Integer'Image(code), Left);
    end case;
    return "";
  end Result_message;

  function Description(op: Entry_Operation) return String is
  begin
    case op is
      when Append =>
        return "Appending...";
      when Replace =>
        return "Replacing...";
      when Copy =>
        return "Copying...";
      when Skip =>
        return "Skipping...";
      when Test =>
        return "Testing...";
      when Extract =>
        return "Extracting...";
      when Search =>
        return "Searching...";
    end case;
  end Description;

  procedure Copy_user_codes(from, to: Zip.Zip_info) is
    procedure Copy_user_code(
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
    pragma Unreferenced (file_index, comp_size, uncomp_size, crc_32, date_time, method, unicode_file_name, read_only);
    begin
      Zip.Set_user_code(to, name, user_code);
    end Copy_user_code;
    procedure Do_it is new Zip.Traverse_verbose(Copy_user_code);
  begin
    Do_it(from);
  end Copy_user_codes;

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
    zif             :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    name_match      :        Name_matching_mode;
    base_folder     :        String;
    search_pattern  :        Wide_String;
    output_folder   :        Wide_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String
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
      -- Call the given non-portable feedback box
      -- (Windows GUI, Gtk, Lumen, iOS, console, ...)
      Feedback(
        file_percents_done,
        archive_percents_done,
        S(current_entry_name),
        is_unicode,
        current_operation,
        user_abort
      );
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
      ignore_case: constant Boolean:= True; -- !! as option
    begin
      -- First we copy the string
      -- !! wide or not : what to do ? --
      stl:= 0;
      for w in search_pattern'Range loop
        stl:= stl + 1;
        str(stl):= To_Character(search_pattern(w)); -- !! lazy conversion
        if ignore_case then
          str(stl):= To_Upper(str(stl));
        end if;
      end loop;
      l:= str(stl);
      occ:= 0;
      for attempt in 1..UnZip.tolerance_wrong_password loop
        begin
          Open( f, zif, name, To_String(To_Wide_String(password)) );
          exit;
        exception
          when UnZip.Wrong_password =>
            if attempt = UnZip.tolerance_wrong_password then
              raise;
            end if;
            Change_password(To_String(current_entry_name), password);
        end;
      end loop;
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
        return To_UTF_8(output_folder) & File_Name;
      else
        return To_UTF_8(output_folder) & Directory_Separator & File_Name;
      end if;
    end Add_extract_directory;
    --
    procedure Get_password_internal(pwd: out Unbounded_String) is
      use UnZip;
    begin
      Change_password(To_String(current_entry_name), password);
      -- persistence over entries + wide strings...
      pwd:=  To_Unbounded_String(To_String(To_Wide_String(password)));
    end Get_password_internal;
    --
    Extract_FS_routines: constant UnZip.FS_routines_type:=
      ( Create_Path         => Ada.Directories.Create_Path'Access,
        Set_Time_Stamp      => Set_Time_Stamp,
        Compose_File_Name   => Add_extract_directory'Unrestricted_Access,
        others              => null
    );
    --
    use Zip.Create, UnZip;
    abort_rest_of_operation: Boolean:= False;
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
      dummy_user_abort, conflict: Boolean;
    begin
      user_code:= nothing;
      if abort_rest_of_operation then
        return;
      end if;
      processed_entries:= processed_entries + 1;
      archive_percents_done:= (100 * processed_entries) / total_entries;
      file_percents_done:= 0;
      --
      case name_match is
        when Exact =>
          -- !! use hashed maps either for searching
          for i in entry_name'Range loop
            if entry_name(i) = To_UTF_16(name, unicode_file_name) then
              match:= True;
              exit;
            end if;
          end loop;
        when Substring =>
          for i in entry_name'Range loop
            declare
              pattern: constant UTF_16_String:= To_Wide_String(entry_name(i));
            begin
              if pattern = "" or else
                Index(To_UTF_16(short_name, unicode_file_name), pattern) > 0
              then
                match:= True;
                exit;
              end if;
            end;
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
            begin
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
              user_code:= success;
            exception
              when Zip.Compress.User_Abort =>
                abort_rest_of_operation:= True;
            end;
          when Remove =>
            Feedback(
              file_percents_done,
              archive_percents_done,
              short_name,
              unicode_file_name,
              Skip,
              dummy_user_abort
            );
          when Test =>
            current_operation:= Test;
            current_entry_name:= U(short_name);
            is_unicode:= unicode_file_name;
            begin
              Extract(
                from                 => zif,
                what                 => name,
                feedback             => Entry_feedback'Unrestricted_Access,
                help_the_file_exists => null,
                tell_data            => null,
                get_pwd              => Get_password_internal'Unrestricted_Access,
                options              => (test_only => True, others => False),
                password             => To_String(To_Wide_String(password))
              );
              user_code:= success;
            exception
              when UnZip.User_Abort =>
                abort_rest_of_operation:= True;
              when UnZip.CRC_Error =>
                user_code:= bad_crc;
              when UnZip.Wrong_password =>
                user_code:= wrong_pwd;
                abort_rest_of_operation:= True;
            end;
          when Extract =>
            current_operation:= Extract;
            current_entry_name:= U(short_name);
            is_unicode:= unicode_file_name;
            if current_user_attitude = none then
              conflict:= Exists(Add_extract_directory(name));
            else
              conflict:= True;
              -- Covers the case where current_user_attitude is "yes" before
              -- call to Extract, then "none" after.
            end if;
            begin
              Extract(
                from                 => zif,
                what                 => name,
                feedback             => Entry_feedback'Unrestricted_Access,
                help_the_file_exists => Name_conflict,
                tell_data            => null,
                get_pwd              => Get_password_internal'Unrestricted_Access,
                options              => (others => False),
                password             => To_String(To_Wide_String(password)),
                file_system_routines => Extract_FS_routines
              );
              case current_user_attitude is
                when yes | yes_to_all | rename_it =>
                  user_code:= success;
                when none =>
                  if conflict then
                    null; -- nothing happened since a file with that name existed
                  else
                    user_code:= success;
                  end if;
                when others =>
                  null;
              end case;
            exception
              when UnZip.User_Abort =>
                abort_rest_of_operation:= True;
              when UnZip.CRC_Error =>
                user_code:= bad_crc;
              when UnZip.Wrong_password =>
                user_code:= wrong_pwd;
                abort_rest_of_operation:= True;
            end;
          when Search =>
            if search_pattern = "" then -- just mark entries with matching names
              -- No feedback, it would be too time-consuming
              -- for just 1 instruction !
              user_code:= 1;
            else
              Feedback(
                file_percents_done,
                archive_percents_done,
                short_name,
                unicode_file_name,
                Search,
                dummy_user_abort
              );
              begin
                -- We need to search the string in the compressed entry...
                Search_1_file(name => name, occ  => user_code);
              exception
                when UnZip.Wrong_password =>
                  user_code:= wrong_pwd;
                  abort_rest_of_operation:= True;
              end;
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
    UnZip.current_user_attitude:= UnZip.yes;
    --------------------------------
    -- The main job is done here: --
    --------------------------------
    Traverse_archive(zif);
    --
    -- Almost done, we only need to process new entries
    --
    case operation is
      when Add =>
        for i in entry_name'Range loop
          processed_entries:= processed_entries + 1;
          archive_percents_done:= (100 * processed_entries) / total_entries;
          declare
            name: constant String:= To_UTF_8(To_Wide_String(entry_name(i)));
            short_name: constant String:= Remove_path(name);
          begin
            if not Zip.Exists(zif, short_name) then
              current_operation:= Append;
              current_entry_name:= U(short_name);
              is_unicode:= True;
              Add_File(
                Info               => new_zip,
                Name               => name,
                Name_in_archive    => short_name,
                Delete_file_after  => False,
                Name_UTF_8_encoded => True,
                Modification_time  => Zip.Convert(Modification_Time(name)),
                Is_read_only       => False, -- !!
                Feedback           => Entry_feedback'Unrestricted_Access
              );
            end if;
          exception
            when Zip.Compress.User_Abort =>
              abort_rest_of_operation:= True;
              exit;
          end;
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
        if abort_rest_of_operation then
          Delete_File(new_temp_name);
        else
          Delete_File(Zip.Zip_Name(zif));
          Rename(new_temp_name, Zip.Zip_Name(zif));
        end if;
      when Read_Only_Operation =>
        null;
    end case;
  end Process_archive;

end AZip_Common.Operations;
