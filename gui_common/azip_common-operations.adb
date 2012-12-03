with Zip.Create, Zip.Compress, UnZip.Streams, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
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
      when corrupt =>
        return "Compressed data is corrupt";
      when bad_crc =>
        return "CRC test failed";
      when others =>
        null;
    end case;
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
          when nothing =>
            return "Not tested";
          when others =>
            null;
        end case;
      when Extract =>
        case code is
          when success =>
            return "Checked and extracted";
          when others =>
            null;
        end case;
      when Search =>
        return Trim(Integer'Image(code), Left);
    end case;
    --
    return "";
  end Result_message;

  max: constant Color_range:= Color_range'Last;
  f_max: constant Float:= FLoat(max);

  procedure Result_color(
    op        : Archive_Operation;
    code      : Integer;
    max_code  : Integer;
    color     : out RGB_type;
    intensity : out Float
  )
  is
    val: Color_range;
    raw_intensity_sq: Natural;
    max_raw_intensity_sq: constant:= max * max * 3;
    code_rel: Float;
  begin
    case op is
      when Search =>
        if max_code = 0 or code < 0 then
          val:= 0;
        else
          code_rel:= Float(code) / Float(max_code);
          code_rel:= code_rel ** 0.25; -- we skew the value (visual effect)
          val:= Color_range(Float'Floor(f_max * code_rel));
        end if;
        color:= (Red => max - val, Green => max - val, Blue => max - val / 4);
      when others =>
        case code is
          when success =>
            color:= (Red => 0, Green => (max * 3) / 4, Blue => 0);
          when others =>
            color:= (Red => max, Green => max, Blue => max);
        end case;
    end case;
    -- Errors are always shown
    case code is
      when wrong_pwd | corrupt | bad_crc =>
        color:= (Red => (max * 3) / 4, Green => 0, Blue => 0);
      when others =>
        null;
    end case;
    raw_intensity_sq:=
      Integer(color.Red)   ** 2 +
      Integer(color.Green) ** 2 +
      Integer(color.Blue)  ** 2;
    intensity:= Sqrt(Float(raw_intensity_sq) / Float(max_raw_intensity_sq));
  end Result_color;

  function Description(op: Entry_Operation; skip_hint: Boolean) return UTF_16_String is
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
        if skip_hint then
          return "Extracting (unless skipped)...";
        else
          return "Extracting...";
        end if;
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
      name_encoding    : Zip.Zip_name_encoding;
      read_only        : Boolean;
      user_code        : in out Integer
    )
    is
    pragma Unreferenced (file_index, comp_size, uncomp_size, crc_32, date_time, method, name_encoding, read_only);
    begin
      Zip.Set_user_code(to, name, user_code);
    end Copy_user_code;
    procedure Do_it is new Zip.Traverse_verbose(Copy_user_code);
  begin
    Do_it(from);
  end Copy_user_codes;

  function U(Source: Wide_String) return Unbounded_Wide_String
    renames Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String;

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  procedure Process_archive(
    zif             :        Zip.Zip_Info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    base_folder     :        UTF_16_String;
    search_pattern  :        UTF_16_String;
    output_folder   :        UTF_16_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String;
    max_code        :    out Integer
  )
  is
    new_zip: Zip.Create.Zip_Create_info;
    new_fzs: aliased Zip_Streams.File_Zipstream;
    old_fzs: aliased Zip_Streams.File_Zipstream;
    file_percents_done: Natural:= 0;
    archive_percents_done: Natural:= 0;
    processed_entries, total_entries: Natural:= 0;
    current_entry_name: UTF_16_Unbounded_String;
    current_operation: Entry_operation;
    current_skip_hint: Boolean;
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
        To_Wide_String(current_entry_name),
        current_operation,
        current_skip_hint,
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
            Change_password(To_Wide_String(current_entry_name), password);
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

    function Add_extract_directory(
      File_Name      : String;
      Name_Encoding  : Zip_name_encoding
    )
    return String
    is
      UTF_8_Name: constant UTF_8_String:= To_UTF_8(File_Name, Name_Encoding);
      -- AZip writes all files with names that passed UTF_8 encoded to the system
    begin
      if output_folder = "" then
        return UTF_8_Name;
      elsif output_folder(output_folder'Last) = '\' or
            output_folder(output_folder'Last) = '/'
      then
        return To_UTF_8(output_folder) & UTF_8_Name;
      else
        return To_UTF_8(output_folder) & '/' & UTF_8_Name;
        -- '/' is also accepted by Windows
      end if;
    end Add_extract_directory;
    --
    procedure Get_password_internal(pwd: out Unbounded_String) is
      use UnZip;
    begin
      Change_password(To_Wide_String(current_entry_name), password);
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
      name_encoding    : Zip.Zip_name_encoding;
      read_only        : Boolean;
      user_code        : in out Integer
    )
    is
      pragma Unreferenced
        (comp_size, uncomp_size, crc_32, date_time, method, read_only);
      name_utf_16: constant UTF_16_String:= To_UTF_16(name, name_encoding);
      short_name_utf_16: constant UTF_16_String:= Remove_path(name_utf_16);
      dummy_user_abort, skip_if_conflict: Boolean;
      match: Boolean:= False;
      idx: Natural;
    begin
      user_code:= nothing;
      if abort_rest_of_operation then
        return;
      end if;
      processed_entries:= processed_entries + 1;
      archive_percents_done:= (100 * processed_entries) / total_entries;
      file_percents_done:= 0;
      --
      -- !! use hashed maps either for searching
      --
      case operation is
        when Add =>
          for i in entry_name'Range loop
            if base_folder & Remove_path(To_Wide_String(entry_name(i))) = name_utf_16 then
              -- The path removed is from an external file name.
              -- The file will be added with the base_folder from archive
              match:= True;
              idx:= i;
              exit;
            end if;
          end loop;
        when Remove | Extract =>
          if entry_name'Length = 0 then
            match:= True; -- empty name list -> we process the whole archive
          else
            for i in entry_name'Range loop
              if To_Wide_String(entry_name(i)) = name_utf_16 then
                match:= True;
                idx:= i;
                exit;
              end if;
            end loop;
          end if;
        when Test =>
          match:= True;
        when Search =>
          if entry_name'Length = 0 then
            match:= True; -- empty name list -> we process the whole archive
          else
            for i in entry_name'Range loop
              declare
                pattern: constant UTF_16_String:= To_Wide_String(entry_name(i));
              begin
                if pattern = "" or else -- empty name -> we process the whole archive
                  Index(short_name_utf_16, pattern) > 0
                then
                  match:= True;
                  idx:= i;
                  exit;
                end if;
              end;
            end loop;
          end if;
      end case;
      --
      if match then
        case operation is
          when Add =>
            current_operation:= Replace;
            current_entry_name:= U(short_name_utf_16);
            declare
            external_file_name: constant UTF_8_String:=
                To_UTF_8(To_Wide_String(entry_name(idx)));
            begin
              -- !! try IBM_437 (same as Append below)
              Add_File(
                Info               => new_zip,
                Name               => external_file_name,
                Name_in_archive    => To_UTF_8(short_name_utf_16),
                Delete_file_after  => False,
                Name_encoding      => UTF_8,
                Modification_time  => Zip.Convert(Modification_Time(external_file_name)),
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
              short_name_utf_16,
              Skip,
              True,
              dummy_user_abort
            );
          when Test =>
            current_operation:= Test;
            current_entry_name:= U(short_name_utf_16);
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
              when Zip.Zip_file_Error =>
                user_code:= corrupt;
              when UnZip.Wrong_password =>
                user_code:= wrong_pwd;
                abort_rest_of_operation:= True;
            end;
          when Extract =>
            current_operation:= Extract;
            current_entry_name:= U(short_name_utf_16);
            if current_user_attitude = none then
              skip_if_conflict:= Zip.Exists(Add_extract_directory(name, name_encoding));
              current_skip_hint:= True;
            else
              skip_if_conflict:= True;
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
                  if skip_if_conflict then
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
              when Zip.Zip_file_Error =>
                user_code:= corrupt;
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
                short_name_utf_16,
                Search,
                False,
                dummy_user_abort
              );
              begin
                -- We need to search the string in the compressed entry...
                Search_1_file(name => name, occ  => user_code);
              exception
                when UnZip.Wrong_password =>
                  user_code:= wrong_pwd;
                  abort_rest_of_operation:= True;
                when UnZip.CRC_Error =>
                  user_code:= bad_crc;
                when Zip.Zip_file_Error =>
                  user_code:= corrupt;
              end;
            end if;
            max_code:= Integer'Max(max_code, user_code);
        end case;
      else -- archive entry name is not matched by a file name in the list
        case operation is
          when Modifying_Operation =>
            current_operation:= Copy;
            current_entry_name:= U(short_name_utf_16);
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
    max_code:= 0;
    if not Zip.Is_loaded(zif) then
      return; -- we have a "null" archive (not even a file with 0 entries)
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
        Zip.Create.Create(new_zip, new_fzs'Unchecked_Access, new_temp_name);
      when Read_Only_Operation =>
        null;
        -- Read-only operation, doesn't need a new archive file;
        -- current archive opened only at entry testing / extracting
    end case;
    UnZip.current_user_attitude:= UnZip.yes;
    current_skip_hint:= False;
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
            name: constant UTF_16_String:= To_Wide_String(entry_name(i));
            short_name: constant UTF_16_String:= Remove_path(name);
            ex: Boolean;
          begin
            begin
              ex:= Zip.Exists(zif, To_IBM_437(short_name));
            exception
              when Cannot_encode_to_IBM_437 =>
                ex:= False; -- cannot exist under 437 encoding
            end;
            ex:= ex or Zip.Exists(zif, To_UTF_8(short_name));
            if not ex then
              current_operation:= Append;
              current_entry_name:= U(short_name);
              begin
                -- We try to convert as many names as possible to the
                -- "old DOS" encoding, for compatibility, e.g. with WinZip 10.0 (2005 !)
                Add_File(
                  Info               => new_zip,
                  Name               => To_UTF_8(name), -- external file -> UTF-8
                  Name_in_archive    => To_IBM_437(short_name),
                  Delete_file_after  => False,
                  Name_encoding      => IBM_437,
                  Modification_time  => Zip.Convert(Modification_Time(To_UTF_8(name))),
                  Is_read_only       => False, -- !!
                  Feedback           => Entry_feedback'Unrestricted_Access
                );
              exception
                when Cannot_encode_to_IBM_437 =>
                  Add_File(
                    Info               => new_zip,
                    Name               => To_UTF_8(name), -- external file -> UTF-8
                    Name_in_archive    => To_UTF_8(short_name),
                    Delete_file_after  => False,
                    Name_encoding      => UTF_8,
                    Modification_time  => Zip.Convert(Modification_Time(To_UTF_8(name))),
                    Is_read_only       => False, -- !!
                    Feedback           => Entry_feedback'Unrestricted_Access
                  );
              end;
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
