with Zip.Compress, Zip.Create, UnZip.Streams, Zip_Streams;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Streams;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;
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
      when unsupported =>
        return "Compression format not supported";
      when others =>
        null;
    end case;
    case op is
      when Add =>
        case code is
          when replaced =>
            return "Replaced";
          when appended =>
            return "Added";
          when others =>
            null;
        end case;
      when Update =>
        case code is
          when nothing =>
            return "Same data or more recent timestamp in archive";
          when only_archive =>
            return "Only in archive; no file";
          when updated =>
            return "Updated from file";
          when others =>
            null;
        end case;
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
            return "OK & extracted";
          when others =>
            null;
        end case;
      when Search =>
        return Trim(Integer'Image(code), Left);
      when Recompress =>
        if code = nothing then
          return "Cannot recompress more";
        else
          return "To" & Integer'Image(101 - code) & "% of previous compression";
        end if;
    end case;
    --
    return "";
  end Result_message;

  function Result_value(s: UTF_16_String) return Integer is -- can be a non-number
  begin
    return Integer'Wide_Value(s);
  exception
    when others =>
      --  !! Consider some hashing here...
      if s'Length < 2 then
        return nothing;
      elsif s = "OK" then
        return success;
      elsif s = "Compressed data is corrupt" then
        return corrupt;
      elsif s = "CRC test failed" then
        return bad_crc;
      elsif s = "Compression format not supported" then
        return unsupported;
      elsif s = "Replaced" then
        return replaced;
      elsif s = "Only in archive; no file" then
        return only_archive;
      elsif s = "Updated from file" then
        return updated;
      elsif s = "Cannot recompress more" then
        return 0;
      elsif s(s'First..s'First+1) = "To" then
        for i in s'First+3 .. s'Last loop
          if s(i)='%' then
            return 101 - Integer'Wide_Value(s(s'First+3 .. i-1));
          end if;
        end loop;
      end if;
      return -100;
  end Result_value;

  max: constant Color_range:= Color_range'Last;
  f_max: constant Float:= Float(max);

  procedure Result_color(
    op        : Archive_Operation;
    code      : Integer;
    max_code  : Integer;
    color     : out RGB_type;
    intensity : out Float      --  Useful for setting a font black or white given the background
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
        color:= (Blue => max - val / 4, Red | Green => max - val);
      when Update =>
        case code is
          when updated =>
            color:= green;
          when only_archive =>
            color:= yellow;
          when others =>
            color:= white;
        end case;
      when Recompress =>
        if max_code = 0 or code < 0 then
          color:= white;
        else
          code_rel:= Float(code) / Float(max_code);
          code_rel:= code_rel ** 0.5; -- we skew the value (visual effect)
          val:= Color_range(Float'Floor(f_max * code_rel));
          color:= (Green => max - val / 4, Red | Blue => max - val);
        end if;
      -- For other operations, we have a simple color code: green or white
      when others =>
        case code is
          when success | appended =>
            color:= green;
          when others =>
            color:= white;
        end case;
    end case;
    -- Errors are always shown - in red of course!
    case code is
      when wrong_pwd | corrupt | bad_crc | unsupported =>
        color:= (Red => (max * 3) / 4, Green | Blue => 0);
      when others =>
        null;
    end case;
    raw_intensity_sq:=
      Integer(color.Red)   ** 2 +
      Integer(color.Green) ** 2 +
      Integer(color.Blue)  ** 2;
    intensity:= Sqrt(Float(raw_intensity_sq) / Float(max_raw_intensity_sq));
  end Result_color;

  function Description(
    e_op      : Entry_Operation;
    a_op      : Archive_Operation;
    skip_hint : Boolean
  )
  return UTF_16_String
  is
  begin
    case e_op is
      when Append =>
        return "Appending...";
      when Replace =>
        if a_op = Update then
          -- Since the replacement is only taken when contents are different
          -- we prefer not to confuse the user by saying "Replacing"...
          return "Processing...";
        else
          return "Replacing...";
        end if;
      when Recompress =>
        return "Recompressing...";
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
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : Zip.File_size_type;
      uncomp_size      : Zip.File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Zip.Time;
      method           : Zip.PKZip_method;
      name_encoding    : Zip.Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    )
    is
    pragma Unreferenced (file_index, comp_size, uncomp_size, crc_32, date_time, method, name_encoding, read_only, encrypted_2_x);
    begin
      Zip.Set_user_code(to, name, user_code);
    exception
      when File_name_not_found =>
        null;  --  Nothing bad: 'name' is a directory name that was skipped on recompression.
    end Copy_user_code;
    --
    procedure Do_it is new Zip.Traverse_verbose(Copy_user_code);
  begin
    Do_it(from);
  end Copy_user_codes;

  procedure Set_user_codes(info: Zip.Zip_info; code: Integer) is
    procedure Set_same_user_code(name: String) is
    begin
      Zip.Set_user_code(info, name, code);
    end Set_same_user_code;
    procedure Do_it is new Zip.Traverse(Set_same_user_code);
  begin
    Do_it(info);
  end Set_user_codes;

  function U(Source: Wide_String) return Unbounded_Wide_String
    renames Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String;

  function Remove_external_path(name: Name_descriptor) return UTF_16_String is
    s: constant Wide_String:= To_Wide_String(name.str);
  begin
    if name.sep = 0 then
      return Remove_path(s);
    else
      return s(s'First + name.sep .. s'Last);
    end if;
  end Remove_external_path;

  ------------------------------------------------
  -- Blocking, visible processing of an archive --
  ------------------------------------------------

  procedure Process_archive(
    zif             :        Zip.Zip_info; -- preserved, even after modifying operation
    operation       :        Archive_Operation;
    entry_name      :        Name_list;
    base_folder     :        UTF_16_String;
    search_pattern  :        UTF_16_String;
    output_folder   :        UTF_16_String;
    Set_Time_Stamp  :        UnZip.Set_Time_Stamp_proc;
    new_temp_name   :        String;
    Name_conflict   :        UnZip.Resolve_conflict_proc;
    password        : in out Unbounded_Wide_String;
    ignore_path     :        Boolean; -- ignore directories upon extraction
    encrypt         :        Boolean;
    max_code        :    out Integer;
    abort_operation :    out Boolean
  )
  is
    new_zip: Zip.Create.Zip_Create_info;
    new_fzs: aliased Zip_Streams.File_Zipstream;
    old_fzs: aliased Zip_Streams.File_Zipstream;
    file_percents_done: Natural:= 0;
    archive_percents_done: Natural:= 0;
    processed_entries, total_entries: Natural:= 0;
    current_entry_name: UTF_16_Unbounded_String;
    current_operation: Entry_Operation;
    current_skip_hint: Boolean;
    total_occurences: Natural:= 0;
    total_files_with_occurence: Natural:= 0;
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
        "", "",
        current_skip_hint,
        user_abort
      );
    end Entry_feedback;

    ignore_case: constant Boolean:= True; -- !! set as an option

    --
    -- Taken from Find_zip
    --
    procedure Search_1_file( name: String; occ: out Natural ) is
      max: constant:= 2**10;
      str: String(1..max);  -- str(1..stl) = string to search
      stl: Natural; -- string length
      l: Character; -- last character of the search string
      use UnZip.Streams;
      -- Define a circular buffer
      siz: constant:= max;
      type Buffer_range is mod siz;
      buf: array(Buffer_range) of Character:= (others => ' ');
      bup: Buffer_range:= 0;
      cancelled: Boolean;
      --  We define a local, ad-hoc stream type.
      --
      type Search_stream is new Ada.Streams.Root_Stream_Type with null record;
      --
      overriding procedure Read
        (Stream : in out Search_stream;
         Item   :    out Ada.Streams.Stream_Element_Array;
         Last   :    out Ada.Streams.Stream_Element_Offset);
      overriding procedure Write
       (Stream : in out Search_stream;
        Item   : in     Ada.Streams.Stream_Element_Array);

      --  Implementation of Read & Write:

      overriding procedure Read
        (Stream : in out Search_stream;
         Item   :    out Ada.Streams.Stream_Element_Array;
         Last   :    out Ada.Streams.Stream_Element_Offset) is null;  --  Not used.

      overriding procedure Write
       (Stream : in out Search_stream;
        Item   : in     Ada.Streams.Stream_Element_Array)
      is
        pragma Unreferenced (Stream);
        c: Character;
        i: Buffer_range:= 0;
        j: Natural;
      begin
        for sei in Item'Range loop
          c:= Character'Val(Item(sei));
          if ignore_case then
            c:= To_Upper(c);
          end if;
          if c = l then -- last character do match, search further...
            i:= bup;
            j:= stl;
            match: loop
              i:= i-1;  --  this loops modulo max: 3, 2, 1, 0, max-1, max-2, ...
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
      end Write;

      sst: Search_stream;

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
          Extract(
            Destination      => sst,
            Archive_Info     => zif,
            Name             => name,
            Ignore_Directory => False,
            Password         => To_String(To_Wide_String(password))
          );
          exit;
        exception
          when UnZip.Wrong_password =>
            if attempt = UnZip.tolerance_wrong_password then
              raise;
            end if;
            Change_password(To_Wide_String(current_entry_name), password, cancelled);
            if cancelled then
              raise UnZip.User_abort;
            end if;
        end;
      end loop;
    end Search_1_file;

    function Add_extract_directory(
      File_Name      : String;
      Name_Encoding  : Zip_name_encoding
    )
    return UTF_8_String
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
      cancelled: Boolean;
    begin
      Change_password(To_Wide_String(current_entry_name), password, cancelled);
      if cancelled then
        raise UnZip.User_abort;
      end if;
      -- persistence over entries + wide strings...
      pwd:=  To_Unbounded_String(To_String(To_Wide_String(password)));
    end Get_password_internal;
    --
    function Encryption_password return String is
    begin
      if encrypt then
        return To_String(To_Wide_String(password));
      else
        return "";
      end if;
    end Encryption_password;
    --
    Extract_FS_routines: constant UnZip.FS_routines_type:=
      ( Create_Path         => Ada.Directories.Create_Path'Access,
        Set_Time_Stamp      => Set_Time_Stamp,
        Compose_File_Name   => Add_extract_directory'Unrestricted_Access,
        others              => null
    );
    --
    use Zip.Create, UnZip;
    none_updated: Boolean:= True;
    none_recompressed: Boolean:= True;
    quick_method: constant Zip.Compress.Compression_Method:= Zip.Compress.Deflate_1;
    temp_single_entry_zip_name: constant String:= new_temp_name & ".single_entry_zip.tmp";
    temp_entry_data_name: constant String:= new_temp_name & ".entry_data.tmp";
    --
    --  Action for entry 'name' in current archive being traversed.
    --
    procedure Action_1_entry (
      name             : String; -- 'name' is compressed entry's full name
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : Zip.File_size_type;
      uncomp_size      : Zip.File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Zip.Time;
      method           : Zip.PKZip_method;
      name_encoding    : Zip.Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    )
    is
      pragma Unreferenced(method, read_only, encrypted_2_x);
      name_utf_16: constant UTF_16_String:= To_UTF_16(name, name_encoding);
      name_utf_8_as_in_archive: constant UTF_8_String:= To_UTF_8(name_utf_16);
      name_utf_8_with_extra_folder: constant UTF_8_String:= Add_extract_directory(name, name_encoding);
      short_name_utf_16: constant UTF_16_String:= Remove_path(name_utf_16);
      dummy_user_abort, skip_if_conflict: Boolean;
      match : Boolean:= False;
      add_file_idx : Natural;  --  Index in the entry_name list (file to be added)
      -- Just copy entry from old to new archive (Modifying_Operation):
      procedure Preserve_entry is
      begin
        current_operation:= Copy;
        current_entry_name:= U(short_name_utf_16);
        Zip_Streams.Set_Index(old_fzs, file_index);
        Zip.Create.Add_Compressed_Stream(
          Info     => new_zip,
          Stream   => old_fzs,
          Feedback => Entry_feedback'Unrestricted_Access
        );
      end Preserve_entry;
      --
      procedure Tentative_compress(
        stamp              : Time;
        method             : Zip.Compress.Compression_Method;
        external_file_name : String
      )
      is
        temp_single_entry_zip_zci: Zip.Create.Zip_Create_info;
        temp_single_entry_zip_fzs: aliased Zip_Streams.File_Zipstream;
      begin
        Zip.Create.Create(
          temp_single_entry_zip_zci,
          temp_single_entry_zip_fzs'Unchecked_Access,
          temp_single_entry_zip_name,
          method
        );
        Add_File(
          Info               => temp_single_entry_zip_zci,
          Name               => external_file_name,
          Name_in_archive    => name_utf_8_as_in_archive,
          Delete_file_after  => False,
          Name_encoding      => UTF_8,
          Modification_time  => stamp,
          Is_read_only       => False,
          Feedback           => Entry_feedback'Unrestricted_Access,
          Password           => ""
          --  Update/recompress operation with password is not supported anyway.
          --  So we renounce doing an accidental encryption.
          --  !! To do: support encryption (perhaps just preserve encrypted entries)
        );
        Finish(temp_single_entry_zip_zci);
      exception
        when Zip.Compress.User_abort =>
          Zip_Streams.Close (temp_single_entry_zip_fzs);
          raise;
      end Tentative_compress;
      --
      procedure Add_tentatively_compressed(single_file_index: Zip_Streams.ZS_Index_Type)
      is
        temp_single_entry_zip_fzs : Zip_Streams.File_Zipstream;
      begin
        Zip_Streams.Set_Name(temp_single_entry_zip_fzs, temp_single_entry_zip_name);
        Zip_Streams.Open(temp_single_entry_zip_fzs, Zip_Streams.In_File);
        Zip_Streams.Set_Index(temp_single_entry_zip_fzs, single_file_index);
        Zip.Create.Add_Compressed_Stream(
          Info     => new_zip,
          Stream   => temp_single_entry_zip_fzs,
          Feedback => Entry_feedback'Unrestricted_Access
        );
        Zip_Streams.Close(temp_single_entry_zip_fzs);
      end Add_tentatively_compressed;
      --
      procedure Update_entry is
        stamp: constant Time:= Zip.Convert(Modification_Time(name_utf_8_with_extra_folder));
        -- Ada.Directories not utf-8 compatible !!
        use Zip_Streams.Calendar;
        temp_single_entry_zip_zif : Zip.Zip_info;
        dummy_name_encoding       : Zip_name_encoding;
        single_file_index         : Zip_Streams.ZS_Index_Type;
        dummy_comp_size           : Zip.File_size_type;
        dummy_uncomp_size         : Zip.File_size_type;
        new_crc_32                : Interfaces.Unsigned_32;
        use Interfaces;
      begin
        if date_time > stamp then -- newer in archive -> preserve enry in archive
          Preserve_entry;
          user_code:= nothing;
          return;
        end if;
        current_operation:= Replace;
        current_entry_name:= U(short_name_utf_16);
        -- We write first a one-file zip file with the new data
        Tentative_compress(stamp, quick_method, name_utf_8_with_extra_folder);
        -- We load the one-file zip file's information
        Load(temp_single_entry_zip_zif, temp_single_entry_zip_name);
        Find_offset(
          info          => temp_single_entry_zip_zif,
          name          => name_utf_8_as_in_archive,
          name_encoding => dummy_name_encoding,
          file_index    => single_file_index,
          comp_size     => dummy_comp_size,
          uncomp_size   => dummy_uncomp_size,
          crc_32        => new_crc_32
        );
        if new_crc_32 = crc_32 then
          --  Nothing to do: file is the same, or different with 1 / 2**32 probability.
          Preserve_entry;
          user_code:= nothing;
        else
          Add_tentatively_compressed(single_file_index);
          user_code:= updated;
          none_updated:= False;
        end if;
      exception
        when Zip.Compress.User_abort =>
          abort_operation:= True;
      end Update_entry;
      --
      procedure Recompress_entry is
        temp_single_entry_zip_zif : Zip.Zip_info;
        dummy_name_encoding       : Zip_name_encoding;
        single_file_index         : Zip_Streams.ZS_Index_Type;
        new_comp_size             : Zip.File_size_type;
        dummy_uncomp_size         : Zip.File_size_type;
        new_crc_32                : Interfaces.Unsigned_32;
        use Interfaces;
      begin
        Zip_Streams.Close(old_fzs);  --  Close the old_fzs stream
        Extract(
          from   => zif,
          what   => name,
          rename => temp_entry_data_name
        );
        Zip_Streams.Open(old_fzs, Zip_Streams.In_File);  --  Reopen the old_fzs stream
        current_operation:= Recompress;
        current_entry_name:= U(short_name_utf_16);
        Tentative_compress(
          date_time,
          Zip.Compress.Preselection_Method'Last,
          temp_entry_data_name
        );
        -- We load the one-file zip file's information
        Load(temp_single_entry_zip_zif, temp_single_entry_zip_name);
        Find_offset(
          info          => temp_single_entry_zip_zif,
          name          => name_utf_8_as_in_archive,
          name_encoding => dummy_name_encoding,
          file_index    => single_file_index,
          comp_size     => new_comp_size,
          uncomp_size   => dummy_uncomp_size,
          crc_32        => new_crc_32
        );
        if new_crc_32 /= crc_32 then
          raise Ada.IO_Exceptions.Data_Error
            with "Data tampered between decompression and recompression";
        end if;
        if new_comp_size < comp_size then
          Add_tentatively_compressed(single_file_index);
          --  101: even a slight gain (< 1%) is recorded
          user_code:= 101 - Integer(100.0 * Float(new_comp_size) / Float(comp_size));
          none_recompressed:= False;
        else
          --  user code remains 0 (nothing)
          Preserve_entry;
        end if;
      exception
        when UnZip.User_abort | Zip.Compress.User_abort =>
          abort_operation:= True;
      end Recompress_entry;
      --
      use type Zip.File_size_type;
    begin  --  Action_1_entry
      user_code:= nothing;
      if abort_operation then
        return;
      end if;
      processed_entries:= processed_entries + 1;
      archive_percents_done:= (100 * processed_entries) / total_entries;
      file_percents_done:= 0;
      --
      -- !! we could use rather hashed maps for searching
      --
      case operation is
        when Add =>
          for i in entry_name'Range loop
            if base_folder & Remove_external_path(entry_name(i)) = name_utf_16 then
              -- The path removed is from an external file name.
              -- The file will be added with the base_folder from archive
              match := True;
              add_file_idx := i;
              exit;
            end if;
          end loop;
        when Update | Recompress | Test =>
          match:= True;
        when Remove | Extract =>
          if entry_name'Length = 0 then
            match:= True; -- empty name list -> we process the whole archive
          else
            for i in entry_name'Range loop
              if To_Wide_String(entry_name(i).str) = name_utf_16 then
                match:= True;
                exit;
              end if;
            end loop;
          end if;
        when Search =>
          --  For Search, the entry name list is actually a list of search patterns.
          if entry_name'Length = 0 then
            --  If the list is empty, we process the whole archive.
            match:= True;
          end if;
          for i in entry_name'Range loop
            declare
              pattern: UTF_16_String := To_Wide_String(entry_name(i).str);
              up_name: UTF_16_String := short_name_utf_16;
            begin
              if ignore_case then
                pattern := To_Upper (pattern);
                up_name := To_Upper (up_name);
              end if;
              if pattern = ""  --  Always match
                or else Index (up_name, pattern) > 0
              then
                match:= True;
                exit;
              end if;
            end;
          end loop;
      end case;
      --
      if match then
        case operation is
          when Add =>
            current_operation := Replace;
            current_entry_name := U (short_name_utf_16);
            -- Here we compress new contents for replacing an existing entry
            declare
              external_file_name : constant UTF_8_String :=
                To_UTF_8 (To_Wide_String (entry_name (add_file_idx).str));
            begin
              Add_File (
                Info               => new_zip,
                Name               => external_file_name,
                Name_in_archive    => name,
                Delete_file_after  => False,
                Name_encoding      => name_encoding,
                Modification_time  => Zip.Convert(Modification_Time(external_file_name)),
                Is_read_only       => False, -- !!
                Feedback           => Entry_feedback'Unrestricted_Access,
                Password           => Encryption_password
              );
              user_code:= success;
            exception
              when Zip.Compress.User_abort =>
                abort_operation:= True;
            end;
          when Update =>
            if name = "" or else (name(name'Last)= '\' or name(name'Last)= '/') then
              Preserve_entry; -- copy: it is a directory name (not visible in AZip)
            elsif Zip.Exists(name_utf_8_with_extra_folder) then
              Update_entry;
            else
              Preserve_entry;
              user_code:= only_archive;
            end if;
          when Recompress =>
            if name = "" or else (name(name'Last)= '\' or name(name'Last)= '/') then
              --  Skip this: useless directory name.
              none_recompressed:= False;  --  Indicate there is a kind of recompression done.
            elsif uncomp_size < 6 then
              Preserve_entry;
            else
              Recompress_entry;
            end if;
          when Remove =>
            Feedback(
              file_percents_done,
              archive_percents_done,
              short_name_utf_16,
              Skip,
              "", "",
              True,
              dummy_user_abort
            );
          when Test =>
            current_operation:= Test;
            current_entry_name:= U(short_name_utf_16);
            Entry_feedback(1, False, dummy_user_abort);
            -- ^ Just have the right title if password is asked for
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
              when UnZip.User_abort =>
                abort_operation:= True;
              when UnZip.CRC_Error =>
                user_code:= bad_crc;
              when Zip.Archive_corrupted =>
                user_code:= corrupt;
              when UnZip.Wrong_password =>
                user_code:= wrong_pwd;
                abort_operation:= True;
              when UnZip.Unsupported_method | UnZip.Not_supported =>
                user_code:= unsupported;
            end;
          when Extract =>
            current_operation:= Extract;
            current_entry_name:= U(short_name_utf_16);
            Entry_feedback(1, False, dummy_user_abort);
            -- ^ Just have the right title if password is asked for
            if current_user_attitude = none then
              skip_if_conflict:= Zip.Exists(name_utf_8_with_extra_folder);
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
                options              => (junk_directories => ignore_path, others => False),
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
              when UnZip.User_abort =>
                abort_operation:= True;
              when UnZip.CRC_Error =>
                user_code:= bad_crc;
              when Zip.Archive_corrupted =>
                user_code:= corrupt;
              when UnZip.Wrong_password =>
                user_code:= wrong_pwd;
                abort_operation:= True;
              when UnZip.Unsupported_method | UnZip.Not_supported =>
                user_code:= unsupported;
            end;
          when Search =>
            if search_pattern = "" then -- just mark entries with matching names
              -- No feedback, it would be too time-consuming
              -- for just 1 instruction !
              user_code:= 1;
            else
              begin
                -- We need to search the string in the compressed entry...
                Search_1_file(name => name, occ  => user_code);
                if user_code > 0 then
                  total_files_with_occurence:= total_files_with_occurence + 1;
                  total_occurences:= total_occurences + user_code;
                end if;
              exception
                when UnZip.User_abort =>
                  abort_operation:= True;
                when UnZip.Wrong_password =>
                  user_code:= wrong_pwd;
                  abort_operation:= True;
                when UnZip.CRC_Error =>
                  user_code:= bad_crc;
                when Zip.Archive_corrupted =>
                  user_code:= corrupt;
                when UnZip.Unsupported_method | UnZip.Not_supported =>
                  user_code:= unsupported;
              end;
              Feedback(
                file_percents_done,
                archive_percents_done,
                short_name_utf_16,
                Search,
                "Occurences found so far:" & Integer'Image(total_occurences),
                "Entries with occurences:" & Integer'Image(total_files_with_occurence),
                False,
                dummy_user_abort
              );
            end if;
        end case;
        max_code:= Integer'Max(max_code, user_code);
      else -- archive entry name is not matched by a file name in the list
        case operation is
          when Modifying_Operation =>
            Preserve_entry;
          when Read_Only_Operation =>
            null; -- Nothing to do here
        end case;
      end if;
    end Action_1_entry;

    procedure Traverse_archive is new Zip.Traverse_verbose (Action_1_entry);

  begin -- Process_archive
    max_code:= 0;
    abort_operation:= False;
    if not Zip.Is_loaded(zif) then
      return; -- we have a "null" archive (not even a file with 0 entries)
    end if;
    case operation is
      when Add =>
        total_entries:= Zip.Entries(zif) + entry_name'Length;
      when Update | Recompress | Remove | Read_Only_Operation =>
        total_entries:= Zip.Entries(zif);
    end case;
    case operation is
      when Modifying_Operation =>
        Zip_Streams.Set_Name(old_fzs, Zip.Zip_name(zif));
        Zip_Streams.Open(old_fzs, Zip_Streams.In_File);
        Zip.Create.Create(
          new_zip,
          new_fzs'Unchecked_Access,
          new_temp_name,
          quick_method
        );
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
    ------------------------------------------------------
    -- Almost done, we only need to process new entries --
    ------------------------------------------------------
    case operation is
      when Add =>
        for i in entry_name'Range loop
          processed_entries:= processed_entries + 1;
          archive_percents_done:= (100 * processed_entries) / total_entries;
          declare
            name: constant UTF_16_String:= To_Wide_String(entry_name(i).str);
            short_name: constant UTF_16_String:=
              base_folder & Remove_external_path(entry_name(i));
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
                  Feedback           => Entry_feedback'Unrestricted_Access,
                  Password           => Encryption_password
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
                    Feedback           => Entry_feedback'Unrestricted_Access,
                    Password           => Encryption_password
                  );
              end;
            end if;
          exception
            when Zip.Compress.User_abort =>
              abort_operation:= True;
              exit;
          end;
        end loop;
      when Update | Remove | Recompress =>
        null;
        -- There should be no file to be updated, recompressed or removed which is
        -- not in original archive.
      when Read_Only_Operation =>
        null;
        -- Nothing to do after archive traversal.
    end case;
    case operation is
      when Modifying_Operation =>
        Zip_Streams.Close(old_fzs);
        Finish(new_zip);
        if abort_operation or
          (operation = Update and none_updated) or
          (operation = Recompress and none_recompressed)
        then
          --  New archive is discarded.
          Delete_File(new_temp_name);
        else
          Delete_File(Zip.Zip_name(zif));
          Rename(new_temp_name, Zip.Zip_name(zif));
        end if;
        if operation in Update .. Recompress then
          if Zip.Exists(temp_single_entry_zip_name) then
            Delete_File(temp_single_entry_zip_name);
          end if;
          if Zip.Exists(temp_entry_data_name) then
            Delete_File(temp_entry_data_name);
          end if;
        end if;
      when Read_Only_Operation =>
        null;
    end case;
  end Process_archive;

  function Expand_folders(l: Name_list) return Name_list is
    --
    -- The challenge is to do it all without heap allocation :-)
    --
    function Expand_one_entry(Name: Name_descriptor) return Name_list is
      --
      files, total_files: Natural:= 0;
      sep: Natural:= 0;
      --
      -- Position of separator in "d:\ada\azip\gwindows\test\jaja\"

      --
      -- This is a modified version of the Rosetta code example
      -- http://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada
      --
      procedure Full_Walk(Name : Wide_String; Count_only: Boolean; New_list: out Name_list) is
        --
        procedure Walk (Name : Wide_String) is
          procedure Print (Item : Directory_Entry_Type) is
          begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
              files:= files + 1;
              if not Count_only then
                New_list(files):= (U((To_Wide_String(Full_Name (Item)))), sep);
              end if;
            end if;
          end Print;
          procedure Subdir (Item : Directory_Entry_Type) is
          begin
            if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
              Walk (To_Wide_String(Full_Name (Item)));
            end if;
          exception
            when Ada.IO_Exceptions.Name_Error => null;
          end Subdir;
        begin
          -- The files
          Search (To_String(Name), "*", (Directory => False, others => True), Print'Access);
          -- The subfolders
          Search (To_String(Name), "", (Directory => True, others => False), Subdir'Access);
        end Walk;
      begin
        Walk(Name);
      end Full_Walk;

      Dir: constant Wide_String:= To_Wide_String(Name.str);
      fake_list: Name_list(1..0);
      --
    begin
      Full_Walk (Name => Dir, Count_only => True, New_list => fake_list);
      total_files:= files;
      files:= 0;
      declare
        new_list: Name_list(1..total_files);
      begin
        for i in Dir'Range loop
          if Dir(i) = '\' or Dir(i) = '/' then
            sep:= i;
          end if;
        end loop;
        Full_Walk (Name => Dir, Count_only => False, New_list => new_list);
        return new_list;
      end;
    exception
      when Ada.IO_Exceptions.Name_Error =>
        return (1 => Name);
    end Expand_one_entry;
    --
  begin
    if l'Length = 0 then
      return l;
    else
      return
        -- Looks like LISP, doesn't it ?...
        ( Expand_one_entry(l(l'First)) &            -- car - first item
          Expand_folders(l(l'First + 1 .. l'Last))  -- cdr - rest of the list
        );
    end if;
  end Expand_folders;

  procedure Count_test_totals(
    archive: Zip.Zip_info;
    count_ok, count_ko, count_nt: out Natural
  )
  is
    procedure Action(
      name             : String; -- 'name' is compressed entry's full name
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : Zip.File_size_type;
      uncomp_size      : Zip.File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Zip.Time;
      method           : Zip.PKZip_method;
      name_encoding    : Zip.Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    )
    is
    pragma Unreferenced (
      name, file_index, comp_size, uncomp_size, crc_32,
      date_time, method, name_encoding, read_only,
      encrypted_2_x);
    begin
      case user_code is
        when success => count_ok:= count_ok + 1;
        when nothing => count_nt:= count_nt + 1; -- happens if cancelled
        when others  => count_ko:= count_ko + 1;
      end case;
    end Action;
    procedure Count_totals is new Zip.Traverse_verbose(Action);
  begin
    count_ok:= 0;
    count_ko:= 0;
    count_nt:= 0;
    Count_totals(archive);
  end Count_test_totals;

end AZip_Common.Operations;
