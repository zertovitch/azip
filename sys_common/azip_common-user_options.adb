with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body AZip_Common.User_options is

  use Ada.Strings.Fixed, Ada.Strings.Wide_Unbounded;

  function Is_Temp_Directory_Valid (opt : Option_Pack_Type) return Boolean is
    procedure Do_Test is
      use Ada.Directories, Ada.Text_IO;
      test : File_Type;
      file_name : constant String :=
        Compose (To_UTF_8 (To_Wide_String (opt.temp_directory)), "$_test_file_$", "tmp");
    begin
      Create (test, Out_File, file_name);
      Close (test);
      Delete_File (file_name);
    end Do_Test;
  begin
    Do_Test;
    return True;
  exception
    when others =>
      return False;
  end Is_Temp_Directory_Valid;

  package body Persistence is

    procedure Load (opt : out Option_Pack_Type) is
      defaults : Option_Pack_Type;
    begin
      for k in Persistence_Key loop
        begin
          declare
            s  : constant Wide_String := Read_Key (k);
          begin
            case k is

              when Col_Width_Key =>
                for t in Entry_topic loop
                  if Index (k'Image, t'Image) > 0 then
                    opt.column_width (t) := Integer'Wide_Value (s);
                    if opt.column_width (t) = 0 then
                      opt.column_width (t) := defaults.column_width (t);
                    end if;
                  end if;
                end loop;

              when Col_Visible_Key =>
                for t in Entry_topic loop
                  if Index (k'Image, t'Image) > 0 then
                    opt.visible_column (t) := Boolean'Wide_Value (s);
                  end if;
                end loop;

              when mru1 .. mru9 =>
                opt.mru (Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1) :=
                  To_Unbounded_Wide_String (s);

              when view_mode           => opt.view_mode                   := View_Mode_Type'Wide_Value (s);
              when sort_column         => opt.sort_column                 := Integer'Wide_Value (s);
              when sort_direction      => opt.sort_direction              := Sort_Direction_Type'Wide_Value (s);
              when win_left            => opt.win_left                    := Integer'Wide_Value (s);
              when win_top             => opt.win_top                     := Integer'Wide_Value (s);
              when win_width           => opt.win_width                   := Integer'Wide_Value (s);
              when win_height          => opt.win_height                  := Integer'Wide_Value (s);
              when maximized           => opt.MDI_main_maximized          := Boolean'Wide_Value (s);
              when children_maximized  => opt.MDI_childen_maximized       := Boolean'Wide_Value (s);
              when tree_portion        => opt.tree_portion                := Float'Wide_Value (s);
              when show_passwords      => opt.show_passwords              := Boolean'Wide_Value (s);
              when ignore_extract_path => opt.ignore_extract_path         := Boolean'Wide_Value (s);
              when extract_directory   => opt.suggested_extract_directory := To_Unbounded_Wide_String (s);
              when temp_directory      => opt.temp_directory              := To_Unbounded_Wide_String (s);
              when first_visit         => opt.first_visit                 := Boolean'Wide_Value (s);
              when backup_recomp       => opt.backup_recomp               := Boolean'Wide_Value (s);
              when backup_update       => opt.backup_update               := Boolean'Wide_Value (s);

            end case;
          end;
        exception
          when others =>
            null;
            --  This key is missing (from registry, config file,...)
            --  Data_Error or something else.
            --  We just continue...
        end;
      end loop;
    end Load;

    procedure Save (opt : in Option_Pack_Type) is
    begin
      for k in Persistence_Key loop
        declare
          procedure R (v : Wide_String) is
          begin
            Write_Key (k, v);
          end R;
        begin
          case k is

            when Col_Width_Key =>
              for t in Entry_topic loop
                if Index (k'Image, t'Image) > 0 then
                  R (opt.column_width (t)'Wide_Image);
                end if;
              end loop;

            when Col_Visible_Key =>
              for t in Entry_topic loop
                if Index (k'Image, t'Image) > 0 then
                  R (opt.visible_column (t)'Wide_Image);
                end if;
              end loop;

            when mru1 .. mru9 =>
              R (To_Wide_String (opt.mru (Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1)));

            when view_mode           => R (opt.view_mode'Wide_Image);
            when sort_column         => R (opt.sort_column'Wide_Image);
            when sort_direction      => R (opt.sort_direction'Wide_Image);
            when win_left            => R (opt.win_left'Wide_Image);
            when win_top             => R (opt.win_top'Wide_Image);
            when win_width           => R (opt.win_width'Wide_Image);
            when win_height          => R (opt.win_height'Wide_Image);
            when maximized           => R (opt.MDI_main_maximized'Wide_Image);
            when children_maximized  => R (opt.MDI_childen_maximized'Wide_Image);
            when tree_portion        => R (opt.tree_portion'Wide_Image);
            when show_passwords      => R (opt.show_passwords'Wide_Image);
            when ignore_extract_path => R (opt.ignore_extract_path'Wide_Image);
            when extract_directory   => R (To_Wide_String (opt.suggested_extract_directory));
            when temp_directory      => R (To_Wide_String (opt.temp_directory));
            when first_visit         => R (opt.first_visit'Wide_Image);
            when backup_recomp       => R (opt.backup_recomp'Wide_Image);
            when backup_update       => R (opt.backup_update'Wide_Image);
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

  procedure Show_Persistence_Keys is
  begin
    for k in Persistence_Key loop
      String_Output (k'Image);
    end loop;
  end Show_Persistence_Keys;

end AZip_Common.User_options;
