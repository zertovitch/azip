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
              when view_mode =>
                opt.view_mode := View_Mode_Type'Wide_Value (s);
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
              when sort_column =>
                opt.sort_column := Integer'Wide_Value (s);
              when sort_direction =>
                opt.sort_direction := Sort_Direction_Type'Wide_Value (s);
              when win_left =>
                opt.win_left   := Integer'Wide_Value (s);
              when win_top =>
                opt.win_top    := Integer'Wide_Value (s);
              when win_width =>
                opt.win_width  := Integer'Wide_Value (s);
              when win_height =>
                opt.win_height := Integer'Wide_Value (s);
              when maximized =>
                opt.MDI_main_maximized := Boolean'Wide_Value (s);
              when children_maximized =>
                opt.MDI_childen_maximized := Boolean'Wide_Value (s);
              when tree_portion =>
                opt.tree_portion := Float'Wide_Value (s);
              when mru1 .. mru9 =>
                opt.mru (Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1) :=
                  To_Unbounded_Wide_String (s);
              when show_passwords =>
                opt.show_passwords := Boolean'Wide_Value (s);
              when ignore_extract_path =>
                opt.ignore_extract_path := Boolean'Wide_Value (s);
              when extract_directory =>
                opt.suggested_extract_directory := To_Unbounded_Wide_String (s);
              when temp_directory =>
                opt.temp_directory := To_Unbounded_Wide_String (s);
              when first_visit =>
                opt.first_visit := Boolean'Wide_Value (s);
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
            when view_mode =>
              R (View_Mode_Type'Wide_Image (opt.view_mode));
            when Col_Width_Key =>
              for t in Entry_topic loop
                if Index (k'Image, t'Image) > 0 then
                  R (Integer'Wide_Image (opt.column_width (t)));
                end if;
              end loop;
            when Col_Visible_Key =>
              for t in Entry_topic loop
                if Index (k'Image, t'Image) > 0 then
                  R (Boolean'Wide_Image (opt.visible_column (t)));
                end if;
              end loop;
            when sort_column =>
              R (Integer'Wide_Image (opt.sort_column));
            when sort_direction =>
              R (Sort_Direction_Type'Wide_Image (opt.sort_direction));
            when win_left =>
              R (Integer'Wide_Image (opt.win_left));
            when win_top =>
              R (Integer'Wide_Image (opt.win_top));
            when win_width =>
              R (Integer'Wide_Image (opt.win_width));
            when win_height =>
              R (Integer'Wide_Image (opt.win_height));
            when maximized =>
              R (Boolean'Wide_Image (opt.MDI_main_maximized));
            when children_maximized =>
              R (Boolean'Wide_Image (opt.MDI_childen_maximized));
            when tree_portion =>
              R (Float'Wide_Image (opt.tree_portion));
            when mru1 .. mru9 =>
              R (To_Wide_String (opt.mru (Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1)));
            when show_passwords =>
              R (Boolean'Wide_Image (opt.show_passwords));
            when ignore_extract_path =>
              R (Boolean'Wide_Image (opt.ignore_extract_path));
            when extract_directory =>
              R (To_Wide_String (opt.suggested_extract_directory));
            when temp_directory =>
              R (To_Wide_String (opt.temp_directory));
            when first_visit =>
              R (Boolean'Wide_Image (opt.first_visit));
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
