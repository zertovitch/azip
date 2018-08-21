package body AZip_Common.User_options is

  package body Persistence is

    procedure Load(opt: out Option_Pack_Type) is
      defaults: Option_Pack_Type;
    begin
      for k in Key loop
        begin
          declare
            ks: constant Wide_String:= Key'Wide_Image(k);
            s : constant Wide_String:= Read_key(ks);
          begin
            case k is
              when view_mode =>
                opt.view_mode:= View_Mode_Type'Wide_Value(s);
              when col_width =>
                for e in Entry_topic loop
                  opt.column_width(e):= Integer'Wide_Value(
                    Read_key(ks & "_" & Entry_topic'Wide_Image(e))
                  );
                  if opt.column_width(e) = 0 then
                    opt.column_width(e):= defaults.column_width(e);
                  end if;
                end loop;
              when sort_column =>
                opt.sort_column:= Integer'Wide_Value(s);
              when sort_direction =>
                opt.sort_direction:= Sort_Direction_Type'Wide_Value(s);
              when win_left =>
                opt.win_left:= Integer'Wide_Value(s);
              when win_top =>
                opt.win_top:= Integer'Wide_Value(s);
              when win_width =>
                opt.win_width:= Integer'Wide_Value(s);
              when win_height =>
                opt.win_height:= Integer'Wide_Value(s);
              when maximized =>
                opt.MDI_main_maximized:= Boolean'Wide_Value(s);
              when children_maximized =>
                opt.MDI_childen_maximized:= Boolean'Wide_Value(s);
              when tree_portion =>
                opt.tree_portion:= Float'Wide_Value(s);
              when mru1..mru9 =>
                opt.mru( Key'Pos(k)-Key'Pos(mru1)+1 ):=
                  To_Unbounded_Wide_String(s);
              when show_passwords =>
                opt.show_passwords:= Boolean'Wide_Value(s);
              when ignore_extract_path =>
                opt.ignore_extract_path:= Boolean'Wide_Value(s);
            end case;
          end;
        exception
          when others =>
            null;
            -- This key is missing (from registry, config file,...)
            -- Data_Error or something else.
            -- We just continue...
        end;
      end loop;
    end Load;

    procedure Save(opt: in Option_Pack_Type) is
    begin
      for k in Key loop
        declare
          ks: constant Wide_String:= Key'Wide_Image(k);
          procedure R( v: Wide_String ) is
          begin
            Write_key(ks, v);
          end R;
        begin
          case k is
            when view_mode =>
              R(View_Mode_Type'Wide_Image(opt.view_mode));
            when col_width =>
              R("(root - dummy)"); -- dummy for having reading going well
              for e in Entry_topic loop
                Write_key(
                  ks & "_" & Entry_topic'Wide_Image(e),
                  Integer'Wide_Image(opt.column_width(e))
                );
              end loop;
            when sort_column =>
              R(Integer'Wide_Image(opt.sort_column));
            when sort_direction =>
              R(Sort_Direction_Type'Wide_Image(opt.sort_direction));
            when win_left =>
              R(Integer'Wide_Image(opt.win_left));
            when win_top =>
              R(Integer'Wide_Image(opt.win_top));
            when win_width =>
              R(Integer'Wide_Image(opt.win_width));
            when win_height =>
              R(Integer'Wide_Image(opt.win_height));
            when maximized =>
              R(Boolean'Wide_Image(opt.MDI_main_maximized));
            when children_maximized =>
              R(Boolean'Wide_Image(opt.MDI_childen_maximized));
            when tree_portion =>
              R(Float'Wide_Image(opt.tree_portion));
            when mru1..mru9 =>
              R( To_Wide_String(opt.mru( Key'Pos(k)-Key'Pos(mru1)+1 )) );
            when show_passwords =>
              R(Boolean'Wide_Image(opt.show_passwords));
            when ignore_extract_path =>
              R(Boolean'Wide_Image(opt.ignore_extract_path));
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

end AZip_Common.User_options;
