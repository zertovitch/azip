package body AZip_Common.User_options is

  package body Persistence is

    type Key is
      (
        view_mode,
        col_width,
        maximized,
        children_maximized
      );


    procedure Load(opt: out Option_Pack_Type) is
    begin
      for k in Key loop
        begin
          declare
            ks: constant String:= key'Image(k);
            s : constant String:= Read_Key(ks);
          begin
            case k is
              when view_mode =>
                opt.view_mode:= View_Mode_Type'Value(s);
              when col_width =>
                null; -- !!
              when maximized =>
                opt.MDI_main_maximized:= Boolean'Value(s);
              when children_maximized =>
                opt.MDI_childen_maximized:= Boolean'Value(s);
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
          ks: constant String:= key'Image(k);
          procedure R( v: String ) is
          begin
            Write_key(ks, v);
          end R;
        begin
          case k is
            when view_mode =>
              R(View_Mode_Type'Image(opt.view_mode));
            when col_width =>
              null; -- !!
            when maximized =>
              R(Boolean'Image(opt.MDI_main_maximized));
            when children_maximized =>
              R(Boolean'Image(opt.MDI_childen_maximized));
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

end AZip_Common.User_options;
