with Zip;                               use Zip;
with AZip_Common;                       use AZip_Common;
with Time_Display;

with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Interfaces;

package body AZip_GWin.MDI_Child is

  function S2G (Value : String) return GString renames To_GString_From_String;

  procedure Update_display(
    Window : in out MDI_Child_Type;
    need   :        Update_need
  )
  is
    row: Natural:= 0;
    procedure Action(
      name             : String; -- 'name' is compressed entry's name
      file_index       : Positive;
      comp_size        : File_size_type;
      uncomp_size      : File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      unicode_file_name: Boolean
    )
    is
    begin
      Window.Directory_List.Insert_Item(S2G(name), row);
      Window.Directory_List.Set_Sub_Item(S2G(Time_Display(Convert(date_time))), row, 2);
      Window.Directory_List.Set_Sub_Item(S2G(To_Lower(PKZip_method'Image(method))), row, 7);
      row:= row + 1; -- more subtle with our sorting
    end Action;

    procedure Traverse is new Zip.Traverse_verbose(Action);

  begin
    case Window.current_options.view_mode is
      when Flat =>
        Window.Folder_Tree.Hide;
        Window.Directory_List.Clear;
        for topic in Entry_topic loop
          if need = first_display then
            Window.Directory_List.Insert_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.current_options.column_width(topic)
            );
          else
            Window.Directory_List.Set_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.current_options.column_width(topic)
            );
          end if;
        end loop;
        if Is_Loaded(Window.zif) and need <= archive_changed then
          Traverse(Window.zif);
        end if;
      when Tree =>
        -- all stuff
        Window.Folder_Tree.Show;
    end case;
  end Update_display;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
  begin
    Small_Icon (Window, "Picture_Icon");

    -- Filial feelings:
    Window.parent:= MDI_Main_Access(Controlling_Parent(Window));

    Create(Window.Directory_List, Window, 50,1,20,20, Multiple, Report_View);
    Create(Window.Folder_Tree, Window, 1,1,20,20);

    -- Window.Draw_Control.parent:=
    --   MDI_Picture_Child_Access(Controlling_Parent(Window.Draw_Control));

    -- Background_Mode (Window.Draw_Control.Drawing_Area, Transparent);
    -- Background_Mode (Window.Draw_Control.Saved_Area, Transparent);

    -- On_Change_Cursor_Handler (Window.Draw_Control, Do_Change_Cursor'Access);
    -- On_Left_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Left_Mouse_Down'Access);
    -- On_Right_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Right_Mouse_Down'Access);
    -- On_Left_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
    -- On_Right_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
    -- On_Mouse_Move_Handler (Window.Draw_Control, Do_Mouse_Move'Access);
    -- On_Mouse_Wheel_Handler (Window, Do_Mouse_Wheel'Access);
    --
    -- On_Character_Down_Handler (Window, Do_Key_Down'Access); -- 14-Oct-2005

    -- Create_Compatible_Bitmap (Window.Draw_Control.Drawing_Area,
    --                           Window.Draw_Control.Saved_Bitmap,
    --                           Desktop_Width,
    --                           Desktop_Height);
    -- Select_Object (Window.Draw_Control.Saved_Area,
    --                Window.Draw_Control.Saved_Bitmap);

    --Refresh_size_dependent_parameters(
    --  Window.Draw_Control.Picture,
    --  objects => True
    --);
    --Subtle_redraw (Window.Draw_Control);

    Dock_Children (Window);

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 5);

    -- 2007: Maximize-demaximize (non-maximized case) to
    -- avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:= not Window.parent.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Freeze(Window.parent.all);
        Zoom(Window);
      end if;
      On_Size(Window,Width(Window),Height(window));
      if memo_unmaximized_children then
        Thaw(Window.parent.all); -- Before Zoom, otherwise uncomplete draw.
        Zoom(Window, False);
        -- Window.parent.Tool_Bar.Redraw;
      end if;
    end;

    --Scroll_Position(Window, Vertical, Scroll_Maximum(Window, Vertical));
    --Adjust_Draw_Control_Position(Window);

    Window.Status_deamon.Start;
    Window.Status_deamon.Display(Window'Unchecked_Access);
    Update_display(Window, first_display);
    Window.Use_Mouse_Wheel;
  end On_Create;

  procedure On_Save (Window : in out MDI_Child_Type) is
  begin
    null; -- nothing to be saved in this application
  end On_Save;

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean is
    pragma Unreferenced (Window);
  begin
    return True;
  end Is_file_saved;

  -- This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" ) is
  begin
    Update_Common_Menus( Window.parent.all, top_entry );
  end Update_Common_Menus;

  procedure Reload_archive (Window : in out MDI_Child_Type) is
  begin
    Zip.Load(Window.zif, GWindows.GStrings.To_String(To_GString_From_Unbounded(Window.File_Name)));
    Update_display(Window, archive_changed);
  end Reload_archive;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);   -- only client area is considered
    pragma Warnings (Off, Height);  -- only client area is considered
    w: Natural:= Window.Client_Area_Width;
    h: Natural:= Window.Client_Area_Height;
  begin
    case Window.current_options.view_mode is
      when Flat =>
        Window.Directory_List.Location(
            GWindows.Types.Rectangle_Type'
            (0, 0, w, h)
        );
      when Tree =>
        Window.Folder_Tree.Location(
            GWindows.Types.Rectangle_Type'
            (0, 0, w/2, h)
        );
        Window.Directory_List.Location(
            GWindows.Types.Rectangle_Type'
            (w/2+1, 0, w, h)
        );
        null;
    end case;
  end On_Size;

  procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean) is
  begin
    Can_close:= True;
    if Is_file_saved(Window) then
      Update_Common_Menus(Window,To_GString_from_Unbounded(Window.File_Name));
      Window.Status_deamon.Stop;
    else -- This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                To_GString_from_Unbounded(Window.Short_Name) & "' ?",
                Yes_No_Cancel_Box,
                Exclamation_Icon)
        is
          when Yes    => On_Save(Window);
                         exit when Is_file_saved(Window);
          when No     => exit;
          when Cancel => Window.parent.Success_In_Enumerated_Close:= False;
                         Can_close:= False;
                         exit;
          when others => null;
        end case;
      end loop;
    end if;
  end On_Close;

  package body Daemons is
      ---------------------------------------------------------------------------
      -- This background task displays on demand (also from another task)      --
      -- informations about archive and various status.                        --
      ---------------------------------------------------------------------------

      task body Status_display is
         current_child_window: AZip_GWin.MDI_Child.MDI_Child_Access;
      begin
         accept Start;
         loop
            select
               accept Stop;
               exit;
            or
               accept Display(w: AZip_GWin.MDI_Child.MDI_Child_Access) do
                  current_child_window:= w;
               end Display;
               -- Update_status_display(current_main_window.all);
            or
               delay 0.05; -- relax
            end select;
         end loop;
      end Status_display;

      -----------------------------------------------------------------
      -- This background task calls an archive test on demand        --
      -- and ensures display of its progress.                        --
      -----------------------------------------------------------------

      task body Testing_type is
         current_child_window: AZip_GWin.MDI_Child.MDI_Child_Access;
      begin
         accept Start;
         loop
            select
               accept Stop;
               exit;
            or
               accept Test(w: AZip_GWin.MDI_Child.MDI_Child_Access) do
                  current_child_window:= w;
               end Test;
               -- (perform test)
            or
               delay 0.05; -- relax
            end select;
         end loop;
      end Testing_type;

   end Daemons;

end AZip_GWin.MDI_Child;
