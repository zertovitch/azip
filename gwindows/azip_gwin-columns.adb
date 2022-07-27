with AZip_Resource_GUI;                 use AZip_Resource_GUI;
with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;

--------------------------------
--  Hiding & showing columns  --
--------------------------------

package body AZip_GWin.Columns is

  --  Number of pixels under which we consider the column as hidden.
  --
  threshold : constant := 20;

  function Get_column_width_from_options (Window : MDI_Child_Type; topic : Entry_topic) return Natural
  is
  begin
    if topic = Path and Window.opt.view_mode = Tree then
      return 0;
    elsif Window.MDI_Root.opt.visible_column (topic) then
      return Integer'Max (threshold, Window.MDI_Root.opt.column_width (topic));
    else
      return 0;
    end if;
  end Get_column_width_from_options;

  procedure Set_column_width_to_options (Window : in out MDI_Child_Type; topic : Entry_topic; new_width : Natural) is
  begin
    if topic = Path and Window.opt.view_mode = Tree then
      null;  --  Do nothing (esp. do not erase width for the Flat view)!
    elsif new_width >= threshold then
      Window.MDI_Root.opt.visible_column (topic) := True;
      Window.MDI_Root.opt.column_width (topic)   := new_width;
    else
      Window.MDI_Root.opt.visible_column (topic) := False;
      --  Don't touch the width: it will be useful when the column is shown again.
    end if;
  end Set_column_width_to_options;

  procedure Get_all_column_widths_from_options (Window : in out MDI_Child_Type) is
  begin
    for t in Entry_topic'Range loop
      Window.Directory_List.Set_Column_Width (
        Window.opt.column_index (t) - 1,
        Get_column_width_from_options (Window, t)
      );
    end loop;
  end Get_all_column_widths_from_options;

  procedure Get_all_column_widths_from_options (Window : MDI_Main_Type) is
    --
    procedure Do_child_window (Child_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Child_Window.all in MDI_Child_Type'Class then
        Get_all_column_widths_from_options (MDI_Child_Type (Child_Window.all));
      end if;
    end Do_child_window;
    --
  begin
    Enumerate_Children
      (MDI_Client_Window (Window).all,
       Do_child_window'Unrestricted_Access);
  end Get_all_column_widths_from_options;

  procedure Set_all_column_widths_to_options (Window : in out MDI_Child_Type) is
  begin
    for t in Entry_topic'Range loop
      Set_column_width_to_options (Window, t, Window.Directory_List.Column_Width (Entry_topic'Pos (t)));
    end loop;
  end Set_all_column_widths_to_options;

  procedure Select_columns_dialog (Window : in out MDI_Main_Type) is
    box : Select_column_box_Type;
    x, y, dy, w, h : Integer;
    check_box_topic : array (Entry_topic) of Check_Box_Type;
    --
    procedure Get_Data (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      for t in Entry_topic'Range loop
        case t is
          when Name | Path =>
            null;  --  Do nothing!
          when others =>
            Window.opt.visible_column (t) := State (check_box_topic (t)) = Checked;
        end case;
      end loop;
      --
      --  Here the big refresh!
      --
      Get_all_column_widths_from_options (Window);
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog (Window);
    box.Center;
    box.Dummy_check_box_1.Hide;
    box.Dummy_check_box_2.Hide;
    x  := Left   (box.Dummy_check_box_1);
    y  := Top    (box.Dummy_check_box_1);
    w  := Width  (box.Dummy_check_box_1);
    h  := Height (box.Dummy_check_box_1);
    --
    dy := Top (box.Dummy_check_box_2) - y;
    --
    for t in Entry_topic loop
      Create (check_box_topic (t), box, Image (t), x, y, w, h);
      y := y + dy;
      case t is
        when Path =>
          if Window.opt.view_mode = Flat then
            State (check_box_topic (t), Checked);    --  Path always visible in Flat mode
          else
            State (check_box_topic (t), Unchecked);  --  Path always hidden in Tree mode
          end if;
          Disable (check_box_topic (t));
        when Name | Result =>
          State (check_box_topic (t), Checked);
          Disable (check_box_topic (t));
        when others =>
          if Window.opt.visible_column (t) then
            State (check_box_topic (t), Checked);
          else
            State (check_box_topic (t), Unchecked);
          end if;
      end case;
      check_box_topic (t).On_Click_Handler (Get_Data'Unrestricted_Access);
    end loop;
    --
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    Show_Dialog (box, Window);
  end Select_columns_dialog;

end AZip_GWin.Columns;
