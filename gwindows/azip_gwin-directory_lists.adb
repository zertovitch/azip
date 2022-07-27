with AZip_Common.Operations;            use AZip_Common, AZip_Common.Operations;

with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Menus;                    use GWindows.Menus;
--  with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Calendar;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body AZip_GWin.Directory_Lists is

  procedure On_Item_Changed (Control : in out Directory_list_type) is
  begin
    if Control.refreshing then
      --  Avoid call to Update_display, item by item, during a full refresh...
      null;
    else
      declare
        PW : MDI_Child_Type renames MDI_Child_Type (Control.Parent.Parent.Parent.all);
      begin
        PW.Update_display (status_bar);
      end;
    end if;
  end On_Item_Changed;

  function On_Compare (
               Control : in Directory_list_type;
               Column  : in Natural;
               Index_1 : in Natural;
               Index_2 : in Natural
  )
  return Integer
  is
    use Interfaces;
    i1, i2  : Integer;
    s1, s2  : Integer_64;
    r1, r2  : Long_Float;
    less    : constant := -1;
    greater : constant := +1;
    equal   : constant :=  0;
  begin
    --  Comparison strategy: for topics where identical values are likely
    --  to be rare, such as file sizes, we start with an inequality test.
    case Control.curr_col_topic (Column) is
      when Size =>
        s1 := Control.Item_Data (Index_1).uncompressed_size;
        s2 := Control.Item_Data (Index_2).uncompressed_size;
        if s1 > s2 then
          return greater;
        elsif s1 = s2 then
          return equal;
        else
          return less;
        end if;
      when Packed =>
        s1 := Control.Item_Data (Index_1).compressed_size;
        s2 := Control.Item_Data (Index_2).compressed_size;
        if s1 > s2 then
          return greater;
        elsif s1 = s2 then
          return equal;
        else
          return less;
        end if;
      when Ratio =>
        r1 := Control.Item_Data (Index_1).ratio;
        r2 := Control.Item_Data (Index_2).ratio;
        if r1 > r2 then
          return greater;
        elsif r1 < r2 then
          return less;
        else
          --  Equality test on floats is a BAD thing, we avoid it.
          return equal;
        end if;
      when Result =>
        i1 := Control.Item_Data (Index_1).result_code;
        i2 := Control.Item_Data (Index_2).result_code;
        if i1 > i2 then
          return greater;
        elsif i1 = i2 then
          return equal;
        elsif i1 > i2 then
          return greater;
        else
          return less;
        end if;
      when others =>
        --  The sort column has the default behaviour with text comparison.
        declare
          Value_1 : constant GString := Control.Text (Index_1, Column);
          Value_2 : constant GString := Control.Text (Index_2, Column);
        begin
          --  Default behaviour: lexicographic.
          if Value_1 = Value_2 then
            return equal;
          elsif Value_1 > Value_2 then
            return greater;
          else
            return less;
          end if;
        end;
    end case;
  end On_Compare;

  procedure Sort (
    Control    : in out Directory_list_type;
    Column     : in Natural;
    Direction  : in AZip_LV_Ex.Sort_Direction_Type;
    Show_Icon  : in Boolean := True;
    Technique  : in AZip_LV_Ex.Comparison_Technique_Type := AZip_LV_Ex.As_Strings)
  is
    timing : constant Boolean := False;
    use Ada.Calendar;
    t0, t1 : Ada.Calendar.Time;
    window : MDI_Child_Type renames MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    if timing then
      t0 := Clock;
    end if;
    --  Get the inverse of opt.column_index:
    for t in Entry_topic loop
      Control.curr_col_topic (window.opt.column_index (t) - 1) := t;
    end loop;
    --  Call parent method, but with the `General` comparison technique,
    --  which avoids strings if possible.
    AZip_LV_Ex.Ex_List_View_Control_Type (Control).Sort
      (Column, Direction, Show_Icon, AZip_LV_Ex.General);
    --
    if timing then
      t1 := Clock;
      MDI_Child_Type (Control.Parent.Parent.Parent.all).Status_Bar.Text (
        Duration'Wide_Image (t1 - t0)
      );
    end if;
  end Sort;

  overriding procedure On_Focus (Control : in out Directory_list_type) is
    Child_Window : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    Child_Window.Update_status_bar;
    Child_Window.Update_tool_bar_and_menus;
  end On_Focus;

  overriding procedure On_Notify (
      Window       : in out Directory_list_type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
  )
  is
    LVN_FIRST      : constant := -100;
    LVN_BEGINDRAG  : constant := LVN_FIRST - 9;
    --  WM_CONTEXTMENU : constant := 16#7B#;
    Child_Window : MDI_Child_Type renames MDI_Child_Type (Window.Parent.Parent.Parent.all);
    Main_Window  : MDI_Main_Type  renames Child_Window.MDI_Root.all;
  begin
    --  Call parent method
    AZip_LV_Ex.Ex_List_View_Control_Type (Window).On_Notify
       (Message, Control, Return_Value);
    case Message.Code is
      when LVN_BEGINDRAG =>
        Window.Focus;
        Capture_Mouse (Child_Window);
        Main_Window.dragging.is_dragging := True;
        --  The rest of the dragging operation is handled by the parent window, of
        --  type MDI_Child_Type: see On_Mouse_Move, On_Left_Mouse_Button_Up.
      --
      --  when WM_CONTEXTMENU =>  --  Documented but doesn't work.
      --    Message_Box ("Key", "Context menu key via WM_CONTEXTMENU");
      --    Immediate_Popup_Menu (MDI_Child.context_menu_file, MDI_Child);
      when others =>
        null;
    end case;
  end On_Notify;

  overriding procedure On_Free_Payload (
    Control : in out Directory_list_type;
    Payload :    out AZip_LV_Ex.Data_Access
  )
  is
    procedure Dispose is new Ada.Unchecked_Deallocation (LV_Payload, AZip_LV_Ex.Data_Access);
  begin
    Dispose (Payload);
  end On_Free_Payload;

  overriding procedure On_Right_Click (Control : in out Directory_list_type) is
    Child_Window : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    Immediate_Popup_Menu (Child_Window.context_menu_file, Child_Window);
  end On_Right_Click;

end AZip_GWin.Directory_Lists;
