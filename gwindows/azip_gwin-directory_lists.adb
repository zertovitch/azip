with AZip_Common.Operations;            use AZip_Common, AZip_Common.Operations;

with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Menus;                    use GWindows.Menus;
--  with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Calendar;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;

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

  function On_Compare(
               Control: in Directory_list_type;
               Column : in Natural;
               Value1 : in GString;
               Value2 : in GString
  )
  return Integer
  is
    i1, i2  : Integer;
    less    : constant := -1;
    greater : constant := +1;
    equal   : constant :=  0;
  begin
    case Control.curr_col_topic (Column) is
      when Size | Packed =>  --  E.g. 3 KB
        i1:= Integer(File_Size_Value(Value1));
        i2:= Integer(File_Size_Value(Value2));
        if i1 = i2 then
          return equal;
        elsif i1 > i2 then
          return greater;
        else
          return less;
        end if;
      when Ratio =>  --  E.g. 77%
        i1:= Pct_Value(Value1);
        i2:= Pct_Value(Value2);
        if i1 = i2 then
          return equal;
        elsif i1 > i2 then
          return greater;
        else
          return less;
        end if;
      when Result =>  --  E.g. 1234
        --  Message_Box("Falk forever", "Waaaah!");
        i1:= Result_value(Value1);
        i2:= Result_value(Value2);
        if i1 = i2 then
          return equal;
        elsif i1 > i2 then
          return greater;
        else
          return less;
        end if;
      when others =>
        null;  --  The sort column has the default behaviour.
    end case;
    --  Default behaviour: lexicographic.
    if Value1 = Value2 then
      return equal;
    elsif Value1 > Value2 then
      return greater;
    else
      return less;
    end if;
  end On_Compare;

  overriding procedure Sort(
    Control   : in out Directory_list_type;
    Column    : in Natural;
    Direction : in AZip_LV_Ex.Sort_Direction_Type;
    Show_Icon : in Boolean := True)
  is
    timing : constant Boolean := False;
    use Ada.Calendar;
    t0, t1 : Ada.Calendar.Time;
    window : MDI_Child_Type renames MDI_Child_Type(Control.Parent.Parent.Parent.all);
  begin
    if timing then
      t0 := Clock;
    end if;
    --  Get the inverse of opt.column_index:
    for t in Entry_topic loop
      Control.curr_col_topic(window.opt.column_index(t)-1) := t;
    end loop;
    --  Call parent method
    AZip_LV_Ex.Ex_List_View_Control_Type (Control).Sort(Column, Direction, Show_Icon);
    if timing then
      t1 := Clock;
      MDI_Child_Type(Control.Parent.Parent.Parent.all).Status_Bar.Text (
        Duration'Wide_Image (t1-t0)
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

  --  !! Missing in EX_LV: freeing internal tables on Delete_Item, Clear.
  --    Rem. 20-Aug-2014
  --  !! Missing in EX_LV: a On_Free_Payload that one can override
  --  overriding procedure On_Free_Payload(
  --              Control: in out Directory_list_type;
  --              Payload: out AZip_LV_Ex.Data_access) is
  --   procedure Dispose is new Ada.Unchecked_Deallocation(LV_Payload, AZip_LV_Ex.Data_Access);
  --  begin
  --   Dispose(Payload);
  --  end On_Free_Payload;

  overriding procedure On_Right_Click (Control : in out Directory_list_type) is
    Child_Window : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    Immediate_Popup_Menu (Child_Window.context_menu_file, Child_Window);
  end On_Right_Click;

end AZip_GWin.Directory_Lists;
