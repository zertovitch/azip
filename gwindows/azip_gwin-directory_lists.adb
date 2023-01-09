with AZip_Common;

with AZip_GWin.MDI_Child,
     AZip_GWin.MDI_Main;

with GWindows.Menus;
--  with GWindows.Message_Boxes;

with Ada.Calendar,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation;

package body AZip_GWin.Directory_Lists is

  use AZip_Common;
  use AZip_GWin.MDI_Child, AZip_GWin.MDI_Main;

  procedure On_Item_Changed (Control : in out Directory_list_type) is
  begin
    if Control.refreshing then
      --  Avoid call to Update_Information, item by item, during a full refresh...
      null;
    else
      declare
        PW : MDI_Child_Type renames MDI_Child_Type (Control.Parent.Parent.Parent.all);
      begin
        PW.Update_Information (status_bar);
      end;
    end if;
  end On_Item_Changed;

  function On_Compare
    (Control   : in Directory_list_type;
     Column    : in Natural;
     Payload_1 : in LV_Payload;
     Payload_2 : in LV_Payload) return Integer
  is
    use Interfaces;
    n1 : AZip_Common.UTF_16_Unbounded_String renames Payload_1.name;
    n2 : AZip_Common.UTF_16_Unbounded_String renames Payload_2.name;
    x1 : AZip_Common.UTF_16_Unbounded_String renames Payload_1.extension;
    x2 : AZip_Common.UTF_16_Unbounded_String renames Payload_2.extension;
    p1 : AZip_Common.UTF_16_Unbounded_String renames Payload_1.path;
    p2 : AZip_Common.UTF_16_Unbounded_String renames Payload_2.path;
    a1 : Boolean renames Payload_1.read_only;
    a2 : Boolean renames Payload_2.read_only;
    e1 : Zip.Zip_name_encoding renames Payload_1.name_encoding;
    e2 : Zip.Zip_name_encoding renames Payload_2.name_encoding;
    f1 : Zip.PKZip_method renames Payload_1.format;
    f2 : Zip.PKZip_method renames Payload_2.format;
    t1 : Integer renames Payload_1.result_code;
    t2 : Integer renames Payload_2.result_code;
    s1 : Integer_64 renames Payload_1.uncompressed_size;
    s2 : Integer_64 renames Payload_2.uncompressed_size;
    k1 : Integer_64 renames Payload_1.compressed_size;
    k2 : Integer_64 renames Payload_2.compressed_size;
    u1 : Unsigned_32 renames Payload_1.crc_32;
    u2 : Unsigned_32 renames Payload_2.crc_32;
    r1 : Long_Float renames Payload_1.ratio;
    r2 : Long_Float renames Payload_2.ratio;
    d1 : Zip_Streams.Time renames Payload_1.date_time;
    d2 : Zip_Streams.Time renames Payload_2.date_time;
    --
    use type AZip_Common.UTF_16_Unbounded_String;
    use Zip_Streams, Zip_Streams.Calendar;
    use Zip;
    --
    less    : constant := -1;
    greater : constant := +1;
    equal   : constant :=  0;
  begin
    case Control.curr_col_topic (Column) is
      --  Comparison strategy: for topics, such as file names or sizes, where identical values for
      --  pairs of items are likely to be rare, we start with an inequality test:
      when Name       => if n1 > n2 then return greater; elsif n1 = n2 then return equal; else return less; end if;
      when Size       => if s1 > s2 then return greater; elsif s1 = s2 then return equal; else return less; end if;
      when Packed     => if k1 > k2 then return greater; elsif k1 = k2 then return equal; else return less; end if;
      when Modified   => if d1 > d2 then return greater; elsif d1 = d2 then return equal; else return less; end if;
      when CRC32      => if u1 > u2 then return greater; elsif u1 = u2 then return equal; else return less; end if;
      when Format     => if f1 > f2 then return greater; elsif f1 = f2 then return equal; else return less; end if;
      when Encoding   => if e1 > e2 then return greater; elsif e1 = e2 then return equal; else return less; end if;
      when Result     => if t1 > t2 then return greater; elsif t1 = t2 then return equal; else return less; end if;
      --  For the following items, we make an equality test first, since it is the most likely case:
      when FType      => if x1 = x2 then return equal; elsif x1 > x2 then return greater; else return less; end if;
      when Path       => if p1 = p2 then return equal; elsif p1 > p2 then return greater; else return less; end if;
      when Attributes => if a1 = a2 then return equal; elsif a1 > a2 then return greater; else return less; end if;
      --  Equality test on floats is a BAD thing; we avoid it:
      when Ratio      => if r1 > r2 then return greater; elsif r1 < r2 then return less; else return equal; end if;
    end case;
  end On_Compare;

  procedure Sort
    (Control    : in out Directory_list_type;
     Column     : in     Natural;
     Direction  : in     AZip_LV_Ex.Sort_Direction_Type;
     Show_Icon  : in     Boolean := True;
     Technique  : in     AZip_LV_Ex.Comparison_Technique_Type := AZip_LV_Ex.As_Strings)
  is
    use Ada.Calendar;
    t0, t1 : Ada.Calendar.Time;
    window : MDI_Child_Type renames MDI_Child_Type (Control.Parent.Parent.Parent.all);
    use AZip_LV_Ex;
  begin
    if timing then
      t0 := Clock;
    end if;
    --  Get the inverse function of opt.column_index:
    for t in Entry_topic loop
      Control.curr_col_topic (window.opt.column_index (t) - 1) := t;
    end loop;
    --  Call parent method, but with the `Using_Payloads` comparison technique,
    --  which bypasses any API call. Parameter `Technique` is ignored.
    Ex_List_View_Control_Type (Control).Sort (Column, Direction, Show_Icon, Using_Payloads);
    --
    if timing then
      t1 := Clock;
      MDI_Child_Type (Control.Parent.Parent.Parent.all).Status_Bar.Text
        ("Sorting time:" & Duration'Wide_Image (t1 - t0));
    end if;
  end Sort;

  overriding procedure On_Focus (Control : in out Directory_list_type) is
    Child_Window : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    Child_Window.Update_status_bar;
    Child_Window.Update_tool_bar_and_menus;
  end On_Focus;

  overriding procedure On_Notify
    (Window       : in out Directory_list_type;
     Message      : in     GWindows.Base.Pointer_To_Notification;
     Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
     Return_Value : in out GWindows.Types.Lresult)
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

  overriding procedure On_Free_Payload
    (Control : in out Directory_list_type;
     Payload :    out AZip_LV_Ex.Data_Access)
  is
    procedure Dispose is new Ada.Unchecked_Deallocation (LV_Payload, AZip_LV_Ex.Data_Access);
  begin
    Dispose (Payload);
  end On_Free_Payload;

  overriding procedure On_Right_Click (Control : in out Directory_list_type) is
    Child_Window : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.Parent.all);
  begin
    GWindows.Menus.Immediate_Popup_Menu (Child_Window.context_menu_file, Child_Window);
  end On_Right_Click;

end AZip_GWin.Directory_Lists;
