with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows;                          use GWindows;
with GWindows.Base;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Menus;                    use GWindows.Menus;

with Interfaces.C;
with GWindows.Types;

package body AZip_GWin.Toolbars is

  use GWindows.Image_Lists, Interfaces.C;

  -- Filter & and \t
  -- Not having TTS_NO_PREFIX in tool_tip creation would do it as well.
  function Filter(s: GString) return GString is
    use type GString_Unbounded;
    u: GString_Unbounded;
  begin
    for i in s'Range loop
      case s(i) is
        when '&' =>
          null;
        when GCharacter'Val(9) => -- Tab
          exit;
        when '\' =>
          exit when i < s'Last and then s(i+1)= 't';
        when others =>
          u:= u & GString'(1=> s(i));
      end case;
    end loop;
    return To_GString_From_Unbounded(u);
  end;

  procedure Add_Button_with_Tip
    (Control     : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
     parent      : in out AZip_GWin.MDI_Main.MDI_Main_Type;
     Image_Index : in     Natural;
     Command_ID     : in     Integer)
  is
    use GWindows.Common_Controls;
    Fake_Menu: Menu_MDI_Child_Type;
  begin
    Create_Full_Menu(Fake_Menu);
    Control.Add_Button(Image_Index, Command_ID);
    Parent.Tool_Bar_Tips.Add_Tool_Tip(
      Parent.zzz,
      -- avoid the toolbar itself, since it would show the same tip
      -- on the whole bar.
      Tip => Filter(Text(Fake_Menu.Main, Command, Command_ID))
    );
  end Add_Button_with_Tip;

  TBSTYLE_TOOLTIPS : constant:= 16#100#;
  TBSTYLE_FLAT     : constant:= 16#800#;
  sep_w: constant:= 8;

  function Num_resource(id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_from_String('#' & img(img'first+1..img'Last));
  end Num_resource;



  procedure Init_Main_toolbar(
    tb    : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out AZip_GWin.MDI_Main.MDI_Main_Type
  )
  is
    use GWindows.Common_Controls;
    st: Interfaces.C.unsigned;

    procedure Assoc(tt: GWindows.Common_Controls.Tool_Tip_Type) is
    WM_USER        : constant := 16#400#;
    TB_SETTOOLTIPS : constant := WM_USER + 36;
      procedure SendMessage_Associate_TT
        (hwnd   : GWindows.Types.Handle := Handle (tb);
         uMsg   : Interfaces.C.int      := TB_SETTOOLTIPS;
         wParam : GWindows.Types.Handle := Handle (tt);
         lParam : GWindows.Types.Wparam := 0);
      pragma Import (StdCall, SendMessage_Associate_TT,
                     "SendMessage" & Character_Mode_Identifier);
    begin
      SendMessage_Associate_TT;
    end assoc;

  begin
    Create (tb, parent, 0, 0, 0, 40);
    Dock (tb, GWindows.Base.At_Top);

    Create (il, Num_resource(Toolbar_Bmp), 32);
    Set_Image_List (tb, il);
    st:= Get_Style(tb);
    Set_Style(tb, TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or st);

    Parent.Tool_Bar_Tips.Create(Parent);
    Parent.Tool_Bar_Tips.Maximum_Width(150);
    --
    Assoc(Parent.Tool_Bar_Tips);
    --
    Add_Button_with_Tip (tb, parent,  2, IDM_EXTRACT);
    Add_Separator(tb, sep_w);
    Add_Button_with_Tip (tb, parent,  0, IDM_ADD_FILES);
    Add_Button_with_Tip (tb, parent,  1, IDM_Delete_selected);
    Add_Separator(tb, sep_w);
    Add_Button_with_Tip (tb, parent,  3, IDM_FIND_IN_ARCHIVE);


  end Init_Main_toolbar;

end AZip_GWin.Toolbars;
