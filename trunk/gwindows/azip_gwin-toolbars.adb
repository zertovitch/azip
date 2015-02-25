with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows;                          use GWindows;
with GWindows.Base;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Menus;                    use GWindows.Menus;

with Interfaces.C;

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
        when '&' | GCharacter'Val(0) =>
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

  -- How to Display Tooltips for Buttons (Windows)
  -- http://msdn.microsoft.com/en-us/library/windows/desktop/hh298386(v=vs.85).aspx

  TBSTYLE_TOOLTIPS : constant:= 16#100#;
  TBSTYLE_FLAT     : constant:= 16#800#;
  TBSTYLE_LIST     : constant:= 16#00001000#;

  TBSTYLE_EX_MIXEDBUTTONS     : constant:= 16#00000008#;

  sep_width: constant:= 8;

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
    string_count: Natural:= 0;
    Fake_Menu: Menu_MDI_Child_Type;
    --
    procedure Add_Button_with_Tip
      (Image_Index : in     Natural;
       Command_ID  : in     Integer)
    is
      use GWindows.Common_Controls;
      use type GString_Unbounded;
    begin
      -- The tool tip is the menu text.
      tb.Add_String(Filter(Text(Fake_Menu.Main, Command, Command_ID)));
      tb.Add_Button(Image_Index, Command_ID, string_count);
      string_count:= string_count + 1;
    end Add_Button_with_Tip;

    use GWindows.Common_Controls;
    st: Interfaces.C.unsigned;
  begin
    Create (tb, parent, 0, 0, 0, 40);
    Dock (tb, GWindows.Base.At_Top);

    Create (il, Num_resource(Toolbar_Bmp), 32);
    Set_Image_List (tb, il);
    st:= Get_Style(tb);
    Set_Style(tb, TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    Set_Extended_Style(tb, TBSTYLE_EX_MIXEDBUTTONS);

    Create_Full_Menu(Fake_Menu);
    Add_Button_with_Tip (2, IDM_EXTRACT);
    Add_Separator(tb, sep_width);
    Add_Button_with_Tip (0, IDM_ADD_FILES);
    Add_Button_with_Tip (6, IDM_Add_Files_Encryption);
    Add_Button_with_Tip (1, IDM_Delete_selected);
    Add_Separator(tb, sep_width);
    Add_Button_with_Tip (4, IDM_TEST_ARCHIVE);
    Add_Button_with_Tip (3, IDM_FIND_IN_ARCHIVE);
    Add_Separator(tb, sep_width);
    Add_Button_with_Tip (5, IDM_UPDATE_ARCHIVE);
  end Init_Main_toolbar;

end AZip_GWin.Toolbars;
