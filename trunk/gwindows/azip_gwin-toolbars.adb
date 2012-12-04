with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows;                          use GWindows;
with GWindows.GStrings;                 use GWindows.GStrings;

with Interfaces.C;

package body AZip_GWin.Toolbars is

  use GWindows.Image_Lists, Interfaces.C;

  procedure Add_Button_with_Tip
    (Control     : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
     Parent      : in out GWindows.Base.Base_Window_Type'Class;
     Image_Index : in     Natural;
     Command     : in     Integer)
  is
    function s return GString is
    begin
      case Command is
        when IDM_EXTRACT =>
          return "Extract...";
        when IDM_ADD_FILES =>
          return "Add files...";
        when IDM_Delete_selected =>
          return "Delete selected";
        when IDM_FIND_IN_ARCHIVE =>
          return "Find in archive";
        when others =>
          return "";
      end case;
    end;
    use GWindows.Common_Controls;
    t: Tool_Tip_Type;
  begin
    Control.Add_button(Image_Index, Command);
    t.Create(Parent);
    t.Add_Tool_Tip(Control, Tip => s);
    t.Maximum_Width(150);
  end Add_Button_with_Tip;

  TBSTYLE_FLAT : constant:= 16#800#;
  sep_w: constant:= 8;

  function Num_resource(id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_from_String('#' & img(img'first+1..img'Last));
  end Num_resource;

  procedure Init_Main_toolbar(
    tb    : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out GWindows.Base.Base_Window_Type'Class)
  is
    use GWindows.Common_Controls;
    st: Interfaces.C.unsigned;
  begin
    Create (tb, parent, 0, 0, 0, 40);
    Dock (tb, GWindows.Base.At_Top);

    Create (il, Num_resource(Toolbar_Bmp), 32);
    Set_Image_List (tb, il);
    st:= Get_Style(tb);
    Set_Style(tb, TBSTYLE_FLAT or st);

    Add_Button_with_Tip (tb, parent,  2, IDM_EXTRACT);
    Add_Separator(tb, sep_w);
    Add_Button_with_Tip (tb, parent,  0, IDM_ADD_FILES);
    Add_Button_with_Tip (tb, parent,  1, IDM_Delete_selected);
    Add_Separator(tb, sep_w);
    Add_Button_with_Tip (tb, parent,  3, IDM_FIND_IN_ARCHIVE);
  end Init_Main_toolbar;

end AZip_GWin.Toolbars;
