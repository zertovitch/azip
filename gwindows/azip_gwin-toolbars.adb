with AZip_Resource_GUI;

with GWindows.Base,
     GWindows.Menus;

with GWin_Util;

with Interfaces.C;

package body AZip_GWin.Toolbars is

  use GWindows.Image_Lists, Interfaces.C;

  --  How to Display Tooltips for Buttons (Windows)
  --  http://msdn.microsoft.com/en-us/library/windows/desktop/hh298386(v=vs.85).aspx

  TBSTYLE_TOOLTIPS : constant := 16#0100#;
  TBSTYLE_FLAT     : constant := 16#0800#;
  TBSTYLE_LIST     : constant := 16#1000#;

  TBSTYLE_EX_MIXEDBUTTONS : constant := 8;

  sep_width : constant := 8;

  procedure Init_Main_Toolbar
    (tb     : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
     il     : in out GWindows.Image_Lists.Image_List_Type;
     parent : in out AZip_GWin.MDI_Main.MDI_Main_Type)
  is
    use AZip_Resource_GUI;
    string_count : Natural := 0;
    Fake_Menu : Menu_MDI_Child_Type;
    Super_Fake_Menu : Fake_menu_for_commands_in_no_real_menu_Type;
    --
    procedure Add_Button_with_Tip
      (Image_Index : in     Natural;
       Command_ID  : in     Integer)
    is
      use GWin_Util, GWindows.Common_Controls, GWindows.Menus;
    begin
      --  The tool tip is the menu text.
      declare
        tip1 : constant GString := Text (Fake_Menu.Main, Command, Command_ID);
        tip2 : constant GString := Text (Super_Fake_Menu.Main, Command, Command_ID);
      begin
        if tip1 = "" then
          tb.Add_String (Menu_Entry_Title_to_Toolbar_Label (tip2));
        else
          tb.Add_String (Menu_Entry_Title_to_Toolbar_Label (tip1));
        end if;
      end;
      tb.Add_Button (Image_Index, Command_ID, string_count);
      string_count := string_count + 1;
    end Add_Button_with_Tip;

    use GWindows.Common_Controls;
    st : Interfaces.C.unsigned;
  begin
    Create (tb, parent, 0, 0, 0, 40);
    Dock (tb, GWindows.Base.At_Top);

    Create (il, Num_resource (Toolbar_BMP), 32);
    Set_Image_List (tb, il);
    st := Get_Style (tb);
    Set_Style (tb, TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    Set_Extended_Style (tb, TBSTYLE_EX_MIXEDBUTTONS);

    Create_Full_Menu (Fake_Menu);
    Create_Full_Menu (Super_Fake_Menu);
    --  "New Archive" is seldom used -> out of the toolbar, stays in the menu.
    --  Add_Button_with_Tip (10, IDM_NEW_ARCHIVE);
    Add_Button_with_Tip (11, IDM_OPEN_ARCHIVE);
    Add_Button_with_Tip  (2, IDM_EXTRACT);
    Add_Separator (tb, sep_width);
    Add_Button_with_Tip  (0, IDM_ADD_FILES);
    Add_Button_with_Tip  (6, IDM_Add_Files_Encryption);
    Add_Button_with_Tip  (1, IDM_Delete_selected);
    Add_Separator (tb, sep_width);
    Add_Button_with_Tip  (4, IDM_TEST_ARCHIVE);
    Add_Button_with_Tip  (3, IDM_FIND_IN_ARCHIVE);
    Add_Separator (tb, sep_width);
    Add_Button_with_Tip  (5, IDM_UPDATE_ARCHIVE);
    Add_Button_with_Tip  (9, IDM_RECOMPRESS_ARCHIVE);
    Add_Separator (tb, sep_width);
    Add_Button_with_Tip (12, IDM_Toggle_Flat_Tree_View);
    Add_Separator (tb, sep_width);
    Add_Button_with_Tip  (7, IDM_Properties);
  end Init_Main_Toolbar;

end AZip_GWin.Toolbars;
