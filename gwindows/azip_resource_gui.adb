---------------------------------------------------------------------------
-- GUI contents of resource script file: azip.rc
-- Transcription time: 2013/01/18   22:03:17
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 25-Nov-2012
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;                 use GWindows.GStrings;
with System;

package body azip_Resource_GUI is

  -- ** Generated code begins here \/ \/ \/.


  -- Menu at line 25
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Child_Type)
  is
  begin
    Menu.Main:= Create_Menu;
    Menu.Popup_0001:= Create_Popup;
    Append_Menu(Menu.Main, "&File", Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&New archive" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+N", IDM_NEW_ARCHIVE);
    Append_Item(Menu.Popup_0001, "&Open archive..." & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+O", IDM_OPEN_ARCHIVE);
    Append_Item(Menu.Popup_0001, "&Save archive as..." & To_GString_from_String((1=>ASCII.HT)) & "F12", IDM_SAVE_ARCHIVE_AS);
    Append_Item(Menu.Popup_0001, "&Close archive" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+W / Ctrl+F4", IDM_CLOSE_ARCHIVE);
    Append_Separator(Menu.Popup_0001);
    Menu.Popup_0002:= Create_Popup;
    Append_Menu(Menu.Popup_0001, "&Recent", Menu.Popup_0002);
    Append_Item(Menu.Popup_0002, "mru_1", IDM_MRU_1);
    Append_Item(Menu.Popup_0002, "mru_2", IDM_MRU_2);
    Append_Item(Menu.Popup_0002, "mru_3", IDM_MRU_3);
    Append_Item(Menu.Popup_0002, "mru_4", IDM_MRU_4);
    Append_Item(Menu.Popup_0002, "mru_5", IDM_MRU_5);
    Append_Item(Menu.Popup_0002, "mru_6", IDM_MRU_6);
    Append_Item(Menu.Popup_0002, "mru_7", IDM_MRU_7);
    Append_Item(Menu.Popup_0002, "mru_8", IDM_MRU_8);
    Append_Item(Menu.Popup_0002, "mru_9", IDM_MRU_9);
    Append_Separator(Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&Quit" & To_GString_from_String((1=>ASCII.HT)) & "Alt+F4", IDM_QUIT);
    Menu.Popup_0003:= Create_Popup;
    Append_Menu(Menu.Main, "&Edit", Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "Select &all" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+A", IDM_Select_all);
    Append_Item(Menu.Popup_0003, "&Unselect all" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+U", IDM_Unselect_all);
    Append_Item(Menu.Popup_0003, "&Extract..." & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+E", IDM_EXTRACT);
    Append_Separator(Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "Delete entries" & To_GString_from_String((1=>ASCII.HT)) & "Del / -", IDM_Delete_selected);
    Append_Item(Menu.Popup_0003, "A&dd files..." & To_GString_from_String((1=>ASCII.HT)) & "+", IDM_ADD_FILES);
    Menu.Popup_0004:= Create_Popup;
    Append_Menu(Menu.Main, "&Tools", Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&Test archive" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+T", IDM_TEST_ARCHIVE);
    Append_Item(Menu.Popup_0004, "&Find in archive..." & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+F", IDM_FIND_IN_ARCHIVE);
    Append_Separator(Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&Update archive" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+P", IDM_UPDATE_ARCHIVE);
    Append_Item(Menu.Popup_0004, "&Recompress archive", IDM_RECOMPRESS_ARCHIVE);
    State(Menu.Popup_0004, Command, IDM_RECOMPRESS_ARCHIVE, Grayed);
    Append_Separator(Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&Compare archives", IDM_COMPARE_ARCHIVES);
    State(Menu.Popup_0004, Command, IDM_COMPARE_ARCHIVES, Grayed);
    Append_Item(Menu.Popup_0004, "&Merge archives", IDM_MERGE_ARCHIVES);
    State(Menu.Popup_0004, Command, IDM_MERGE_ARCHIVES, Grayed);
    Menu.Popup_0005:= Create_Popup;
    Append_Menu(Menu.Main, "&View", Menu.Popup_0005);
    Append_Item(Menu.Popup_0005, "&Flat view", IDM_FLAT_VIEW);
    Append_Item(Menu.Popup_0005, "&Tree view", IDM_TREE_VIEW);
    State(Menu.Popup_0005, Command, IDM_TREE_VIEW, Grayed);
    Menu.Popup_0006:= Create_Popup;
    Append_Menu(Menu.Main, "&Window", Menu.Popup_0006);
    Append_Item(Menu.Popup_0006, "&Cascade", IDM_WINDOW_CASCADE);
    Append_Item(Menu.Popup_0006, "Tile &Horizontal", IDM_WINDOW_TILE_HORIZONTAL);
    Append_Item(Menu.Popup_0006, "Tile &Vertical", IDM_WINDOW_TILE_VERTICAL);
    Append_Item(Menu.Popup_0006, "&Close All", IDM_WINDOW_CLOSE_ALL);
    Menu.Popup_0007:= Create_Popup;
    Append_Menu(Menu.Main, "&Help", Menu.Popup_0007);
    Append_Item(Menu.Popup_0007, "&About AZip", IDM_ABOUT);
  end Create_Full_Menu; -- Menu_MDI_Child_Type


  -- Menu at line 90
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Main_Type)
  is
  begin
    Menu.Main:= Create_Menu;
    Menu.Popup_0001:= Create_Popup;
    Append_Menu(Menu.Main, "&File", Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&New archive" & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+N", IDM_NEW_ARCHIVE);
    Append_Item(Menu.Popup_0001, "&Open archive..." & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+O", IDM_OPEN_ARCHIVE);
    Append_Separator(Menu.Popup_0001);
    Menu.Popup_0002:= Create_Popup;
    Append_Menu(Menu.Popup_0001, "&Recent", Menu.Popup_0002);
    Append_Item(Menu.Popup_0002, "mru_1", IDM_MRU_1);
    Append_Item(Menu.Popup_0002, "mru_2", IDM_MRU_2);
    Append_Item(Menu.Popup_0002, "mru_3", IDM_MRU_3);
    Append_Item(Menu.Popup_0002, "mru_4", IDM_MRU_4);
    Append_Item(Menu.Popup_0002, "mru_5", IDM_MRU_5);
    Append_Item(Menu.Popup_0002, "mru_6", IDM_MRU_6);
    Append_Item(Menu.Popup_0002, "mru_7", IDM_MRU_7);
    Append_Item(Menu.Popup_0002, "mru_8", IDM_MRU_8);
    Append_Item(Menu.Popup_0002, "mru_9", IDM_MRU_9);
    Append_Separator(Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&Quit" & To_GString_from_String((1=>ASCII.HT)) & "Alt+F4", IDM_QUIT);
    Menu.Popup_0003:= Create_Popup;
    Append_Menu(Menu.Main, "&Window", Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "&Cascade", IDM_WINDOW_CASCADE);
    Append_Item(Menu.Popup_0003, "Tile &Horizontal", IDM_WINDOW_TILE_HORIZONTAL);
    Append_Item(Menu.Popup_0003, "Tile &Vertical", IDM_WINDOW_TILE_VERTICAL);
    Append_Item(Menu.Popup_0003, "&Close All", IDM_WINDOW_CLOSE_ALL);
    Menu.Popup_0004:= Create_Popup;
    Append_Menu(Menu.Main, "&Help", Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&About AZip", IDM_ABOUT);
  end Create_Full_Menu; -- Menu_MDI_Main_Type


  -- Dialog at resource line 134

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out About_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "About AZip";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 289, 210, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- About_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 289, 210, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  12, 14, 87, 80, x,y,w,h);
    Create( Window.Static_0001, Window, Num_resource(AZip_Icon), x,y,w,h, GWindows.Static_Controls.STATIC_SIZE, HALF_SUNKEN);
    Dlg_to_Scn(  110, 14, 165, 8, x,y,w,h);
    Create_label( Window, "AZip - A portable Zip Archive Manager", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  110, 29, 151, 8, x,y,w,h);
    Create_label( Window, "Copyright © Gautier de Montmollin 2012", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  110, 44, 100, 8, x,y,w,h);
    Create_label( Window, "MIT Open Source License", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  110, 61, 30, 8, x,y,w,h);
    Create_label( Window, "Internet:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  157, 61, 89, 8, x,y,w,h);
    Create( Window.AZip_URL, Window, "http://azip.sf.net/", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => AZip_URL);
    Dlg_to_Scn(  110, 81, 30, 8, x,y,w,h);
    Create_label( Window, "Version:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  157, 81, 118, 8, x,y,w,h);
    Create( Window.Version_label, Window, "(ver)", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Version_label);
    Dlg_to_Scn(  5, 105, 278, 78, x,y,w,h);
    Create( Window.Static_0007, Window, "Software made with the following free, open source components:", x,y,w,h);
    Dlg_to_Scn(  23, 119, 100, 8, x,y,w,h);
    Create( Window.GNAT_URL, Window, "GNAT -  free Ada compiler", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAT_URL);
    Dlg_to_Scn(  132, 119, 147, 8, x,y,w,h);
    Create( Window.GNAT_Version, Window, "GNAT_Version", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAT_Version);
    Dlg_to_Scn(  23, 134, 118, 8, x,y,w,h);
    Create( Window.GNAVI_URL, Window, "GNAVI / GWindows", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAVI_URL);
    Dlg_to_Scn(  23, 149, 170, 8, x,y,w,h);
    Create( Window.ResEdit_URL, Window, "ResEdit", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ResEdit_URL);
    Dlg_to_Scn(  23, 164, 75, 8, x,y,w,h);
    Create( Window.ZipAda_URL, Window, "Zip-Ada", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ZipAda_URL);
    Dlg_to_Scn(  132, 164, 147, 8, x,y,w,h);
    Create( Window.ZipAda_Version, Window, "ZA_Version", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ZipAda_Version);
    Dlg_to_Scn(  87, 186, 115, 18, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Close", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "Close", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  5, 186, 44, 18, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Credits_button, Window, "Credits", x,y,w,h, ID => Credits_button);
    Create( Window.Credits_button_permanent, Window, "Credits", x,y,w,h, ID => Credits_button);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Credits_button_permanent);
    else -- hide the closing button
      Hide(Window.Credits_button);
    end if;
  end Create_Contents; -- About_box_Type


  -- Dialog at resource line 161

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Credits_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Credits";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 301, 199, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Credits_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Credits_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 301, 199, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  107, 177, 88, 18, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Close", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "Close", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  11, 54, 284, 62, x,y,w,h);
    Create( Window.Static_0001, Window, "GWindows - native MS Windows framework", x,y,w,h);
    Dlg_to_Scn(  24, 69, 237, 8, x,y,w,h);
    Create_label( Window, "David Botton: main author", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  24, 83, 260, 8, x,y,w,h);
    Create_label( Window, "Frank Piron, Falk Maier at KonAd GmbH: authors of GWindows Extended", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  11, 4, 283, 44, x,y,w,h);
    Create( Window.Static_0004, Window, "Zip-Ada", x,y,w,h);
    Dlg_to_Scn(  25, 17, 234, 8, x,y,w,h);
    Create_label( Window, "Stratégies Software team: intensive profiling and contributions", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  25, 31, 226, 8, x,y,w,h);
    Create_label( Window, "ITEC team at NXP Semiconductors: contributions", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- Credits_box_Type


  -- Dialog at resource line 178

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out File_exists_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out File_exists_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "File already exists";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 359, 95, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- File_exists_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out File_exists_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 359, 95, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  4, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Overwrite_Yes, Window, "Yes", x,y,w,h, ID => Overwrite_Yes);
    Create( Window.Overwrite_Yes_permanent, Window, "Yes", x,y,w,h, ID => Overwrite_Yes);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Overwrite_Yes_permanent);
    else -- hide the closing button
      Hide(Window.Overwrite_Yes);
    end if;
    Dlg_to_Scn(  64, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Overwrite_No, Window, "No", x,y,w,h, ID => Overwrite_No);
    Create( Window.Overwrite_No_permanent, Window, "No", x,y,w,h, ID => Overwrite_No);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Overwrite_No_permanent);
    else -- hide the closing button
      Hide(Window.Overwrite_No);
    end if;
    Dlg_to_Scn(  124, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Overwrite_All, Window, "All", x,y,w,h, ID => Overwrite_All);
    Create( Window.Overwrite_All_permanent, Window, "All", x,y,w,h, ID => Overwrite_All);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Overwrite_All_permanent);
    else -- hide the closing button
      Hide(Window.Overwrite_All);
    end if;
    Dlg_to_Scn(  184, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Overwrite_None, Window, "None", x,y,w,h, ID => Overwrite_None);
    Create( Window.Overwrite_None_permanent, Window, "None", x,y,w,h, ID => Overwrite_None);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Overwrite_None_permanent);
    else -- hide the closing button
      Hide(Window.Overwrite_None);
    end if;
    Dlg_to_Scn(  244, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Overwrite_Rename, Window, "Rename", x,y,w,h, ID => Overwrite_Rename);
    Create( Window.Overwrite_Rename_permanent, Window, "Rename", x,y,w,h, ID => Overwrite_Rename);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Overwrite_Rename_permanent);
    else -- hide the closing button
      Hide(Window.Overwrite_Rename);
    end if;
    Dlg_to_Scn(  304, 74, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  18, 26, 327, 8, x,y,w,h);
    Create( Window.Conflict_simple_name, Window, "(name)", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Conflict_simple_name);
    Dlg_to_Scn(  18, 41, 319, 8, x,y,w,h);
    Create( Window.Conflict_location, Window, "(location)", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Conflict_location);
    Dlg_to_Scn(  4, 7, 349, 63, x,y,w,h);
    Create( Window.Static_0001, Window, "A file with the same name exists on the target location.", x,y,w,h);
    Dlg_to_Scn(  12, 58, 156, 8, x,y,w,h);
    Create_label( Window, "Do you want to replace this file ?", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- File_exists_box_Type


  -- Dialog at resource line 198

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out Find_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Find_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Find";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 191, 95, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Find_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Find_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 191, 95, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  76, 76, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  129, 76, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  9, 20, 172, 12, x,y,w,h);
    Create( Window.Name_to_be_searched, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Name_to_be_searched);
    Dlg_to_Scn(  9, 7, 172, 8, x,y,w,h);
    Create_label( Window, "Entry &name ( if empty: all names )", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  9, 54, 172, 12, x,y,w,h);
    Create( Window.Content_to_be_searched, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Content_to_be_searched);
    Dlg_to_Scn(  9, 41, 172, 8, x,y,w,h);
    Create_label( Window, "&Content ( if empty: any content )", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- Find_box_Type


  -- Dialog at resource line 214

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out Password_input_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Password_input_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Password";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 228, 137, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Password_input_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Password_input_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 228, 137, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  112, 119, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  167, 119, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  14, 73, 204, 13, x,y,w,h);
    Create( Window.Password_edit, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Password_edit);
    Dlg_to_Scn(  13, 42, 157, 8, x,y,w,h);
    Create_label( Window, "The current password is invalid.", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  14, 56, 159, 8, x,y,w,h);
    Create_label( Window, "Please enter a new password:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  13, 97, 96, 8, x,y,w,h);
    Create( Window.Show_password_box, Window, "Show password", x,y,w,h, ID => Show_password_box);
    Dlg_to_Scn(  26, 21, 175, 8, x,y,w,h);
    Create( Window.Encrypted_entry, Window, "(name)", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Encrypted_entry);
    Dlg_to_Scn(  12, 10, 203, 26, x,y,w,h);
    Create( Window.Static_0003, Window, "This entry is encrypted.", x,y,w,h);
  end Create_Contents; -- Password_input_box_Type


  -- Dialog at resource line 232

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out Progress_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Progress_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "AZip is busy.";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 242, 116, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Progress_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Progress_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 242, 116, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  7, 12, 218, 10, x,y,w,h);
    Create( Window.Entry_operation_name, Window, "Adding...", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Entry_operation_name);
    Dlg_to_Scn(  7, 30, 228, 8, x,y,w,h);
    Create( Window.Entry_name, Window, "Some file", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Entry_name);
    Dlg_to_Scn(  7, 52, 227, 9, x,y,w,h);
    Create( Window.File_Progress, Window, x,y,w,h, HORIZONTAL, FALSE);
    Dlg_to_Scn(  7, 74, 227, 17, x,y,w,h);
    Create( Window.Archive_Progress, Window, x,y,w,h, HORIZONTAL, FALSE);
    Dlg_to_Scn(  96, 96, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Cancel_button, Window, "Cancel", x,y,w,h, ID => Cancel_button);
    Create( Window.Cancel_button_permanent, Window, "Cancel", x,y,w,h, ID => Cancel_button);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Cancel_button_permanent);
    else -- hide the closing button
      Hide(Window.Cancel_button);
    end if;
  end Create_Contents; -- Progress_box_Type


  -- Dialog at resource line 247

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Wait_refresh_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Please wait";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 186, 95, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Wait_refresh_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Wait_refresh_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 186, 95, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  18, 12, 143, 8, x,y,w,h);
    Create_label( Window, "Zip directory is loaded", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  17, 37, 156, 8, x,y,w,h);
    Create_label( Window, "Please wait for filling the List View", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- Wait_refresh_box_Type


  -- ** Generated code ends here /\ /\ /\.

  -- ** Some helper utilities (body).

  procedure Dlg_to_Scn( -- converts dialog coords to screen (pixel) coords.
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer)
  is
    -- function GetDialogBaseUnits return Integer;
    -- pragma Import (StdCall, GetDialogBaseUnits, "GetDialogBaseUnits");
    -- baseunit, baseunitX, baseunitY: Integer;
    baseunitX: constant:= 6;
    baseunitY: constant:= 13;
  begin
    -- baseunit:= GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)
    -- baseunitX:= baseunit mod (2 ** 16);
    -- baseunitY:= baseunit  / (2 ** 16);
    -- NB: the other way with MapDialogRect works only
    --   by full moon, hence the use-defined units.
    xs := (xd * baseunitX) / 4;
    ws := (wd * baseunitX) / 4;
    ys := (yd * baseunitY) / 8;
    hs := (hd * baseunitY) / 8;
  end Dlg_to_Scn;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    -- ^ These fonts are created once, at startup
    --   it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    -- in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function Num_resource(id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_from_String('#' & img(img'first+1..img'Last));
  end Num_resource;

  package body Common_Fonts is

    procedure Create_Common_Fonts is

     type Face_Name_Type is array(1..32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight: Interfaces.C.long;
       lfWidth: Interfaces.C.long;
       lfEscapement: Interfaces.C.long;
       lfOrientation: Interfaces.C.long;
       lfWeight: Interfaces.C.long;
       lfItalic: Interfaces.C.char;
       lfUnderline: Interfaces.C.char;
       lfStrikeOut: Interfaces.C.char;
       lfCharSet: Interfaces.C.char;
       lfOutPrecision: Interfaces.C.char;
       lfClipPrecision: Interfaces.C.char;
       lfQuality: Interfaces.C.char;
       lfPitchAndFamily: Interfaces.C.char;
       lfFaceName: Face_Name_Type;
     end record;

     Log_of_current_font: aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);
        cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;
        lpvObject: LPVOID                 := Log_of_Current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject: LPVOID                 := Log_of_Current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font(
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font(URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            -- !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_Current_font.lfUnderline:= Interfaces.C.Char'Val(1);
        GWindows.Drawing_Objects.Handle(URL_font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Common_Fonts.Create_Common_Fonts;

  -- Last line of resource script file: 330

end azip_Resource_GUI;
