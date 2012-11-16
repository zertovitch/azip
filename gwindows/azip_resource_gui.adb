---------------------------------------------------------------------------
-- GUI contents of resource script file: azip.rc
-- Transcription time: 2012/11/16   02:51:11
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 26-Aug-2012
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;                 use GWindows.GStrings;
with System;

package body azip_Resource_GUI is

  -- ** Generated code begins here \/ \/ \/.


  -- Menu at line 17
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
    Append_Item(Menu.Popup_0003, "&Extract...", IDM_EXTRACT);
    Append_Separator(Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "Delete entries" & To_GString_from_String((1=>ASCII.HT)) & "- / Del", IDM_Delete_selected);
    Append_Item(Menu.Popup_0003, "A&dd files..." & To_GString_from_String((1=>ASCII.HT)) & "+", IDM_A_DD_FILES_1);
    Menu.Popup_0004:= Create_Popup;
    Append_Menu(Menu.Main, "&Tools", Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&Test archive" & To_GString_from_String((1=>ASCII.HT)) & "Shift+T", IDM_TEST_ARCHIVE);
    Append_Item(Menu.Popup_0004, "Fre&shen archive", IDM_FRESHEN_ARCHIVE);
    State(Menu.Popup_0004, Command, IDM_FRESHEN_ARCHIVE, Grayed);
    Append_Item(Menu.Popup_0004, "&Recompress archive", IDM_RECOMPRESS_ARCHIVE);
    State(Menu.Popup_0004, Command, IDM_RECOMPRESS_ARCHIVE, Grayed);
    Append_Item(Menu.Popup_0004, "&Find in archive..." & To_GString_from_String((1=>ASCII.HT)) & "Ctrl+F", IDM_FIND_IN_ARCHIVE);
    Append_Separator(Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&Compare archives", IDM_COMPARE_ARCHIVES);
    State(Menu.Popup_0004, Command, IDM_COMPARE_ARCHIVES, Grayed);
    Append_Item(Menu.Popup_0004, "&Merge archives", IDM_MERGE_ARCHIVES);
    State(Menu.Popup_0004, Command, IDM_MERGE_ARCHIVES, Grayed);
    Menu.Popup_0005:= Create_Popup;
    Append_Menu(Menu.Main, "&View", Menu.Popup_0005);
    Append_Item(Menu.Popup_0005, "&Flat view", IDM_FLAT_VIEW);
    Append_Item(Menu.Popup_0005, "&Tree view", IDM_TREE_VIEW);
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


  -- Menu at line 81
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


  -- Dialog at resource line 125

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
    Dlg_to_Scn(  0, 0, 256, 210, x,y,w,h);
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
    Dlg_to_Scn(  0, 0, 256, 210, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  103, 190, 50, 14, x,y,w,h);
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
    Dlg_to_Scn(  47, 10, 165, 8, x,y,w,h);
    Create_label( Window, "AZip - a portable Zip user interface", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  47, 25, 151, 8, x,y,w,h);
    Create_label( Window, "Copyright © Gautier de Montmollin 2012", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  47, 40, 100, 8, x,y,w,h);
    Create_label( Window, "MIT Open Source License", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  10, 66, 30, 8, x,y,w,h);
    Create_label( Window, "Internet:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  57, 66, 132, 8, x,y,w,h);
    Create( Window.AZip_URL, Window, "http://sf.net/projects/azip", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => AZip_URL);
    Dlg_to_Scn(  5, 105, 247, 81, x,y,w,h);
    Create( Window.Static_0005, Window, "Software made with the following free, open source components:", x,y,w,h);
    Dlg_to_Scn(  23, 119, 100, 8, x,y,w,h);
    Create( Window.GNAT_URL, Window, "GNAT -  free Ada compiler", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAT_URL);
    Dlg_to_Scn(  133, 119, 113, 8, x,y,w,h);
    Create( Window.GNAT_Version, Window, "GNAT_Version", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAT_Version);
    Dlg_to_Scn(  23, 134, 118, 8, x,y,w,h);
    Create( Window.GNAVI_URL, Window, "GNAVI / GWindows", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAVI_URL);
    Dlg_to_Scn(  23, 149, 170, 8, x,y,w,h);
    Create( Window.ResEdit_URL, Window, "ResEdit", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ResEdit_URL);
    Dlg_to_Scn(  23, 164, 75, 8, x,y,w,h);
    Create( Window.ZipAda_URL, Window, "Zip-Ada", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ZipAda_URL);
    Dlg_to_Scn(  133, 164, 113, 8, x,y,w,h);
    Create( Window.ZipAda_Version, Window, "ZA_Version", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ZipAda_Version);
  end Create_Contents; -- About_box_Type


  -- Dialog at resource line 148

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


  -- Dialog at resource line 164

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Progress_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "AZip is busy";
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

  function Num_resource(id: Natural) return String is
    img: constant String:= Integer'Image(id);
  begin
    return '#' & img(img'first+1..img'Last);
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

  -- Last line of resource script file: 222

end azip_Resource_GUI;
