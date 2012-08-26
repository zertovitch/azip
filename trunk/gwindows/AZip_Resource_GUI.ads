---------------------------------------------------------------------------
-- GUI contents of resource script file: azip.rc
-- Transcription time: 2012/08/26   13:03:46
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 26-Aug-2012
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_drawn;      use GWindows.Buttons.Owner_drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

package azip_Resource_GUI is

  type Menu_MDI_Child_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 2; title: "&Recent"
    Popup_0003: Menu_Type;  -- level 1; title: "&Edit"
    Popup_0004: Menu_Type;  -- level 1; title: "&Action"
    Popup_0005: Menu_Type;  -- level 1; title: "&View"
    Popup_0006: Menu_Type;  -- level 1; title: "&Window"
    Popup_0007: Menu_Type;  -- level 1; title: "&Help"
  end record; -- Menu_MDI_Child_Type

  -- Menu at line 67
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Child_Type);

  type Menu_MDI_Main_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 2; title: "&Recent"
    Popup_0003: Menu_Type;  -- level 1; title: "&Window"
    Popup_0004: Menu_Type;  -- level 1; title: "&Help"
  end record; -- Menu_MDI_Main_Type

  -- Menu at line 106
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Main_Type);

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin";
    FileDescription: constant String:= "AZip - A portable Zip user interface";
    FileVersion: constant String:= "0.01";
    LegalCopyright: constant String:= "© 2012 G. de Montmollin (MIT license)";
    ProductName: constant String:= "AZip";
    Translation: constant:= 1033;
  end Version_info;


  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC                  : constant:=     -1;
  Menu_MDI_Main               : constant:=    102;
  Menu_MDI_Child              : constant:=    104;
  IDM_NEW_FILE                : constant:=  40000;
  IDM_OPEN_FILE               : constant:=  40001;
  IDM_MRU_1                   : constant:=  40002;
  IDM_ABOUT                   : constant:=  40003;
  IDM_SAVE_FILE_AS            : constant:=  40004;
  IDM_TEST_ARCHIVE            : constant:=  40004;
  IDM_QUIT                    : constant:=  40005;
  IDM_RECOMPRESS_ARCHIVE      : constant:=  40006;
  IDM_FIND_FILE_IN_ARCHIVE    : constant:=  40007;
  IDM_FIND_CONTENTS_IN_ARCHIVE: constant:=  40008;
  IDM_COMPARE_ARCHIVES        : constant:=  40009;
  IDM_FLAT_VIEW               : constant:=  40010;
  IDM_TREE_VIEW               : constant:=  40011;
  IDM_MRU_2                   : constant:=  40012;
  IDM_MRU_3                   : constant:=  40013;
  IDM_MRU_4                   : constant:=  40014;
  IDM_MRU_5                   : constant:=  40015;
  IDM_MRU_6                   : constant:=  40016;
  IDM_MRU_7                   : constant:=  40017;
  IDM_MRU_8                   : constant:=  40018;
  IDM_MRU_9                   : constant:=  40019;
  IDM_WINDOW_CASCADE          : constant:=  40020;
  IDM_WINDOW_TILE_HORIZONTAL  : constant:=  40021;
  IDM_WINDOW_TILE_VERTICAL    : constant:=  40022;
  IDM_WINDOW_CLOSE_ALL        : constant:=  40023;
  IDM_MERGE_ARCHIVES          : constant:=  40024;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return String;


  -- Last line of resource script file: 149

end azip_Resource_GUI;
