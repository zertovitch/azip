-- To do: make a generic "Office Classic" application framework

with AZip_Common;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows;                          use GWindows;
with GWindows.Base;
with GWindows.Windows.MDI;
with GWindows.Common_Controls;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Image_Lists;
with GWindows.Menus;
with GWindows.Constants;

package AZip_GWin.MDI_Main is

  -- type MDI_Toolbar_Type is
  --   new GWindows.Common_Controls.Toolbar_Control_Type with null record;

  -- procedure On_Button_Select (Control : in out MDI_Toolbar_Type;
  --                             Item    : in     Integer);
  --  Handle click on toolbar

  type IDM_MRU_List is array(1..9) of Natural;

  type MRU_List is array(IDM_MRU_List'Range) of GString_Unbounded;

  type MDI_Main_Type is
    new GWindows.Windows.MDI.MDI_Main_Window_Type with
      record
        MDI_childen_maximized: Boolean:= True;
        MDI_main_maximized   : Boolean:= True;
        Memorized_Left,
        Memorized_Top,
        Memorized_Width,
        Memorized_Height     : Integer:= GWindows.Constants.Use_Default;
        Success_in_enumerated_close: Boolean;
        -- MRU (Most recently used) files names:
        IDM_MRU: IDM_MRU_List;
        mru: MRU_List:= (others=> To_GString_Unbounded(""));
        -- Tool_Bar               : MDI_Toolbar_Type;
        -- Images                 : GWindows.Image_Lists.Image_List_Type;
        Menu                   : Menu_MDI_Main_Type;
        -- record_dimensions      : Boolean:= False; -- in On_Move, On_Size
        User_maximize_restore  : Boolean:= True;
        -- ^ Detect user-triggered max/restore commands
        latest_options         : AZip_Common.Option_Pack_Type;
      end record;

  type MDI_Main_Access is access all MDI_Main_Type;

  procedure On_Create (Window : in out MDI_Main_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc: Boolean);
  --  File|New event

  procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer        );

  procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        );

  procedure Update_Common_Menus(Window    : in out MDI_Main_Type;
                                top_entry : GString:= "" );

end AZip_GWin.MDI_Main;
