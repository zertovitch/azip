-- To do: make a generic "Office Classic" application framework

with AZip_GWin.Dragging;
with AZip_Common.User_options;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows.Base;
with GWindows.Common_Controls;
with GWindows.Image_Lists;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Drawing;
with GWindows.Taskbar;                  use GWindows.Taskbar;
with GWindows.Types;

package AZip_GWin.MDI_Main is

  type MDI_Toolbar_Type is
    new GWindows.Common_Controls.Toolbar_Control_Type with null record;

  procedure On_Button_Select (Control : in out MDI_Toolbar_Type;
                              Item    : in     Integer);
  -- Handle click on toolbar

  type IDM_MRU_List is array(AZip_Common.User_options.MRU_List'Range) of Natural;

  type MDI_Main_Type is
    new GWindows.Windows.MDI.MDI_Main_Window_Type with
      record
        Success_in_enumerated_close: Boolean;
        -- MRU (Most recently used) files names:
        -- Menu ID's stored into a handy array
        IDM_MRU                : IDM_MRU_List;
        Tool_Bar               : MDI_Toolbar_Type;
        Toolbar_Images         : GWindows.Image_Lists.Image_List_Type;
        Folders_Images         : GWindows.Image_Lists.Image_List_Type;
        Menu                   : Menu_MDI_Main_Type;
        -- record_dimensions      : Boolean:= False; -- in On_Move, On_Size
        User_maximize_restore  : Boolean:= True;
        -- ^ Detect user-triggered max/restore commands
        record_dimensions      : Boolean:= False; -- in On_Move, On_Size
        -- Options of a "model" child window.
        opt                    : AZip_Common.User_options.Option_Pack_Type;
        remember_sorting       : Boolean := True;
        --
        Task_bar_gadget_ok     : Boolean := False;  --  Coloring of taskbar icon (Windows 7+)
        Task_bar_gadget        : Taskbar_List;
        --
        dragging               : AZip_GWin.Dragging.Dragging_info;
      end record;

  type MDI_Main_Access is access all MDI_Main_Type;

  overriding procedure On_Create (Window : in out MDI_Main_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc: Boolean);
  --  File|New event

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer);

  overriding procedure On_Size (Window : in out MDI_Main_Type;
                                Width  : in     Integer;
                                Height : in     Integer);

  overriding procedure On_File_Drop (Window     : in out MDI_Main_Type;
                                     File_Names : in     Array_Of_File_Names);

  overriding procedure On_Erase_Background
     (Window : in out MDI_Main_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Main_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  procedure Open_Child_Window_And_Load (
    Window     : in out MDI_Main_Type;
    File_Name  :        GWindows.GString_Unbounded
  );

  overriding procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer        );

  overriding procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        );

  procedure Update_Common_Menus(Window    : in out MDI_Main_Type;
                                top_entry : GString:= "" );

  function All_Zip_files(File_Names: Array_Of_File_Names) return Boolean;

  function Confirm_archives_if_all_Zip_files(
    Window: GWindows.Base.Base_Window_Type'Class;
    File_Names: Array_Of_File_Names
  )
  return Boolean;

end AZip_GWin.MDI_Main;
