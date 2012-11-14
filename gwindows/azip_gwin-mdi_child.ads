with AZip_Common;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with Zip;

with GWindows;                          use GWindows;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Cursors;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;
with GWindows.Drawing_Panels;
with GWindows.Menus;
with GWindows.Types;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;


package AZip_GWin.MDI_Child is

  type MDI_Child_Type;
  type MDI_Child_Access is access all MDI_Child_Type;

   package Daemons is
      ---------------------------------------------------------------------------
      -- This background task displays on demand (also from another task)      --
      -- informations about archive and various status.                        --
      ---------------------------------------------------------------------------
      task type Status_display is
         entry Start;
         entry Display(w: AZip_GWin.MDI_Child.MDI_Child_Access);
         entry Stop;
      end;

      -----------------------------------------------------------------
      -- This background task calls an archive test on demand        --
      -- and ensures display of its progress.                        --
      -----------------------------------------------------------------
      task type Testing_type is
         entry Start;
         entry Test(w: AZip_GWin.MDI_Child.MDI_Child_Access);
         entry Stop;
      end;
   end Daemons;

  type MDI_Child_Status_bar_part is ( directory_info, task_message );

  type MDI_Child_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

  type MDI_Child_List_View_Control_Type is new List_View_Control_Type with null record;
  procedure On_Item_Changed (Control : in out MDI_Child_List_View_Control_Type);

  type MDI_Child_Type is
    new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
        File_Name        : GString_Unbounded;
        Short_Name       : GString_Unbounded;
        -- ^ Window title = Short_Name & {""|" *"}
        Parent           : MDI_Main_Access; -- -> access to the containing window
        Extra_first_doc  : Boolean:= False;
        -- ^ new file closed if kept virgin when opening another one (like blank Excel sheet).
        Menu             : Menu_MDI_Child_Type;
        Directory_List   : MDI_Child_List_View_Control_Type;
        Folder_Tree      : Tree_View_Control_Type;
        zif              : Zip.Zip_info;
        current_options  : AZip_Common.Option_Pack_Type;
        Status_deamon    : Daemons.Status_display;
        Status_Bar       : MDI_Child_Status_Bar_Type;
      end record;

  procedure On_Create (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);
  -- This would be abstract in a 'generic' Office framework.

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean;
  -- This would be abstract in a 'generic' Office framework.

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names);

  type Update_need is
    (first_display,   -- first display ever, no columns set
     archive_changed, -- directory needs a refresh
     simple_refresh
    );

  procedure Update_display (
    Window : in out MDI_Child_Type;
    need   :        Update_need
  );

  procedure Load_archive_catalogue (Window : in out MDI_Child_Type);

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer);

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item   : in     Integer        );

  procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean);


end AZip_GWin.MDI_Child;
