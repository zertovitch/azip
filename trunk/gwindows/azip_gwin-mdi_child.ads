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
with GWindows.Windows.MDI;

with GWindows.Types;
with AZip_Common;

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
        Directory_List   : List_View_Control_Type;
        Folder_Tree      : Tree_View_Control_Type;
        zif              : Zip.Zip_info;
        current_options  : AZip_Common.Option_Pack_Type;
        Status_deamon    : Daemons.Status_display;
      end record;

  procedure On_Create (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);
  -- This would be abstract in a 'generic' Office framework.

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean;
  -- This would be abstract in a 'generic' Office framework.

  type Update_need is
    (first_display,
     archive_changed,
     simple_refresh);

  procedure Update_display (
    Window : in out MDI_Child_Type;
    need   :        Update_need
  );

  procedure Reload_archive (Window : in out MDI_Child_Type);

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer);

  procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean);


end AZip_GWin.MDI_Child;
