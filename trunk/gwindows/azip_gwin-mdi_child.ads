with AZip_Common.User_options;          use AZip_Common.User_options;
with AZip_Common.Operations;            use AZip_Common.Operations;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with Zip;

with GWindows;                          use GWindows;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Common_Controls.Ex_List_View;
with GWindows.Drawing;
with GWindows.GControls.GSize_Bars;
with GWindows.Packing_Boxes;
with GWindows.Panels;
with GWindows.Static_Controls;
with GWindows.Types;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Numerics.Float_Random;

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

  type LV_Payload is record
    index_before_sorting: Integer;
  end record;

  package AZip_LV_Ex is new GWindows.Common_Controls.Ex_List_View(LV_Payload);

  type MDI_Child_List_View_Control_Type is
    new AZip_LV_Ex.Ex_List_View_Control_Type with null record;

  overriding procedure On_Item_Changed
    (Control : in out MDI_Child_List_View_Control_Type);
  overriding function On_Compare(
               Control: in MDI_Child_List_View_Control_Type;
               Column : in Natural;
               Value1 : in GString;
               Value2 : in GString) return Integer;
  overriding procedure On_Focus (Control : in out MDI_Child_List_View_Control_Type);
  -- overriding procedure On_Free_Payload(
  --              Control: in out MDI_Child_List_View_Control_Type;
  --              Payload: out AZip_LV_Ex.Data_access);
  type MDI_Child_Tree_View_Control_Type is new Tree_View_Control_Type with null record;
  overriding procedure On_Selection_Change (Control : in out MDI_Child_Tree_View_Control_Type);
  overriding procedure On_Focus (Control : in out MDI_Child_Tree_View_Control_Type);

  type MDI_Child_Packing_Box_Type is new GWindows.Packing_Boxes.Packing_Box_Type with null record;
  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Packing_Box_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Child_Packing_Box_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  type MDI_Child_Panel_Type is new GWindows.Panels.Panel_Type with null record;
  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Child_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  type MDI_Child_GSize_Bar_Type is new GWindows.GControls.GSize_Bars.GSize_Bar_Type with null record;
  overriding procedure On_Bar_Moved (Window : in out MDI_Child_GSize_Bar_Type);

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
        Tree_Bar_and_List: MDI_Child_Packing_Box_Type;
        Bar_and_List     : MDI_Child_Panel_Type;
        Directory_List   : MDI_Child_List_View_Control_Type;
        Splitter         : MDI_Child_GSize_Bar_Type;
        Splitter_dashes  : GWindows.Static_Controls.Label_Type;
        Folder_Tree      : aliased MDI_Child_Tree_View_Control_Type;
        zif              : Zip.Zip_info;
        path_map         : AZip_Common.Path_Catalogues.Map;
        node_map         : AZip_Common.Node_Catalogues.Map;
        selected_path    : GString_Unbounded:= Null_GString_Unbounded;
        opt              : Option_Pack_Type;
        Status_deamon    : Daemons.Status_display;
        Status_Bar       : MDI_Child_Status_Bar_Type;
        name_search      : GString_Unbounded;
        content_search   : GString_Unbounded;
        current_password : GString_Unbounded;
        temp_name_gen    : Ada.Numerics.Float_Random.Generator;
        last_operation   : Archive_Operation:= Remove;
        last_max_code    : Integer;
        any_path_in_zip  : Boolean;
        extract_dir      : GString_Unbounded;
        last_sort_col    : Integer:= -1; -- -1 if none
        last_sort_direc  : AZip_LV_Ex.Sort_Direction_Type;
        refreshing_list  : Boolean:= False;
        is_closing       : Boolean:= False;
        last_op_comment_1: GString_Unbounded;
        last_op_comment_2: GString_Unbounded;
      end record;

  overriding procedure On_Create (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);
  -- This would be abstract in a 'generic' Office framework.

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean;
  -- This would be abstract in a 'generic' Office framework.

  procedure On_Save_As (Window : in out MDI_Child_Type);

  -- Add files (past all dialogs)
  procedure Go_for_adding (
    Window     : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names;
    Encrypt    : in     Boolean
  );

  overriding procedure On_File_Drop (
    Window     : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names
  );

  type Update_need is
    (first_display,   -- first display ever, no columns set
     archive_changed, -- directory list needs to be refilled
     node_selected,   -- same archive, but partial directory list needs to be refilled
     results_refresh, -- same directory, only results changed
     status_bar,      -- status bar and topics listed below
     toolbar_and_menu -- update enable/disable of toolbar items and menu items
    );

  procedure Update_display (
    Window : in out MDI_Child_Type;
    need   :        Update_need
  );

  procedure Load_archive_catalogue (
    Window     : in out MDI_Child_Type;
    copy_codes :        Boolean
  );

  overriding procedure On_Size (
    Window : in out MDI_Child_Type;
    Width  : in     Integer;
    Height : in     Integer
  );

  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Child_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Menu_Select (
    Window : in out MDI_Child_Type;
    Item   : in     Integer
  );

  overriding procedure On_Focus (Window : in out MDI_Child_Type);

  overriding procedure On_Close (
    Window    : in out MDI_Child_Type;
    Can_Close :    out Boolean
  );

  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" );

end AZip_GWin.MDI_Child;
