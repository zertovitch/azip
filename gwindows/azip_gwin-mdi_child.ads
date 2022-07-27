with AZip_Common.User_options;          use AZip_Common.User_options;
with AZip_Common.Operations;            use AZip_Common.Operations;
with AZip_GWin.Folder_Trees;
with AZip_GWin.Directory_Lists;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with Zip;

with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Drawing;
with GWindows.GControls.GSize_Bars;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Packing_Boxes;
with GWindows.Panels;
with GWindows.Types;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;

with GWin_Util;

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
         entry Display (w : AZip_GWin.MDI_Child.MDI_Child_Access);
         entry Stop;
      end Status_display;

      -----------------------------------------------------------------
      -- This background task calls an archive test on demand        --
      -- and ensures display of its progress.                        --
      -----------------------------------------------------------------
      task type Testing_type is
         entry Start;
         entry Test (w : AZip_GWin.MDI_Child.MDI_Child_Access);
         entry Stop;
      end Testing_type;
   end Daemons;

  type MDI_Child_Status_Bar_Part is (directory_info, task_message);

  type MDI_Child_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

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
        File_Name           : GString_Unbounded;
        Short_Name          : GString_Unbounded;
        --  ^ Window title = Short_Name & {""|" *"}
        MDI_Root            : MDI_Main_Access; -- -> access to the containing window
        Extra_first_doc     : Boolean := False;
        --  ^ new file closed if kept virgin when opening another one (like blank Excel sheet).
        Menu                : Menu_MDI_Child_Type;
        context_menu_file   : Menu_Type := Create_Popup;
        context_menu_folder : Menu_Type := Create_Popup;
        Tree_Bar_and_List   : MDI_Child_Packing_Box_Type;
        Bar_and_List        : MDI_Child_Panel_Type;
        Directory_List      : AZip_GWin.Directory_Lists.Directory_list_type;
        Splitter            : GWin_Util.Splitter_with_dashes;
        Folder_Tree         : AZip_GWin.Folder_Trees.Folder_tree_type;
        zif                 : Zip.Zip_info;
        path_map            : AZip_Common.Path_Catalogues.Map;
        node_map            : AZip_Common.Node_Catalogues.Map;
        selected_path       : GString_Unbounded := Null_GString_Unbounded;
        opt                 : Option_Pack_Type;
        Status_deamon       : Daemons.Status_display;
        Status_Bar          : MDI_Child_Status_Bar_Type;
        name_search         : GString_Unbounded;
        content_search      : GString_Unbounded;
        current_password    : GString_Unbounded;
        temp_name_gen       : Ada.Numerics.Float_Random.Generator;
        last_operation      : Archive_Operation := Remove;
        last_max_code       : Integer;
        any_path_in_zip     : Boolean;
        extract_dir         : GString_Unbounded;
        is_closing          : Boolean := False;
        last_op_comment_1   : GString_Unbounded;
        last_op_comment_2   : GString_Unbounded;
      end record;

  procedure Update_status_bar (Window : in out MDI_Child_Type);
  procedure Update_tool_bar_and_menus (Window : in out MDI_Child_Type);

  overriding procedure On_Create (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);
  --  This would be abstract in a 'generic' Office framework.

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean;
  --  This would be abstract in a 'generic' Office framework.

  procedure On_Save_As (Window : in out MDI_Child_Type);

  --  Add files (past all dialogs)
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

  procedure Update_Display (
    Window : in out MDI_Child_Type;
    need   :        Update_need
  );

  procedure Load_Archive_Catalogue (
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

  overriding procedure On_Mouse_Move (
        Window : in out MDI_Child_Type;
        X      : in     Integer;
        Y      : in     Integer;
        Keys   : in     Mouse_Key_States);

  overriding procedure On_Left_Mouse_Button_Up (
        Window : in out MDI_Child_Type;
        X      : in     Integer;
        Y      : in     Integer;
        Keys   : in     Mouse_Key_States);

  procedure Update_Common_Menus (Window    : MDI_Child_Type;
                                 top_entry : GString := "");

end AZip_GWin.MDI_Child;
