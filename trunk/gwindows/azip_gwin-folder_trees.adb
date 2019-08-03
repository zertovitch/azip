with AZip_Common;                       use AZip_Common;
with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Menus;                    use GWindows.Menus;

with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces.C;

package body AZip_GWin.Folder_Trees is

  use GWindows.Common_Controls;

  overriding procedure On_Selection_Change (Control : in out Folder_tree_type) is
    w_node: constant Tree_Item_Node:= Control.Selected_Item;
    parent_window: MDI_Child_Type renames MDI_Child_Type(Control.Parent.Parent.all);
    new_path: GString_Unbounded;
    use AZip_Common.Node_Catalogues;
    curs : constant Cursor :=
      parent_window.node_map.Find (Integer (w_node));
  begin
    if curs = No_Element then
      null;  --  Tree node not registered by us. We let new_path empty.
    else
      new_path := Element (curs);
    end if;
    if new_path = parent_window.selected_path then
      parent_window.Update_status_bar;
      return; -- the same node as before has been selected, no further refresh needed.
    end if;
    parent_window.selected_path:= new_path;
    parent_window.Update_display(node_selected);
  end On_Selection_Change;

  overriding procedure On_Focus (Control : in out Folder_tree_type) is
    MDI_Child : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.all);
  begin
    MDI_Child.Update_status_bar;
    MDI_Child.Update_tool_bar_and_menus;
  end On_Focus;

  overriding procedure On_Notify (
      Window       : in out Folder_tree_type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
  )
  is
    TVN_FIRST      : constant := -400;
    TVN_BEGINDRAGA : constant := TVN_FIRST -  7;
    TVN_BEGINDRAGW : constant := TVN_FIRST - 56;
    --  \/ \/ \/ \/ Code from Ex_TV's body:
    type NMTREEVIEW is
      record
         Hdr     : Notification;
         Action  : Interfaces.C.unsigned;
         ItemOld : TVITEM;
         ItemNew : TVITEM;
         PtDrag  : GWindows.Types.Point_Type;
      end record;

    type Pointer_To_NMTREEVIEW_Type is access all NMTREEVIEW;

    function Message_To_NmTreeView_Pointer is
      new Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                                    Pointer_To_NMTREEVIEW_Type);
    --  /\ /\ /\ /\
  begin
    --  Call parent method
    Tree_View_Control_Type (Window).On_Notify
       (Message, Control, Return_Value);
    case Message.Code is
      when TVN_BEGINDRAGA | TVN_BEGINDRAGW =>
        declare
          MDI_Child : MDI_Child_Type renames MDI_Child_Type (Window.Parent.Parent.all);
          MDI_Main  : MDI_Main_Type  renames MDI_Child.MDI_Root.all;
          Nmtv_Ptr  : constant Pointer_To_NMTREEVIEW_Type :=
                 Message_To_NmTreeView_Pointer (Message);
          success   : Boolean;
        begin
          Window.Focus;
          --  When you drag a tree item which is not selected, Windows doesn't select the
          --  dragged item, contrary to what happens for instance with the list view.
          --  NB: the same problem appears with right-click.
          --  So, we need to select programmatically the dragged item right now.
          success := Window.Select_Item (Nmtv_Ptr.ItemNew.HItem);
          if success then
            Capture_Mouse (MDI_Child);
            MDI_Main.dragging.is_dragging := True;
            --  The rest of the dragging operation is handled by the parent window, of
            --  type MDI_Child_Type: see On_Mouse_Move, On_Left_Mouse_Button_Up.
          end if;
        end;
      when others =>
        null;
    end case;
  end On_Notify;

  overriding procedure On_Right_Click (Control : in out Folder_tree_type) is
    MDI_Child : MDI_Child_Type renames
      MDI_Child_Type (Control.Parent.Parent.all);
    use AZip_Common.Node_Catalogues;
    clicked_node : constant Tree_Item_Node :=
      Control.Item_At_Position (
        Control.Point_To_Client (Get_Cursor_Position)
      );
    curs : constant Cursor :=
      MDI_Child.node_map.Find (Integer (clicked_node));
    success   : Boolean;
  begin
    --  Windows forgets to actually select the folder, despite
    --  a temporary coloration of the clicked folder.
    --  NB: the same problem appears with dragging.
    --  So, we need to select programmatically the clicked item right now.
    --  But first, we need to check if the clicked node is valid.
    --  If we click below the tree, clicked_node has a bogus value.
    if curs = No_Element then
      null;  --  Tree node not registered by us -> invalid.
    else
      --  The node is known to us, we select it.
      success := Control.Select_Item (clicked_node);
      if success then
        Immediate_Popup_Menu (MDI_Child.context_menu_folder, MDI_Child);
      end if;
    end if;
  end On_Right_Click;

end AZip_GWin.Folder_Trees;
