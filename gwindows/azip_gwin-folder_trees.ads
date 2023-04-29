with GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Types;

package AZip_GWin.Folder_Trees is

  ----------------------------------------------
  --  Folder_tree_type : archive folder tree  --
  ----------------------------------------------

  type Folder_tree_type is
    new GWindows.Common_Controls.Tree_View_Control_Type with null record;

  overriding procedure On_Selection_Change (Control : in out Folder_tree_type);

  overriding procedure On_Focus (Control : in out Folder_tree_type);

  overriding procedure On_Notify (
      Window       : in out Folder_tree_type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
  );

   overriding procedure On_Right_Click (Control : in out Folder_tree_type);

end AZip_GWin.Folder_Trees;
