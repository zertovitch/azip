with GWindows.Base, GWindows.Common_Controls, GWindows.Image_Lists;

package AZip_GWin.Toolbars is

  -- ** Main tool bar (add / remove / ...) at top left of the main window:

  procedure Init_Main_toolbar(
    tb    : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out GWindows.Base.Base_Window_Type'Class);

end AZip_GWin.Toolbars;
