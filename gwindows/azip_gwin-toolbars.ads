with AZip_GWin.MDI_Main;

with GWindows.Common_Controls, GWindows.Image_Lists;

package AZip_GWin.Toolbars is

  --  ** Main tool bar (add / remove / ...) at top left of the main window:

  procedure Init_Main_toolbar(
    tb    : in out GWindows.Common_Controls.Toolbar_Control_Type'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out AZip_GWin.MDI_Main.MDI_Main_Type
  );

end AZip_GWin.Toolbars;
