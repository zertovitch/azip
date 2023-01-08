with AZip_GWin.MDI_Main;

with Office_Applications;

package AZip_GWin.Toolbars is

  --  ** Main tool bar (add / remove / ...) at top left of the main window:

  procedure Init_Main_Tool_Bar
    (tb     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     parent : in out AZip_GWin.MDI_Main.MDI_Main_Type);

end AZip_GWin.Toolbars;
