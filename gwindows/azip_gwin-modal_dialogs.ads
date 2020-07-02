--  Small modal dialogs

with AZip_GWin.MDI_Main;
with GWindows.Base;

package AZip_GWin.Modal_Dialogs is

  procedure Show_About_Box (Window : in out MDI_Main.MDI_Main_Type);

  procedure Show_Sponsoring_Box
    (Window      : in out GWindows.Base.Base_Window_Type'Class;
     First_Visit : in     Boolean
    );

end AZip_GWin.Modal_Dialogs;
