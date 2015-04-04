with GWindows;                          use GWindows;
with GWindows.Base;                     use GWindows.Base;

package AZip_GWin.Drop_file_dialog is

  procedure Do_drop_file_dialog(
    Parent         : in out GWindows.Base.Base_Window_Type'Class;
    archive_name   : in     GString;
    new_archive    : in     Boolean;
    encrypt        : in out Boolean;
    yes            :    out Boolean
  );

end AZip_GWin.Drop_file_dialog;
