with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows;                          use GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;

package body AZip_GWin.Drop_file_dialog is

  procedure Do_drop_file_dialog(
    Parent         : in out GWindows.Base.Base_Window_Type'Class;
    archive_name   : in     GString;
    new_archive    : in     Boolean;
    archive_window : in     Boolean;
    encrypt        : in out Boolean;
    yes            :    out Boolean
  )
  is
    box: Drop_files_Type;
  begin
    box.Create_Full_Dialog(Parent);
    box.Center;
    case Show_Dialog(box, Parent) is
      when IDOK =>
        yes:= True;
      when others =>
        yes:= False;
    end case;
  end Do_drop_file_dialog;

end AZip_GWin.Drop_file_dialog;
