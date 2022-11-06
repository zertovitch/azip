with GWindows.Base;

package AZip_GWin.Drop_File_Dialog is

  procedure Do_Drop_File_Dialog
    (Parent         : in out GWindows.Base.Base_Window_Type'Class;
     archive_name   : in     GString;
     new_archive    : in     Boolean;
     encrypt        : in out Boolean;
     yes            :    out Boolean);

end AZip_GWin.Drop_File_Dialog;
