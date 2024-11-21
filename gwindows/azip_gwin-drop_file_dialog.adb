with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Buttons,
     GWindows.Constants;

package body AZip_GWin.Drop_File_Dialog is

  procedure Do_Drop_File_Dialog
    (Parent         : in out GWindows.Base.Base_Window_Type'Class;
     archive_name   : in     GString;
     new_archive    : in     Boolean;
     encrypt        : in out Boolean;
     yes            :    out Boolean)
  is
    box : AZip_Resource_GUI.Drop_files_box_Type;
    procedure Get_Data (dummy : in out GWindows.Base.Base_Window_Type'Class) is
      use GWindows.Buttons;
    begin
      encrypt := box.Encrypt_check_box.State = Checked;
    end Get_Data;
  begin
    Parent.Focus;
    box.Create_Full_Dialog (Parent);
    box.Center (Parent);
    box.Drop_archive_name.Text (archive_name);
    if new_archive then
      box.New_archive_msg.Show;
    end if;
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    case GWindows.Application.Show_Dialog (box, Parent) is
      when GWindows.Constants.IDOK =>
        yes := True;
      when others =>
        yes := False;
    end case;
  end Do_Drop_File_Dialog;

end AZip_GWin.Drop_File_Dialog;
