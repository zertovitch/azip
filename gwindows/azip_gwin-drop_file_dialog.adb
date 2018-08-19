with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Constants;                use GWindows.Constants;

package body AZip_GWin.Drop_file_dialog is

  procedure Do_drop_file_dialog(
    Parent         : in out GWindows.Base.Base_Window_Type'Class;
    archive_name   : in     GString;
    new_archive    : in     Boolean;
    encrypt        : in out Boolean;
    yes            :    out Boolean
  )
  is
    box: Drop_files_box_Type;
    procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      encrypt:= box.Encrypt_check_box.State = Checked;
    end Get_Data;
  begin
    Parent.Focus;
    box.Create_Full_Dialog(Parent);
    box.Center;
    box.Drop_archive_name.Text(archive_name);
    if new_archive then
      box.New_archive_msg.Show;
    end if;
    box.On_Destroy_Handler(Get_Data'Unrestricted_Access);
    case Show_Dialog(box, Parent) is
      when IDOK =>
        yes:= True;
      when others =>
        yes:= False;
    end case;
  end Do_drop_file_dialog;

end AZip_GWin.Drop_file_dialog;
