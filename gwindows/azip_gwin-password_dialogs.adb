with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;

with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package body azip_gwin.password_dialogs is

  procedure Get_password_decrypt(
    Window     : in     MDI_Child_Type;
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
                 --  Immediate UI parent
    entry_name : in     GString;
    password   : in out GString_Unbounded
  )
  is
      box: Password_decryption_box_Type;
      pwd_candidate: GString_Unbounded:= Window.current_password;
      --
      procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
        pragma Warnings(off, dummy);
      begin
        pwd_candidate:= G2GU(box.Password_edit.Text);
        Window.Parent.opt.show_passwords:= box.Show_password_box.State = Checked;
      end Get_Data;
      --
      procedure Show_or_Hide_Password ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
        pragma Warnings(off, dummy);
      begin
        if box.Show_password_box.State = Checked then
          box.Password_edit.Password(Off);
        else
          box.Password_edit.Password('=');
        end if;
        box.Password_edit.Redraw;
        box.Password_edit.Focus;
      end Show_or_Hide_Password;
    begin
      box.Create_Full_Dialog(Parent);
      box.Encrypted_entry.Text(entry_name);
      box.Password_edit.Text(GU2G(password));
      if Window.Parent.opt.show_passwords then
        box.Show_password_box.State(Checked);
      else
        box.Show_password_box.State(Unchecked);
      end if;
      box.Center;
      box.On_Destroy_Handler(Get_Data'Unrestricted_Access);
      box.Show_password_box.On_Click_Handler(Show_or_Hide_Password'Unrestricted_Access);
      Show_or_Hide_Password(box);
      box.Password_edit.Set_Selection(0,Length(password));
      case Show_Dialog(box, Parent) is
        when IDOK =>
          password:= pwd_candidate;
        when others =>
          null; -- abandond pwd change
      end case;
    end Get_password_decrypt;

end azip_gwin.password_dialogs;
