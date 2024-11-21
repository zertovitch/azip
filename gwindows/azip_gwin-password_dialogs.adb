with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Buttons,
     GWindows.Constants,
     GWindows.Edit_Boxes,
     GWindows.Message_Boxes;

with Ada.Strings.Wide_Unbounded;

package body AZip_GWin.Password_Dialogs is

  use GWindows.Buttons, GWindows.Edit_Boxes;
  use Ada.Strings.Wide_Unbounded;

  -----------------------------------------
  --  Dialog: password for *decryption*  --
  -----------------------------------------

  procedure Get_password_for_decryption
    (Window     : in     MDI_Child.MDI_Child_Type;
     Parent     : in out GWindows.Base.Base_Window_Type'Class;
                  --  Immediate UI parent
     entry_name : in     GString;
     password   : in out GString_Unbounded;
     cancelled  :    out Boolean)
  is
    box : AZip_Resource_GUI.Password_decryption_box_Type;
    pwd_candidate : GString_Unbounded := Window.current_password;
    --
    procedure Get_Data (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      pwd_candidate := G2GU (box.Password_edit.Text);
      Window.mdi_root.opt.show_passwords := box.Show_password_box.State = Checked;
    end Get_Data;
    --
    procedure Show_or_Hide_Password (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      if box.Show_password_box.State = Checked then
        box.Password_edit.Password (Off);
      else
        box.Password_edit.Password ('=');
      end if;
      box.Password_edit.Redraw;
      box.Password_edit.Focus;
    end Show_or_Hide_Password;

  begin
    box.Create_Full_Dialog (Parent);
    box.Encrypted_entry.Text (entry_name);
    box.Password_edit.Text (GU2G (password));
    if Window.mdi_root.opt.show_passwords then
      box.Show_password_box.State (Checked);
    else
      box.Show_password_box.State (Unchecked);
    end if;
    box.Center (Window);
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    box.Show_password_box.On_Click_Handler (Show_or_Hide_Password'Unrestricted_Access);
    Show_or_Hide_Password (box);
    box.Password_edit.Set_Selection (0, Length (password));
    case GWindows.Application.Show_Dialog (box, Parent) is
      when GWindows.Constants.IDOK =>
        password := pwd_candidate;
        cancelled := False;
      when others =>
        cancelled := True;  --  abandon pwd change and cancel current operation
    end case;
  end Get_password_for_decryption;

  -----------------------------------------
  --  Dialog: password for *encryption*  --
  -----------------------------------------

  procedure Get_password_for_encryption
    (Window     : in out MDI_Child.MDI_Child_Type;
     cancelled  :    out Boolean)
  is
    box : AZip_Resource_GUI.Password_encryption_box_Type;
    pwd_candidate : GString_Unbounded := Window.current_password;
    pwd_confirm_candidate : GString_Unbounded;
    confirm_ok : Boolean;
    --
    procedure Get_Data (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      pwd_candidate := G2GU (box.Password_edit.Text);
      pwd_confirm_candidate := G2GU (box.Password_confirm_edit.Text);
      Window.mdi_root.opt.show_passwords := box.Show_password_box.State = Checked;
    end Get_Data;
    --
    procedure Show_or_Hide_Password (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      if box.Show_password_box.State = Checked then
        box.Password_edit.Password (Off);
        box.Password_confirm_edit.Disable;
        box.Confirm_Icon.Hide;
        box.Confirm_Password_Label.Disable;
      else
        box.Password_edit.Password ('=');
        box.Password_confirm_edit.Enable;
        box.Confirm_Icon.Show;
        box.Confirm_Password_Label.Enable;
      end if;
      box.Password_confirm_edit.Text ("");
      box.Password_confirm_edit.Redraw;
      box.Password_edit.Redraw;
      box.Password_edit.Focus;
    end Show_or_Hide_Password;

    use GWindows.Message_Boxes;
  begin
    loop
      box.Create_Full_Dialog (Window);
      box.Password_edit.Text (GU2G (Window.current_password));
      if Window.mdi_root.opt.show_passwords then
        box.Show_password_box.State (Checked);
      else
        box.Show_password_box.State (Unchecked);
      end if;
      box.Center (Window);
      box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
      box.Show_password_box.On_Click_Handler (Show_or_Hide_Password'Unrestricted_Access);
      Show_or_Hide_Password (box);
      box.Password_confirm_edit.Password ('=');
      box.Password_edit.Set_Selection (0, Length (Window.current_password));
      case GWindows.Application.Show_Dialog (box, Window) is
        when GWindows.Constants.IDOK =>
          Window.current_password := pwd_candidate;
          cancelled := False;
          if Window.mdi_root.opt.show_passwords then
            confirm_ok := True;
          else
            confirm_ok := pwd_confirm_candidate = pwd_candidate;
          end if;
          if not confirm_ok then
            Message_Box
              (Window, "Password mismatch", "Passwords do not match, please retry",
               OK_Box, Exclamation_Icon);
          end if;
        when others =>
          confirm_ok := False;
          cancelled := True;  --  abandon pwd change and cancel current operation
      end case;
      exit when confirm_ok or cancelled;
    end loop;
  end Get_password_for_encryption;

end AZip_GWin.Password_Dialogs;
