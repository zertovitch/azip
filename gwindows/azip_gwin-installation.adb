with AZip_GWin.Persistence;

with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Message_Boxes;

with GWin_Util;

with Ada.Command_Line,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Strings.Fixed;

package body AZip_GWin.Installation is

  function Program_Files_32_Bit_Folder return String is
    use Ada.Environment_Variables;
    P_32  : constant String := "ProgramFiles(x86)";  --  This one doesn't exist on 32-bit Windows
    P_Any : constant String := "ProgramFiles";
  begin
    if Exists (P_32) then
      return Value (P_32);
    else
      return Value (P_Any);
    end if;
  end Program_Files_32_Bit_Folder;

  function Executable_Location return Executable_Location_Choice is
    Current_Exe : constant String := Ada.Command_Line.Command_Name;
    use Ada.Environment_Variables, Ada.Strings.Fixed;
    Admin_Path   : constant String := Program_Files_32_Bit_Folder;
    Appdata_Path : constant String := Value ("APPDATA");
  begin
    if Head (Current_Exe, Admin_Path'Length) = Admin_Path then
      return Administrator;
    elsif Head (Current_Exe, Appdata_Path'Length) = Appdata_Path then
      return Current_User;
    else
      return Elsewhere;
    end if;
  end Executable_Location;

  procedure Installation_Dialog
   (Main_Window : in out MDI_Main.MDI_Main_Type;
    First_Visit : in     Boolean
   )
  is
    use GWindows.Application, GWindows.Buttons, GWindows.Message_Boxes;
    box : AZip_Resource_GUI.Install_box_Type;
    --
    procedure Create_Shortcut_Clicked (Dummy : in out GWindows.Base.Base_Window_Type'Class) is
      Exe_Loc : constant Executable_Location_Choice := Executable_Location;
      procedure Message (Text : GString) is
      begin
        Message_Box (box, "AZip Desktop Shortcut", Text, Icon => Information_Icon);
      end Message;
      --
      All_Desktops : Boolean := False;
      --
      procedure Message_2 (Extra_Text : GString) is
      begin
        if All_Desktops then
          Message (
            "If this current instance of AZip is running in Administrator" &
            " mode, a desktop shortcut" &
            " has been created on desktops of all users." & Extra_Text
          );
        else
          Message ("A desktop shortcut has been created on your desktop." & Extra_Text);
        end if;
      end Message_2;
    begin
      if Exe_Loc /= Current_User then
        All_Desktops :=
          Message_Box (box,
            "AZip Desktop Shortcut",
            "Do you want a shortcut on all desktops ?" & NL &
            "Yes: needs AZip running in admin mode." & NL & 
            "No: only your desktop.",
            Yes_No_Box, Question_Icon
          )
          = Yes;
      end if;
      GWin_Util.Create_Desktop_Shortcut (
        "AZip",
        Ada.Command_Line.Command_Name,
        All_Users => All_Desktops
      );
      if Exe_Loc = Elsewhere then
        Message_2 (NL & "*Caution* : shortcut points to this uninstalled instance of AZip.");
      else
        Message_2 ("");
      end if;
    end Create_Shortcut_Clicked;
    --
    procedure Do_Install (Mode : Installation_Mode) is
      use Ada.Directories, Ada.Environment_Variables;
      App_Folder : constant String :=
        (if Mode = Administrator then Program_Files_32_Bit_Folder else Value ("APPDATA")) & "\AZip";
      New_Exe : constant String := App_Folder & "\AZip.exe";
    begin
      Create_Path (App_Folder);
      Copy_File (Ada.Command_Line.Command_Name, New_Exe);
      GWin_Util.Create_Desktop_Shortcut (
        "AZip",
        New_Exe,
        All_Users => Mode = Administrator
      );
      Message_Box (Main_Window,
        "Installation successful",
        "AZip has installed itself successfully." & NL &
        "For your next use of the installed copy of AZip," & NL &
        "a desktop shortcut has been created.",
        Icon => Information_Icon
      );
    exception
      when others =>
        Message_Box (Main_Window,
          "Installation not successful",
          "AZip could not be installed successfully." & NL &
          "Check your access rights.",
          Icon => Error_Icon
      );
    end;
    use AZip_Resource_GUI;
  begin
    box.Create_Full_Dialog (Main_Window);
    if First_Visit then
      box.Label_Install_note_first_visit.Text (
        "You can also reach this box later through the Options -> Install AZip menu."
      );
    else
      box.Label_Install_note_first_visit.Text ("");
    end if;
    --
    case Executable_Location is
      when Administrator =>
        box.Check_box_installed_all_users.State (Checked);
        box.Label_Installed_All_Users.Enable;
      when Current_User =>
        box.Check_box_installed_current_user.State (Checked);
        box.Label_Installed_All_Users.Enable;
      when Elsewhere =>
        box.Check_box_not_installed.State (Checked);
        box.Label_NOT_Installed.Enable;
    end case;
    --
    if AZip_GWin.Persistence.Cfg_file_available then
      box.Radio_button_stealth.State (Checked);
      box.Label_Stealth.Enable;
    else
      box.Radio_button_registry.State (Checked);
      box.Label_Registry.Enable;
    end if;
    --
    box.Button_create_shortcut.Hide;
    box.Button_create_shortcut_permanent.Show;
    box.Button_create_shortcut_permanent.On_Click_Handler (Create_Shortcut_Clicked'Unrestricted_Access);
    --
    box.Button_context_add.Hide;
    box.Button_context_add_permanent.Show;
    box.Button_context_add_permanent.Disable;
    --
    box.Button_extension_choose.Hide;
    box.Button_extension_choose_permanent.Show;
    box.Button_extension_choose_permanent.Disable;
    --
    if Is_Installed then
      box.ID_Install_all_users.Disable;
      box.ID_Install_current_user.Disable;
    end if;
    --
    box.Center;
    --
    case Show_Dialog (box, Main_Window) is
      when ID_Install_all_users    => Do_Install (Administrator);
      when ID_Install_current_user => Do_Install (Current_User);
      when others =>  --  Includes IDOK
        null;
    end case;
  end Installation_Dialog;

end AZip_GWin.Installation;
