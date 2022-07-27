with AZip_GWin.Persistence;

with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Message_Boxes,
     GWindows.Registry;

with GWin_Util;

with Ada.Command_Line,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Strings.Fixed;

package body AZip_GWin.Installation is

  Program_Files_Folder : constant String := "ProgramFiles";

  function Executable_Location return Executable_Location_Choice is
    Current_Exe : constant String := Ada.Command_Line.Command_Name;
    use Ada.Environment_Variables, Ada.Strings.Fixed;
    Admin_Path   : constant String := Program_Files_Folder;
    Appdata_Path : constant String := Value ("APPDATA");
  begin
    if Head (Current_Exe, Admin_Path'Length) = Admin_Path then
      return All_Users;
    elsif Head (Current_Exe, Appdata_Path'Length) = Appdata_Path then
      return Current_User;
    else
      return Elsewhere;
    end if;
  end Executable_Location;

  procedure Do_Create_Shortcut (Target_Exe : String; All_Desktops : Boolean) is
  begin
    GWin_Util.Create_Desktop_Shortcut (
      (if All_Desktops then "AZip" else "My AZip"),
      Target_Exe,
      User_Scope => (if All_Desktops then GWin_Util.All_Users else GWin_Util.Current_User)
    );
  end Do_Create_Shortcut;

  --  Here is an explanation about the shortcut creation:
  --
  --                                           Shortcut for...
  --      Executable Location         \ Current User  |   All Users
  --      -------------------------------------------------------------
  --      All_Users (Prog. Files)     |      OK       |    Install    |
  --      -------------------------------------------------------------
  --      Current_User  (%appdata%)   |    Install    |*** No Way! ***|
  --      -------------------------------------------------------------
  --      Elsewhere                   |      OK       |      OK       |
  --      -------------------------------------------------------------
  --
  --          OK      : possible with the "Create Shortcut" button
  --                      (a warning is given if Location = Elsewhere)
  --          Install : done by default on installation but also
  --                      available with the "Create Shortcut" button
  --                      for restoring a deleted shortcut

  Context_Menu_Msg_Box_Title : constant GString :=
    "AZip Explorer Context (""Right-Click"") Menu Entries";

  Desktop_Shortcut_Msg_Box_Title : constant GString :=
    "AZip Desktop Shortcut";

  procedure Do_Manage_Context_Menu (
    Parent_Box :     AZip_Resource_GUI.Install_box_Type;
    Target_Exe :     String;
    All_Menus  :     Boolean;
    Action     :     GWin_Util.Context_Menu_Action;
    Success    : out Boolean
  )
  is
    use GWin_Util, GWindows.Message_Boxes;
  begin
    Success := True;
    for Subject in Context_Menu_Subject loop
      begin
        Explorer_Context_Menu (
          Entry_Name  => "AZip_Menu_Item"
                         --  & (if All_Menus then "_All_Users" else "_Curr_User"),
                         ,
          Subject     => Subject,
          User_Scope  => (if All_Menus then
                            GWin_Util.All_Users
                          else
                            GWin_Util.Current_User),
          Action      => Action,
          Entry_Label => (if Subject = Any_File then
                            "AZip: open archive / add file to archive"
                          else
                            "AZip: add folder to Zip archive"),
          Command     => S2G (Target_Exe) & " ""%1""",
          Icon_Path   => S2G (Target_Exe)
        );
      exception
        when GWindows.Registry.REGISTRY_ERROR =>
          case Action is
            when Add =>
              Message_Box (Parent_Box, Context_Menu_Msg_Box_Title,
                "Error" & NL &
                "Cannot add entry for " & Subject'Wide_Image & '.' & NL &
                "Eventually, there is an access right issue.",
                Icon => Error_Icon);
            when Remove =>
              Message_Box (Parent_Box, Context_Menu_Msg_Box_Title,
                "Error" & NL &
                "Entry for " & Subject'Wide_Image & " was perhaps already removed" & NL &
                "or there is an access right issue.",
                Icon => Error_Icon);
          end case;
          Success := False;
      end;
    end loop;
  end Do_Manage_Context_Menu;

  procedure Installation_Dialog
   (Main_Window : in out MDI_Main.MDI_Main_Type;
    First_Visit : in     Boolean
   )
  is
    use GWindows.Application, GWindows.Buttons, GWindows.Message_Boxes;
    box : AZip_Resource_GUI.Install_box_Type;
    Exe_Loc : constant Executable_Location_Choice := Executable_Location;
    --
    procedure Create_Shortcut_Clicked (Dummy : in out GWindows.Base.Base_Window_Type'Class) is
      procedure Message (Text : GString) is
      begin
        Message_Box (box, Desktop_Shortcut_Msg_Box_Title, Text, Icon => Information_Icon);
      end Message;
      --
      All_Desktops : Boolean;
      --
      procedure Message_2 (Extra_Text : GString) is
      begin
        if All_Desktops then
          Message (
            "If this current instance of AZip is running in" &
            " Administrator mode, a desktop shortcut" &
            " has been created on desktops of all users." & Extra_Text
          );
        else
          Message ("A desktop shortcut has been created on your desktop." & Extra_Text);
        end if;
      end Message_2;
    begin
      if Exe_Loc = Current_User then
        All_Desktops := False;
        --  A shortcut, visible on all desktops, to a personal copy of
        --  the exectuable, is not something we want...
      else
        All_Desktops :=
          Message_Box (box,
            Desktop_Shortcut_Msg_Box_Title,
            "Do you want a shortcut on all desktops ?" & NL & NL &
            "Yes : Needs AZip running in admin mode." & NL &
            "No : Only on your desktop.",
            Yes_No_Box, Question_Icon
          )
          = Yes;
      end if;
      Do_Create_Shortcut (Ada.Command_Line.Command_Name, All_Desktops);
      if Exe_Loc = Elsewhere then
        Message_2 (
          NL & NL & "*Caution* : shortcut points to this non-installed instance of AZip."
        );
      else
        Message_2 ("");
      end if;
    end Create_Shortcut_Clicked;
    --
    procedure Manage_Context_Menu_Clicked (Dummy : in out GWindows.Base.Base_Window_Type'Class) is
      use Ada.Command_Line, GWin_Util;
      Action : Context_Menu_Action;
      is_success : Boolean;
      --
      procedure Message (Text : GString) is
      begin
        Message_Box (box, Context_Menu_Msg_Box_Title, Text, Icon => Information_Icon);
      end Message;
      --
      Menus_All_Users : Boolean;
      --
      procedure Message_2 (Extra_Text : GString) is
        Action_Img : constant GString :=
          (if Action = Add then "added" else "removed");
        Destination : constant GString :=
          (if Menus_All_Users then "all users" else "you");
      begin
        Message (
          "Context menu entries for Any File and Any Folder have been " &
          Action_Img &
          " for " &
          Destination & "." &
          Extra_Text
        );
      end Message_2;
      --
    begin
      case Exe_Loc is
        when Current_User =>
          Menus_All_Users := False;
          --  A context menu command pointings to a personal copy of
          --  the exectuable, is not something we want...
        when All_Users =>
          Menus_All_Users := True;
          --  Contrary to the shortcut, we are less flexible in the All Users case.
          --  We want to avoid having 2 menu entries doing the same thing.
        when Elsewhere =>
          Menus_All_Users :=
            Message_Box (box,
              Context_Menu_Msg_Box_Title,
              "Do you want to manage context menu entries for all users ?" & NL & NL &
              "Yes : Needs AZip running in admin mode." & NL &
              "No : Will influence only *your* context menu.",
              Yes_No_Box, Question_Icon
            )
          = Yes;
      end case;
      if Message_Box (
        box,
        Context_Menu_Msg_Box_Title,
        "Do you want to add or remove context menu entries ?" & NL & NL &
        "Yes : Add." & NL &
        "No : Remove.",
        Yes_No_Box, Question_Icon
        )
        = Yes
      then
        Action := Add;
      else
        Action := Remove;
      end if;
      --
      Do_Manage_Context_Menu (box, '"' & Command_Name & '"', Menus_All_Users, Action, is_success);
      --
      if is_success then
        if Action = Add and then Exe_Loc = Elsewhere then
          Message_2 (
            NL & NL &
            "*Caution* : context menu commands points to this non-installed instance of AZip."
          );
        else
          Message_2 ("");
        end if;
      end if;
    end Manage_Context_Menu_Clicked;
    --
    --  Here the really serious stuff: AZip installs itself to a permanent location.
    --
    procedure Do_Install (Mode : Installation_Mode) is
      use Ada.Directories, Ada.Environment_Variables, GWin_Util;
      App_Folder : constant String :=
        (if Mode = All_Users then Program_Files_Folder else Value ("APPDATA")) & "\AZip";
      New_Exe : constant String := App_Folder & "\AZip.exe";
      Success : Boolean;
      procedure Complaint is
      begin
        Message_Box (Main_Window,
          "Installation not successful",
          "AZip could not be installed successfully." & NL & NL &
          "There is perhaps an access right issue (Administrator mode is " &
          "needed for an installation for all users).",
          Icon => Error_Icon
        );
      end Complaint;
    begin
      Create_Path (App_Folder);
      Copy_File (Ada.Command_Line.Command_Name, New_Exe);  --  Self-propagation :-)
      --
      Do_Create_Shortcut (New_Exe, Mode = All_Users);
      Do_Manage_Context_Menu (box, New_Exe, Mode = All_Users, Add, Success);
      if Success then
        Message_Box (Main_Window,
          "Installation successful",
          "AZip has installed itself successfully." & NL & NL &
          "For your next use of the *installed* copy of AZip," & NL &
          "   - a desktop shortcut has been created;" & NL &
          "   - context menu (""right-click"") entries have been added.",
          Icon => Information_Icon
        );
      else
        Complaint;
      end if;
    exception
      when others =>
        Complaint;
    end Do_Install;
    use AZip_Resource_GUI, GWin_Util;
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
    case Exe_Loc is
      when All_Users =>
        box.Check_box_installed_all_users.State (Checked);
        box.Label_Installed_All_Users.Enable;
      when Current_User =>
        box.Check_box_installed_current_user.State (Checked);
        box.Label_Installed_Current_User.Enable;
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
    box.Button_context_add_permanent.On_Click_Handler (Manage_Context_Menu_Clicked'Unrestricted_Access);
    --
    box.Button_extension_choose.Hide;
    box.Button_extension_choose_permanent.Show;
    box.Button_extension_choose_permanent.Disable;
    --
    if Is_Installed then
      box.ID_Install_all_users.Disable;
      box.ID_Install_current_user.Disable;
    else
      box.ID_Install_all_users.Text    (Right_Arrow & "  " & box.ID_Install_all_users.Text);
      box.ID_Install_current_user.Text (Right_Arrow & "  " & box.ID_Install_current_user.Text);
    end if;
    --
    box.Center;
    --
    case Show_Dialog (box, Main_Window) is
      when ID_Install_all_users    => Do_Install (All_Users);
      when ID_Install_current_user => Do_Install (Current_User);
      when others =>  --  Includes IDOK
        null;
    end case;
  end Installation_Dialog;

end AZip_GWin.Installation;
