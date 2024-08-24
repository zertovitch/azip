with AZip_Common.User_options;

with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Message_Boxes;

with Ada.Strings.Wide_Unbounded;

package body AZip_GWin.Options is

  procedure On_General_Options (main : in out MDI_Main.MDI_Main_Type) is
    --
    box : AZip_Resource_GUI.Option_box_Type;
    candidate : AZip_Common.User_options.Option_Pack_Type := main.opt;

    use GWindows.Message_Boxes;

    --  Windows' Get_Directory ends directory names with '\' for drive letters.
    --  GNAT's run-time's Ada.Directories.Compose expects the same for Containing_Directory.
    --  Reason may be that without '\', the location is implicit ("R:file.txt" means
    --  "file.txt in the current directory of drive R").
    --  In other cases, the trailing '\' is optional for GNAT's Compose.

    function Complete_Drive_Letter (s : GString) return GString is
    (if s'Length = 2 and then s (s'Last) = ':' then s & '\' else s);

    ---------------------------------------------------------
    --  Data exchange between dialog and Option_Pack_Type  --
    ---------------------------------------------------------

    procedure Set_Data is
    begin
      box.Extract_Directory_Edit_Box.Text (GU2G (candidate.suggested_extract_directory));
      box.Temp_Directory_Edit_Box.Text    (GU2G (candidate.temp_directory));
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      candidate.suggested_extract_directory := G2GU (Complete_Drive_Letter (box.Extract_Directory_Edit_Box.Text));
      candidate.temp_directory              := G2GU (Complete_Drive_Letter (box.Temp_Directory_Edit_Box.Text));
    exception
      when others =>
        Message_Box (Window, "Invalid data", "Incomplete reading of your changes", OK_Box, Error_Icon);
    end Get_Data;

    -----------------------
    --  Button handlers  --
    -----------------------

    procedure Choose_Extract_Directory (dummy : in out GWindows.Base.Base_Window_Type'Class) is
      dir : GString_Unbounded;
      use type GString_Unbounded;
    begin
      dir :=
        G2GU
          (GWindows.Common_Dialogs.Get_Directory
            (Window       => main,
             Dialog_Title => "Choose extract directory",
             Initial_Path => box.Extract_Directory_Edit_Box.Text));
      if dir = "" then
        null;  --  Cancel pressed - no change in the edit box
      else
        box.Extract_Directory_Edit_Box.Text (GU2G (dir));
      end if;
    end Choose_Extract_Directory;

    procedure Choose_Temp_Directory (dummy : in out GWindows.Base.Base_Window_Type'Class) is
      dir : GString_Unbounded;
      use type GString_Unbounded;
    begin
      dir :=
        G2GU
          (GWindows.Common_Dialogs.Get_Directory
            (Window       => main,
             Dialog_Title => "Choose temp directory",
             Initial_Path => box.Temp_Directory_Edit_Box.Text));
      if dir = "" then
        null;  --  Cancel pressed - no change in the edit box
      else
        box.Temp_Directory_Edit_Box.Text (GU2G (dir));
      end if;
    end Choose_Temp_Directory;

    has_changes : Boolean;
    use AZip_Common.User_options, Ada.Strings.Wide_Unbounded;

  begin
    box.Create_Full_Dialog (main);
    Set_Data;
    box.Choose_Extract_Directory_Button_permanent.Show;
    box.Choose_Extract_Directory_Button.Hide;
    box.Choose_Extract_Directory_Button_permanent.On_Click_Handler (Choose_Extract_Directory'Unrestricted_Access);
    box.Choose_Temp_Directory_Button_permanent.Show;
    box.Choose_Temp_Directory_Button.Hide;
    box.Choose_Temp_Directory_Button_permanent.On_Click_Handler (Choose_Temp_Directory'Unrestricted_Access);
    box.Center (main);
    box.Small_Icon ("Options_Icon");
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    case GWindows.Application.Show_Dialog (box, main) is
      when GWindows.Constants.IDOK =>
        has_changes := main.opt /= candidate;
        if has_changes then
          if candidate.temp_directory /= "" and then not Is_Temp_Directory_Valid (candidate) then
            Message_Box
              (main,
               "Temporary Directory",
               "The temporary directory is invalid." & NL &
               "This change will be ignored.",
               OK_Box,
               Error_Icon);
            candidate.temp_directory := main.opt.temp_directory;
          end if;
          main.opt := candidate;
        end if;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end On_General_Options;

end AZip_GWin.Options;
