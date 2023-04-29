with AZip_Common.User_options;

with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Message_Boxes;

package body AZip_GWin.Options is

  procedure On_General_Options (main : in out MDI_Main.MDI_Main_Type) is
    --
    box : AZip_Resource_GUI.Option_box_Type;
    candidate : AZip_Common.User_options.Option_Pack_Type := main.opt;
    --
    procedure Set_Data is
    begin
      box.Extract_directory_edit_box.Text (GU2G (candidate.extract_directory));
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
      use GWindows.Message_Boxes;
    begin
      candidate.extract_directory := G2GU (box.Extract_directory_edit_box.Text);
    exception
      when others =>
        Message_Box (Window, "Invalid data", "Incomplete reading of your changes", OK_Box, Error_Icon);
    end Get_Data;
    --
    procedure Choose_extract_directory (dummy : in out GWindows.Base.Base_Window_Type'Class) is
      dir : GString_Unbounded;
      use type GString_Unbounded;
    begin
      dir :=
        G2GU
          (GWindows.Common_Dialogs.Get_Directory
            (Window       => main,
             Dialog_Title => "Choose extract directory",
             Initial_Path => box.Extract_directory_edit_box.Text));
      if dir = "" then
        null;  --  Cancel pressed - no change in the edit box
      else
        box.Extract_directory_edit_box.Text (GU2G (dir));
      end if;
    end Choose_extract_directory;
    --
    has_changes : Boolean;
    use AZip_Common.User_options;
    --
  begin
    box.Create_Full_Dialog (main);
    Set_Data;
    box.Choose_extract_directory_button_permanent.Show;
    box.Choose_extract_directory_button.Hide;
    box.Choose_extract_directory_button_permanent.On_Click_Handler (Choose_extract_directory'Unrestricted_Access);
    box.Center (main);
    box.Small_Icon ("Options_Icon");
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    case GWindows.Application.Show_Dialog (box, main) is
      when GWindows.Constants.IDOK =>
        has_changes := main.opt /= candidate;
        if has_changes then
          main.opt := candidate;
        end if;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end On_General_Options;

end AZip_GWin.Options;
