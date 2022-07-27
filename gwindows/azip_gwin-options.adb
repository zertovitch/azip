with AZip_Common;                        use AZip_Common;
with AZip_Common.User_options;           use AZip_Common.User_options;
--  with AZip_GWin.MDI_Child;                use AZip_GWin.MDI_Child;

with AZip_Resource_GUI;                  use AZip_Resource_GUI;

with GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body AZip_GWin.Options is

  procedure On_General_Options (main : in out MDI_Main_Type) is
    --
    box : Option_box_Type;
    candidate : Option_Pack_Type := main.opt;
    --
    procedure Set_Data is
    begin
      box.Extract_directory_edit_box.Text (GU2G (candidate.extract_directory));
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
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
      dir := G2GU (Get_Directory (
        Window       => main,
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
    --
  begin
    box.Create_Full_Dialog (main);
    Set_Data;
    box.Choose_extract_directory_button_permanent.Show;
    box.Choose_extract_directory_button.Hide;
    box.Choose_extract_directory_button_permanent.On_Click_Handler (Choose_extract_directory'Unrestricted_Access);
    box.Center (main);
    box.Small_Icon ("Options_Icon");
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, main) is
      when IDOK     =>
        has_changes := main.opt /= candidate;
        if has_changes then
          main.opt := candidate;
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end On_General_Options;

end AZip_GWin.Options;
