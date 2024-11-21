--  Derived from GWindows Sample:
--  gwindows/samples/tab_in_dialog_test.adb

with AZip_Resource_GUI;

with GWindows.Application,
     GWindows.Common_Controls;

package body AZip_GWin.Help is

   procedure Quick_Help_Dialog (Parent : in out GWindows.Base.Base_Window_Type'Class)
   is
     use AZip_Resource_GUI;
     box             : Quick_help_box_Type;
     tab_control     : GWindows.Common_Controls.Tab_Window_Control_Type;
     help_on_gui     : aliased Quick_help_tab_gui_Type;
     help_on_install : aliased Quick_help_tab_install_Type;
     help_on_command : aliased Quick_help_tab_command_Type;

     --  procedure Do_Click (Window : in out Base_Window_Type'Class)
     --  is
     --  begin
     --     Text (Top_Window, "Clicked: ---> " & Text (Window));
     --  end Do_Click;

   begin
      box.Create_Full_Dialog (Parent);
      box.Center (Parent);
      tab_control.Create (box, 0, 0, box.Client_Area_Width, box.Client_Area_Height);
      --  Any click in the buttons Button_1, Button_2 below
      --  will freeze the application if the following has
      --  not been called:
      tab_control.Set_As_Control_Parent;
      --  See discussion:
      --  [Gnavi-discuss] Button clicked inside of a tab -> hanging
      --  06.01.2007

      tab_control.Insert_Tab (0, "User Interface");
      tab_control.Insert_Tab (1, "Installation");
      tab_control.Insert_Tab (2, "Command-line");

      Create_As_Control (help_on_gui,     tab_control, "", 0, 0, 10, 10,
         Show => False);
      Create_As_Control (help_on_install, tab_control, "", 0, 0, 10, 10,
         Show => False);
      Create_As_Control (help_on_command, tab_control, "", 0, 0, 10, 10,
         Show => False);

      help_on_gui.Create_Contents (True);
      help_on_install.Create_Contents (True);
      help_on_command.Create_Contents (True);

      tab_control.Tab_Window (0, help_on_gui'Unchecked_Access);
      tab_control.Tab_Window (1, help_on_install'Unchecked_Access);
      tab_control.Tab_Window (2, help_on_command'Unchecked_Access);

      --  declare
      --     Button_1 : Button_Access;
      --     Button_2 : Button_Access;
      --  begin
      --     Button_1 := new Button_Type;
      --     Create (Button_1.all, Window_1, "Button 1",
      --        10, 30, 75, 25, Is_Dynamic => True);
      --     On_Click_Handler (Button_1.all, Do_Click'Unrestricted_Access);
      --
      --     Button_2 := new Button_Type;
      --     Create (Button_2.all, Window_2, "Button 2",
      --        10, 30, 75, 25, Is_Dynamic => True);
      --     On_Click_Handler (Button_2.all, Do_Click'Unrestricted_Access);
      --  end;
      --
      tab_control.Dock (GWindows.Base.Fill);
      help_on_gui.Dock (GWindows.Base.Fill);
      --  Create (Okay, box, "O&k", 20,
      --     Client_Area_Height (box) - 40, 60, 25, ID => IDOK);
      box.Dock_Children;

      GWindows.Application.Show_Dialog (box, Parent);
   end Quick_Help_Dialog;

end AZip_GWin.Help;
