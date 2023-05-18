with AZip_Common;

with AZip_GWin.Drop_File_Dialog,
     AZip_GWin.Help,
     AZip_GWin.Installation,
     AZip_GWin.MDI_Child,
     AZip_GWin.Modal_Dialogs,
     AZip_GWin.Options,
     AZip_GWin.Password_Dialogs,
     AZip_GWin.Persistence,
     AZip_GWin.Toolbars;

with Zip;

with GWindows.Application,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Cursors,
     GWindows.Message_Boxes;

with GWin_Util;

with Ada.Command_Line,
     Ada.Text_IO;

package body AZip_GWin.MDI_Main is

  use AZip_Common;
  use GWindows.Message_Boxes;
  use type GString_Unbounded;

  procedure Focus_an_already_opened_window
    (Window    : MDI_Main_Type;
     File_Name : GString_Unbounded;
     is_open   : out Boolean)
  is
    procedure Identify (Child_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use MDI_Child;
    begin
      if Child_Window.all in MDI_Child_Type'Class then
        declare
          pw : MDI_Child_Type renames MDI_Child_Type (Child_Window.all);
        begin
          if pw.File_Name = File_Name then
            is_open := True;
            Focus (pw);
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open := False;
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window).all,
       Identify'Unrestricted_Access);
  end Focus_an_already_opened_window;

  procedure Redraw_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child.MDI_Child_Type'Class then
      --  !! some content refresh, dbl buffering
      Window.Redraw;
    end if;
  end Redraw_Child;

  procedure Redraw_all (Window : in out MDI_Main_Type) is
  begin
    Window.Redraw;
    --  Redraw(Window.Tool_bar);
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window).all, Redraw_Child'Access);
  end Redraw_all;

  procedure Finish_subwindow_opening
    (m : in out MDI_Main_Type;
     c : in out MDI_Child.MDI_Child_Type)
  is
  begin
    m.User_maximize_restore := True;
    if m.opt.MDI_childen_maximized then
      c.Zoom;
      m.Redraw_all;
    end if;
    --  Show things in the main status bar - effective only after Thaw!
  end Finish_subwindow_opening;

  procedure Open_Child_Window_And_Load (
    Window     : in out MDI_Main_Type;
    File_Name,
    File_Title :        GWindows.GString_Unbounded
  )
  is
    is_open : Boolean;
  begin
    Focus_an_already_opened_window (Window, File_Name, is_open);
    if is_open then
      return;        --  nothing to do, archive already in a window
    end if;
    case Is_valid_Zip_archive (To_UTF_8 (GU2G (File_Name))) is
      when valid =>
        null;
      when with_case_sensitive_duplicates =>
        --  Added 24-Mar-2016. Some crazenuts use archives with identical entry names...
        if Message_Box (
          Window,
          "Zip archive with duplicate full names",
          "Archive " & GU2G (File_Name) & NL &
          "contains at least one case of a pair of identical names; this may cause problems." &
          NL & NL &
          "Continue opening ?",
          Yes_No_Box,
          Warning_Icon
        ) = No
        then
          return;
        end if;
      when invalid =>
        Message_Box (
          Window,
          "Invalid zip archive",
          "File " & GU2G (File_Name) & NL &
          "is not a valid zip archive. Eventually, it's a ZIP64 archive.",
          OK_Box,
          Error_Icon
        );
        return;
      when file_doesnt_exist =>
        Message_Box (
          Window,
          "Zip archive issue",
          "File " & GU2G (File_Name) & NL &
          "doesn't exist, or is locked.",
          OK_Box,
          Error_Icon
        );
        return;
    end case;
    declare
      New_Window : constant MDI_Child.MDI_Child_Access :=
        new MDI_Child.MDI_Child_Type;
    begin
      --  We do here like Excel or Word: close the unused blank window
      Window.Close_Initial_Document;
      --
      Window.User_maximize_restore := False;
      New_Window.File_Name := File_Name;
      if Window.opt.extract_directory = "" then
        --  First suggested extract directory is the archive's directory.
        New_Window.extract_dir := G2GU (Give_path (GU2G (File_Name)));
      else
        --  A default extract directory is set as option.
        New_Window.extract_dir := Window.opt.extract_directory;
      end if;
      New_Window.Create_MDI_Child
        (Window,
         GU2G (File_Title),
         Is_Dynamic => True);
      New_Window.Short_Name := File_Title;
      MDI_Active_Window (Window, New_Window.all);
      Update_Common_Menus (Window, GU2G (New_Window.File_Name));
      New_Window.Load_Archive_Catalogue (False);
      Finish_subwindow_opening (Window, New_Window.all);
      New_Window.Focus;
    end;
  exception
--    when E : TC.Input.Load_Error =>
--      Message_Box(
--        Window,
--        "Error when loading archive data",
--        Ada.Exceptions.Exception_Message(E),
--        Icon => Exclamation_Icon
--      );
    when Ada.Text_IO.Name_Error =>
      Message_Box (Window, "Error", "Archive file not found", Icon => Exclamation_Icon);
  end Open_Child_Window_And_Load;

  procedure Open_Child_Window_And_Load
    (Window     : in out MDI_Main_Type;
     File_Name  :        GWindows.GString_Unbounded)
  is
    use Office_Applications;
  begin
    Open_Child_Window_And_Load (
      Window,
      File_Name,
      G2GU (Shorten_File_Name (GU2G (File_Name), 50))
    );
  end Open_Child_Window_And_Load;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Main_Type) is
    use Ada.Command_Line, AZip_Resource_GUI,
        GWindows.Application, GWindows.Cursors, GWindows.Taskbar, GWindows.Windows;
    --
    --  Replace AZip default values by system-dependent ones (here those of GWindows)
    --
    procedure Replace_default (x : in out Integer) is
    begin
      if x = AZip_Common.User_options.use_default then
        x := GWindows.Constants.Use_Default;
      end if;
    end Replace_default;
    --
  begin
    AZip_GWin.Persistence.Blockwise_IO.Load (Window.opt);
    for m in Window.MRU.Item'Range loop
      Window.MRU.Item (m) :=
        (Name => Window.opt.mru (m),
         Line => 0);
    end loop;
    Replace_default (Window.opt.win_left);
    Replace_default (Window.opt.win_width);
    Replace_default (Window.opt.win_top);
    Replace_default (Window.opt.win_height);

    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");

    --  ** Menus and accelerators:

    Window.Menu.Create_Full_Menu;
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 3);
    Accelerator_Table (Window, "Main_Menu");
    Window.MRU.ID_Menu :=
      (IDM_MRU_1,       IDM_MRU_2,       IDM_MRU_3,       IDM_MRU_4,
       IDM_MRU_5,       IDM_MRU_6,       IDM_MRU_7,       IDM_MRU_8,
       IDM_MRU_9
      );

    --  ** Main tool bar (add / remove / ...) at top left of the main window:

    Toolbars.Init_Main_Tool_Bar (Window.Tool_Bar, Window);

    --  ** Other resources
    Window.Folders_Images.Create (Num_resource (Folders_BMP), 16);

    --  ** Resize according to options:

    if Screen_Visibility ((Window.opt.win_left, Window.opt.win_top)) = Good then
      Left (Window, Window.opt.win_left);
      Top  (Window, Window.opt.win_top);
    end if;
    Size (Window,
      Integer'Max (400, Window.opt.win_width),
      Integer'Max (200, Window.opt.win_height)
    );
    Zoom (Window, Window.opt.MDI_main_maximized);

    Window.Dock_Children;
    Window.Show;

    if Argument_Count = 0 then
      On_File_New (Window, extra_first_doc => True);
      --  ^ The MS Office-like first, empty document
    else
      declare
        args : Array_Of_File_Names (1 .. Argument_Count);
      begin
        for I in 1 .. Argument_Count loop
          args (I) := G2GU (To_UTF_16 (Argument (I), Zip.UTF_8));
        end loop;
        --  We simulate a file dropping onto the MDI main window.
        Window.On_File_Drop (args);
        Update_Common_Menus (Window);
      end;
    end if;
    --  Dropping files on the background will trigger creating an archive:
    Window.Accept_File_Drag_And_Drop;
    Window.record_dimensions := True;
    --
    begin
      Window.Task_bar_gadget.Set_Progress_State (Window, No_Progress);
      Window.Task_bar_gadget_ok := True;
    exception
      when Taskbar_Interface_Not_Supported =>
        Window.Task_bar_gadget_ok := False;
    end;
    --
    Window.dragging.cursor_drag_unpack := Load_Cursor ("Drag_Unpack_Cursor");
    Window.dragging.cursor_drag_no_way := Load_System_Cursor (IDC_NO);
    Window.dragging.cursor_arrow       := Load_System_Cursor (IDC_ARROW);
    --
    if Window.opt.first_visit then
      Modal_Dialogs.Show_Sponsoring_Box (Window, First_Visit => True);
      Installation.Installation_Dialog (Window, First_Visit => True);
      Window.opt.first_visit := False;
    end if;
  end On_Create;

  function Minimized (Window : GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left (Window) <= -32000;
  end Minimized;

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer) is
  begin
    if Window.record_dimensions and not (Window.Zoom or Minimized (Window)) then
      --  ^ Avoids recording dimensions before restoring them
      --    from previous session.
      Window.opt.win_left := Left;
      Window.opt.win_top  := Top;
      --  Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  procedure On_Size (Window : in out MDI_Main_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
  begin
    Dock_Children (Window);
    if Window.record_dimensions and not (Window.Zoom or Minimized (Window)) then
      --  ^ Avoids recording dimensions before restoring them
      --    from previous session.
      Window.opt.win_width  := Width;
      Window.opt.win_height := Height;
      --  Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  Current_MDI_Window : Natural := 0;

  procedure On_File_New (
    Window          : in out MDI_Main_Type;
    extra_first_doc :        Boolean;
    New_Window      : in     MDI_Child.MDI_Child_Access
  )
  is

    function Suffix return GWindows.GString is
    begin
      if Current_MDI_Window = 0 then
        return "";
      else
        return Integer'Wide_Image (Current_MDI_Window + 1);
      end if;
    end Suffix;

    File_Title : constant GString := "New Archive" & Suffix;

  begin
    New_Window.Extra_First_Doc := extra_first_doc;
    Window.User_maximize_restore := False;
    New_Window.Create_MDI_Child (Window, File_Title, Is_Dynamic => True);
    New_Window.Short_Name := G2GU (File_Title);
    MDI_Active_Window (Window, New_Window.all);

    --  Transfer user-defined default options:
    --  New_Window.xxx.Opt:= Gen_Opt.Options_For_New;
    --  Refresh_size_dependent_parameters(
    --  New_Window.Draw_Control.Picture,
    --  objects => True
    --  );

    Current_MDI_Window := Current_MDI_Window + 1;

    --  This is just to set the MRUs in the new window's menu:
    Update_Common_Menus (Window);

    Finish_subwindow_opening (Window, New_Window.all);
  end On_File_New;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc : Boolean) is
    New_Window : constant MDI_Child.MDI_Child_Access :=
      new MDI_Child.MDI_Child_Type;
  begin
    On_File_New (Window, extra_first_doc, New_Window);
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (Window : in out MDI_Main_Type) is
    File_Name, File_Title : GString_Unbounded;
    Success    : Boolean;
  begin
    GWindows.Common_Dialogs.Open_File
      (Window, "Open Zip archive",
       File_Name, Zip_archives_filters, ".zip", File_Title,
       Success);
    if Success then
      Open_Child_Window_And_Load (Window, File_Name, File_Title);
    end if;
  end On_File_Open;

  procedure On_File_Drop (Window     : in out MDI_Main_Type;
                          File_Names : in     GWindows.Windows.Array_Of_File_Names) is
    New_Window : MDI_Child.MDI_Child_Access;
    encrypt    : Boolean := False;
    yes        : Boolean;
    cancelled  : Boolean;
    use AZip_GWin.Password_Dialogs, MDI_Child, Zip;
  begin
    Window.Focus;
    if Window.Count_MDI_Children > 0 and then Window.opt.MDI_childen_maximized then
      --  If children windows are maximized, it is intuitive that files dropped (on the small
      --   areas like the window borders or the tool bar) are for the focused child window.
      declare
        use GWindows.Base;
        cwc : constant Pointer_To_Base_Window_Class :=
          Window.MDI_Active_Window;
      begin
        if cwc /= null and then cwc.all in MDI_Child_Type'Class then
          MDI_Child_Type (cwc.all).On_File_Drop (File_Names);
        end if;
      end;
    else
      --  Files are dropped onto the background
      --  area of the MDI main window.
      --
      if All_Zip_files (File_Names) then
        --  All files are Zip archives (even those without .zip extension).
        for i in File_Names'Range loop
          Open_Child_Window_And_Load (Window, File_Names (i));
        end loop;
      else
        Drop_File_Dialog.Do_Drop_File_Dialog
          (Parent         => Window,
           archive_name   => "(A new Zip archive)",
           new_archive    => True,
           encrypt        => encrypt,
           yes            => yes);
        if yes then
          New_Window := new MDI_Child_Type;
          On_File_New (Window, extra_first_doc => False, New_Window => New_Window);
          New_Window.On_Save_As;
          --
          if Is_loaded (New_Window.zif) then
            if encrypt then
              Get_password_for_encryption (New_Window.all, cancelled);
            else
              cancelled := False;
            end if;
            if not cancelled then
              New_Window.Go_for_adding (File_Names, Encrypt => encrypt);
            end if;
          end if;
        end if;
      end if;
    end if;
  end On_File_Drop;

  ----------------------
  -- My_MDI_Close_All --
  ----------------------

  procedure My_MDI_Close_All (Main_Window : in out MDI_Main_Type) is
    procedure My_Close_Win (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    --  Enumeration call back to close MDI child windows
    is
    begin
      if Any_Window.all in MDI_Child.MDI_Child_Type'Class and then
        Main_Window.Success_in_enumerated_close
      then  --  No [cancel] button was selected up to now.
        GWindows.Base.Close (Any_Window.all);
      end if;
    end My_Close_Win;
  begin
    Main_Window.Success_in_enumerated_close := True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Main_Window).all,
                                      My_Close_Win'Unrestricted_Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer)
  is
    use AZip_Resource_GUI;
  begin
    case Item is
      when IDM_NEW_ARCHIVE =>
        On_File_New (Window, extra_first_doc => False);
      when IDM_OPEN_ARCHIVE =>
        On_File_Open (Window);
      when IDM_Quick_Help =>
        Help.Quick_Help_Dialog (Window);
      when IDM_ABOUT =>
        Modal_Dialogs.Show_About_Box (Window);
      when IDM_Sponsoring =>
        Modal_Dialogs.Show_Sponsoring_Box (Window, First_Visit => False);
      when IDM_Install =>
        Installation.Installation_Dialog (Window, First_Visit => False);
      when IDM_Web =>
        GWin_Util.Start (azip_web_page);
      when IDM_AZip_Web_news =>
        GWin_Util.Start (azip_news_web_page);
      when IDM_QUIT =>
        Close (Window);
      when IDM_CLOSE_ARCHIVE =>
        if Window.Count_MDI_Children = 0 then
          Close (Window);  --  Ctrl-W when no subwindow is open.
        else
          GWindows.Windows.Window_Type (Window).On_Menu_Select (Item);
        end if;
      when IDM_WINDOW_CASCADE   =>
        MDI_Cascade (Window);
      when IDM_WINDOW_TILE_HORIZONTAL =>
        MDI_Tile_Horizontal (Window);
      when IDM_WINDOW_TILE_VERTICAL =>
        MDI_Tile_Vertical (Window);
      when IDM_WINDOW_CLOSE_ALL =>
        My_MDI_Close_All (Window);
      when IDM_General_options =>
        Options.On_General_Options (Window);
      when others =>
        for i_mru in Window.MRU.ID_Menu'Range loop
          if Item = Window.MRU.ID_Menu (i_mru) then
            Open_Child_Window_And_Load (
              Window,
              Window.MRU.Item (i_mru).Name
            );
            exit;
          end if;
        end loop;
       GWindows.Windows.Window_Type (Window).On_Menu_Select (Item);
    end case;
  end On_Menu_Select;

  -------------

  procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean)
  is
  begin
    Window.opt.MDI_main_maximized := Window.Zoom;
    if not (Window.opt.MDI_main_maximized or Minimized (Window)) then
      Window.opt.win_left   := Left (Window);
      Window.opt.win_top    := Top (Window);
      Window.opt.win_width  := Width (Window);
      Window.opt.win_height := Height (Window);
    end if;

    My_MDI_Close_All (Window);
    --  ^ Don't forget to save unsaved files !
    --  Operation can be cancelled by user for one unsaved picture.
    Can_Close := Window.Success_in_enumerated_close;
    --
    if Can_Close then
      for m in Window.MRU.Item'Range loop
        Window.opt.mru (m) := Window.MRU.Item (m).Name;
      end loop;
      AZip_GWin.Persistence.Blockwise_IO.Save (Window.opt);
      GWindows.Base.On_Exception_Handler (Handler => null);
      --  !! Trick to remove a strange crash on Destroy_Children
      --  !! on certain Windows platforms - 29-Jun-2012
    end if;
  end On_Close;

  procedure Update_Common_Menus_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
    use MDI_Child, Office_Applications;
  begin
    if Window.all in MDI_Child_Type'Class then
      declare
        cw : MDI_Child_Type renames MDI_Child_Type (Window.all);
      begin
        Update_MRU_Menu (cw.MDI_Root.MRU, cw.Menu.Popup_0001);
        --  Update_Toolbar_Menu(cw.View_menu, cw.parent.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus (Window    : in out MDI_Main_Type;
                                 top_entry :        GString := "")
  is
    use Office_Applications;
  begin
    if top_entry /= "" then
      Add_MRU
        (Window.MRU,
         top_entry,
         Add_To_Desktop => not AZip_GWin.Persistence.Key_IO.Is_Config_File_Available);
    end if;
    Update_MRU_Menu (Window.MRU, Window.Menu.Popup_0001);
    --  Update_Toolbar_Menu(Window.View_menu, Window.Floating_toolbars);
    GWindows.Base.Enumerate_Children (
      MDI_Client_Window (Window).all,
      Update_Common_Menus_Child'Access
    );
  end Update_Common_Menus;

  function All_Zip_files (File_Names : GWindows.Windows.Array_Of_File_Names) return Boolean is
    all_valid, empty : Boolean := True;
  begin
    for i in File_Names'Range loop
      empty := False;
      all_valid := all_valid and
        Is_valid_Zip_archive (To_UTF_8 (GU2G (File_Names (i)))) = valid;
    end loop;
    return all_valid and not empty;
  end All_Zip_files;

  function Confirm_archives_if_all_Zip_files
    (Window     : GWindows.Base.Base_Window_Type'Class;
     File_Names : GWindows.Windows.Array_Of_File_Names)
  return Boolean
  is
    answer : Message_Box_Result;
  begin
    if All_Zip_files (File_Names) then
      if File_Names'Length = 1 then
        answer :=
          Message_Box (
            Window,
            "File is a Zip archive",
            "Should AZip open this Zip archive individually," & NL &
            "in a separate window ?" & NL &
            "If not, it will be added as a file *into* a Zip archive.",
            Yes_No_Box,
            Question_Icon
          );
      else
        answer :=
          Message_Box (
            Window,
            "Files are Zip archives",
            "Should AZip open these Zip archives individually," & NL &
            "in separate windows ?" & NL &
            "If not, they will be added as files *into* a Zip archive.",
            Yes_No_Box,
            Question_Icon
          );
      end if;
      if answer = Yes then
        return True;
      end if;
    end if;
    return False;
  end Confirm_archives_if_all_Zip_files;

end AZip_GWin.MDI_Main;
