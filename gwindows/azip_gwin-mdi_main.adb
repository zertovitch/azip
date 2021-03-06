with AZip_Common;                       use AZip_Common;
with AZip_GWin.Drop_file_dialog;        use AZip_GWin.Drop_file_dialog;
with AZip_GWin.Help;                    use AZip_GWin.Help;
with AZip_GWin.Installation;
with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with AZip_GWin.Modal_Dialogs;
with AZip_GWin.Options;                 use AZip_GWin.Options;
with AZip_GWin.Password_dialogs;
with AZip_GWin.Persistence;
with AZip_GWin.Toolbars;

with Zip;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with GWin_Util;

with Ada.Command_Line,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body AZip_GWin.MDI_Main is

  use type GString_Unbounded;

  procedure Focus_an_already_opened_window(
    Window    : MDI_Main_Type;
    File_Name : GString_Unbounded;
    is_open   : out Boolean )
  is
    procedure Identify (Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Window.all in MDI_Child_Type'Class then
        declare
          pw: MDI_Child_Type renames MDI_Child_Type(Window.all);
        begin
          if pw.File_Name = File_Name then
            is_open:= True;
            Focus(pw);
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open:= False;
    Enumerate_Children(
      MDI_Client_Window (Window).all,
      Identify'Unrestricted_Access
    );
  end Focus_an_already_opened_window;

  procedure Redraw_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      -- !! some content refresh, dbl buffering
      Window.Redraw;
    end if;
  end Redraw_Child;

  procedure Redraw_all(Window: in out MDI_Main_Type) is
  begin
    Window.Redraw;
    -- Redraw(Window.Tool_bar);
    Enumerate_Children(MDI_Client_Window (Window).all,Redraw_Child'Access);
  end Redraw_all;

  procedure Close_extra_first_child(Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      declare
        w: MDI_Child_Type renames MDI_Child_Type(Window.all);
      begin
        if w.Extra_first_doc and Is_file_saved(w) then
          Window.Close;
        end if;
      end;
    end if;
  end Close_extra_first_child;

  procedure Close_extra_first_child(Window: in out MDI_Main_Type) is
  begin
    Enumerate_Children(MDI_Client_Window (Window).all,Close_extra_first_child'Access);
  end Close_extra_first_child;

  procedure Finish_subwindow_opening(
    m : in out MDI_Main_Type;
    c : in out MDI_Child_Type )
  is
  begin
    m.User_maximize_restore:= True;
    if m.opt.MDI_childen_maximized then
      Zoom(c);
      Redraw_all(m);
    end if;
    -- Show things in the main status bar - effective only after Thaw!
  end Finish_subwindow_opening;

  procedure Open_Child_Window_And_Load (
    Window     : in out MDI_Main_Type;
    File_Name,
    File_Title :        GWindows.GString_Unbounded
  )
  is
    is_open: Boolean;
  begin
    Focus_an_already_opened_window( Window, File_Name, is_open );
    if is_open then
      return;        -- nothing to do, archive already in a window
    end if;
    case Is_valid_Zip_archive(To_UTF_8(GU2G(File_Name))) is
      when valid =>
        null;
      when with_case_sensitive_duplicates =>
        --  Added 24-Mar-2016. Some crazenuts use archives with identical entry names...
        if Message_Box(
          Window,
          "Zip archive with duplicate full names",
          "Archive " & GU2G(File_Name) & NL &
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
        Message_Box(
          Window,
          "Invalid zip archive",
          "File " & GU2G(File_Name) & NL &
          "is not a valid zip archive. Eventually, it's a ZIP64 archive.",
          OK_Box,
          Error_Icon
        );
        return;
      when file_doesnt_exist =>
        Message_Box(
          Window,
          "Zip archive issue",
          "File " & GU2G(File_Name) & NL &
          "doesn't exist, or is locked.",
          OK_Box,
          Error_Icon
        );
        return;
    end case;
    declare
      New_Window : constant MDI_Child_Access := new MDI_Child_Type;
    begin
      -- We do here like Excel or Word: close the unused blank window
      Close_extra_first_child(Window);
      --
      Window.User_maximize_restore:= False;
      New_Window.File_Name:= File_Name;
      if Window.opt.extract_directory = "" then
        --  First suggested extract directory is the archive's directory.
        New_Window.extract_dir:= G2GU(Give_path(GU2G(File_Name)));
      else
        --  A default extract directory is set as option.
        New_Window.extract_dir:= Window.opt.extract_directory;
      end if;
      Create_MDI_Child (New_Window.all,
        Window,
        GU2G(File_Title),
        Is_Dynamic => True
      );
      New_Window.Short_Name:= File_Title;
      MDI_Active_Window (Window, New_Window.all);
      Update_Common_Menus(Window, GU2G(New_Window.File_Name));
      New_Window.Load_archive_catalogue(False);
      Finish_subwindow_opening(Window, New_Window.all);
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
      Message_Box(Window, "Error", "Archive file not found", Icon => Exclamation_Icon);
  end Open_Child_Window_And_Load;

  procedure On_Button_Select (
        Control : in out MDI_Toolbar_Type;
        Item    : in     Integer           ) is
    Parent : constant MDI_Main_Access := MDI_Main_Access (Controlling_Parent (Control));
  begin
    On_Menu_Select (Parent.all, Item);
  end On_Button_Select;

  function Shorten_file_name( s: GString ) return GString is
    max: constant:= 33;
    beg: constant:= 6;
  begin
    if s'Length < max then
      return s;
    else
      return
        s(s'First .. s'First + beg-1) &       -- beg
        "..." &                               -- 3
        s(s'Last - max + beg + 1 .. s'Last);  -- max - beg - 3
    end if;
  end Shorten_file_name;

  procedure Open_Child_Window_And_Load (
        Window     : in out MDI_Main_Type;
        File_Name  :        GWindows.GString_Unbounded ) is
  begin
    Open_Child_Window_And_Load(
      Window,
      File_Name,
      G2GU(Shorten_file_name(GU2G(File_Name)))
    );
  end Open_Child_Window_And_Load;

  function Valid_Left_Top(Left, Top: Integer)
    return Boolean
  is
  begin
    return Left in -320 .. Desktop_Width  - 30 and
           Top  in -320 .. Desktop_Height - 80;
  end Valid_Left_Top;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create ( Window : in out MDI_Main_Type ) is
    use Ada.Command_Line;
    --
    -- Replace AZip default values by system-dependent ones (here those of GWindows)
    --
    procedure Replace_default(x: in out Integer) is
    begin
      if x = AZip_Common.User_options.use_default then
        x:= GWindows.Constants.Use_Default;
      end if;
    end Replace_default;
    --
  begin
    AZip_GWin.Persistence.Load (Window.opt);
    Replace_default(Window.opt.win_left);
    Replace_default(Window.opt.win_width);
    Replace_default(Window.opt.win_top);
    Replace_default(Window.opt.win_height);

    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");

    -- ** Menus and accelerators:

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 3);
    Accelerator_Table (Window, "Main_Menu");
    Window.IDM_MRU:=
      (IDM_MRU_1,       IDM_MRU_2,       IDM_MRU_3,       IDM_MRU_4,
       IDM_MRU_5,       IDM_MRU_6,       IDM_MRU_7,       IDM_MRU_8,
       IDM_MRU_9
      );

    -- ** Main tool bar (add / remove / ...) at top left of the main window:

    AZip_GWin.Toolbars.Init_Main_toolbar(Window.Tool_Bar, Window.Toolbar_Images, Window);

    -- ** Other resources
    Window.Folders_Images.Create (Num_resource(Folders_BMP), 16);

    -- ** Resize according to options:

    if Valid_Left_Top(Window.opt.win_left, Window.opt.win_top) then
      Left(Window, Window.opt.win_left);
      Top( Window, Window.opt.win_top);
    end if;
    Size(Window,
      Integer'Max(400, Window.opt.win_width),
      Integer'Max(200, Window.opt.win_height)
    );
    Zoom(Window,Window.opt.MDI_main_maximized);

    Window.Dock_Children;
    Window.Show;

    if Argument_Count = 0 then
      On_File_New (Window, extra_first_doc => True);
      -- ^ The MS Office-like first, empty document
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
    Window.record_dimensions:= True;
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

  function Minimized(Window: GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left(Window) <= -32000;
  end Minimized;

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer) is
  begin
    if Window.record_dimensions and
       not (Zoom(Window) or Minimized(Window))
    then
      -- ^ Avoids recording dimensions before restoring them
      --   from previous session.
      Window.opt.win_left  := Left;
      Window.opt.win_top   := Top;
      -- Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  procedure On_Size (Window : in out MDI_Main_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
  begin
    Dock_Children(Window);
    if Window.record_dimensions and
       not (Zoom(Window) or Minimized(Window))
    then
      -- ^ Avoids recording dimensions before restoring them
      --   from previous session.
      Window.opt.win_width := Width;
      Window.opt.win_height:= Height;
      -- Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  Current_MDI_Window : Natural := 0;

  procedure On_File_New (
    Window          : in out MDI_Main_Type;
    extra_first_doc : Boolean;
    New_Window      : in     MDI_Child_Access
  )
  is

    function Suffix return GWindows.GString is
    begin
      if Current_MDI_Window = 0 then
        return "";
      else
        return Integer'Wide_Image(Current_MDI_Window + 1);
      end if;
    end Suffix;

    File_Title: constant GString:= "New Archive" & Suffix;

  begin
    New_Window.Extra_first_doc:= extra_first_doc;
    Window.User_maximize_restore:= False;
    Create_MDI_Child (New_Window.all, Window, File_Title, Is_Dynamic => True);
    New_Window.Short_Name:= G2GU(File_Title);
    MDI_Active_Window (Window, New_Window.all);

    -- Transfer user-defined default options:
    -- New_Window.xxx.Opt:= Gen_Opt.Options_For_New;
    -- Refresh_size_dependent_parameters(
    --  New_Window.Draw_Control.Picture,
    --  objects => True
    -- );

    Current_MDI_Window := Current_MDI_Window + 1;

    -- This is just to set the MRUs in the new window's menu:
    Update_Common_Menus(Window);

    Finish_subwindow_opening(Window, New_Window.all);
  end On_File_New;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc: Boolean) is
    New_Window : constant MDI_Child_Access := new MDI_Child_Type;
  begin
    On_File_New(Window, extra_first_doc, New_Window);
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (Window : in out MDI_Main_Type) is
    File_Name, File_Title : GString_Unbounded;
    Success    : Boolean;
  begin
    Open_File (
      Window, "Open Zip archive",
      File_Name, Zip_archives_filters, ".zip", File_Title,
      Success
    );
    if Success then
      Open_Child_Window_And_Load( Window, File_Name, File_Title );
    end if;
  end On_File_Open;

  procedure On_File_Drop (Window     : in out MDI_Main_Type;
                          File_Names : in     Array_Of_File_Names) is
    New_Window : MDI_Child_Access;
    encrypt    : Boolean := False;
    yes        : Boolean;
    cancelled  : Boolean;
    use AZip_GWin.Password_dialogs, Zip;
  begin
    Window.Focus;
    if Window.Count_MDI_Children > 0 and then Window.opt.MDI_childen_maximized then
      --  If children windows are maximized, it is intuitive that files dropped (on the small
      --   areas like the window borders or the tool bar) are for the focused child window.
      declare
        cwc: constant Pointer_To_Base_Window_Class := Window.MDI_Active_Window;
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
          Open_Child_Window_And_Load (Window, File_Names(i));
        end loop;
      else
        Do_drop_file_dialog (
          Parent         => Window,
          archive_name   => "(A new Zip archive)",
          new_archive    => True,
          encrypt        => encrypt,
          yes            => yes
        );
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
      if Any_Window.all in MDI_Child_Type'Class and then
        Main_Window.Success_in_enumerated_close
      then  --  No [cancel] button was selected up to now.
        GWindows.Base.Close (Any_Window.all);
      end if;
    end My_Close_Win;
  begin
    Main_Window.Success_in_enumerated_close:= True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Main_Window).all,
                                      My_Close_Win'Unrestricted_Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_NEW_ARCHIVE =>
        On_File_New (Window, extra_first_doc => False);
      when IDM_OPEN_ARCHIVE =>
        On_File_Open (Window);
      when IDM_Quick_Help =>
        Quick_Help_Dialog (Window);
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
          On_Menu_Select (Window_Type (Window), Item);
        end if;
      when IDM_WINDOW_CASCADE   =>
        MDI_Cascade (Window);
      when IDM_WINDOW_TILE_HORIZONTAL =>
        MDI_Tile_Horizontal (Window);
      when IDM_WINDOW_TILE_VERTICAL =>
        MDI_Tile_Vertical (Window);
      when IDM_WINDOW_CLOSE_ALL =>
        My_MDI_Close_All(Window);
      when IDM_General_options =>
        On_General_Options (Window);
      when others =>
        for i_mru in Window.IDM_MRU'Range loop
          if Item = Window.IDM_MRU(i_mru) then
            Open_Child_Window_And_Load(
              Window,
              Window.opt.mru( i_mru )
            );
            exit;
          end if;
        end loop;
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  -------------

  procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        ) is
  begin
    Window.opt.MDI_main_maximized:= Zoom(Window);
    if not (Window.opt.MDI_main_maximized or Minimized(Window)) then
      Window.opt.win_left  := Left(Window);
      Window.opt.win_top   := Top(Window);
      Window.opt.win_width := Width(Window);
      Window.opt.win_height:= Height(Window);
    end if;

    -- TC.GWin.Options.Save;

    My_MDI_Close_All(Window);
    -- ^ Don't forget to save unsaved files !
    -- Operation can be cancelled by user for one unsaved picture.
    Can_Close:= Window.Success_in_enumerated_close;
    --
    if Can_Close then
      AZip_GWin.Persistence.Save (Window.opt);
      GWindows.Base.On_Exception_Handler (Handler => null);
      -- !! Trick to remove a strange crash on Destroy_Children
      -- !! on certain Windows platforms - 29-Jun-2012
    end if;
  end On_Close;

  -------------
  -- Add_MRU --
  -------------

  procedure Add_MRU (Window: in out MDI_Main_Type; name: GString) is
    x: Integer:= Window.opt.mru'First-1;
    up_name: GString:= name;
  begin
    To_Upper(up_name);

    -- Search for name in the list
    for m in Window.opt.mru'Range loop
      declare
        up_mru_m: GString:= GU2G(Window.opt.mru(m));
      begin
        To_Upper(up_mru_m);
        if up_mru_m = up_name then -- case insensitive comparison (Jan-2007)
          x:= m;
          exit;
        end if;
      end;
    end loop;

    -- name exists in list ?
    if x /= 0 then
      -- roll up entries after it, erasing it
      for i in x .. Window.opt.mru'Last-1 loop
        Window.opt.mru(i):= Window.opt.mru(i+1);
      end loop;
      Window.opt.mru(Window.opt.mru'Last):= Null_GString_Unbounded;
    end if;

    -- roll down the full list
    for i in reverse Window.opt.mru'First .. Window.opt.mru'Last-1 loop
      Window.opt.mru(i+1):= Window.opt.mru(i);
    end loop;

    -- name exists in list
    Window.opt.mru(Window.opt.mru'First):= G2GU(name);

  end Add_MRU;

  procedure Update_MRU_Menu(Window: in out MDI_Main_Type; m: in Menu_Type) is
  begin
    for i in reverse Window.opt.mru'Range loop
      Text(
        m, Command, Window.IDM_MRU(i),
         '&' &
         S2G(Ada.Strings.Fixed.Trim(Integer'Image(i),Ada.Strings.Left)) &
         ' ' &
         Shorten_file_name(GU2G(Window.opt.mru(i)))
      );
    end loop;
  end Update_MRU_Menu;

  procedure Update_Common_Menus_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      declare
        cw: MDI_Child_Type renames MDI_Child_Type(Window.all);
      begin
        Update_MRU_Menu (cw.MDI_Root.all, cw.Menu.Popup_0001);
        -- Update_Toolbar_Menu(cw.View_menu, cw.parent.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus(Window    : in out MDI_Main_Type;
                                top_entry : GString:= "" ) is
  begin
    if top_entry /= "" then
      Add_MRU(Window, top_entry);
    end if;
    Update_MRU_Menu(Window, Window.Menu.Popup_0001);
    -- Update_Toolbar_Menu(Window.View_menu, Window.Floating_toolbars);
    GWindows.Base.Enumerate_Children(
      MDI_Client_Window (Window).all,
      Update_Common_Menus_Child'Access
    );
  end Update_Common_Menus;

  function All_Zip_files(File_Names: Array_Of_File_Names) return Boolean is
    all_valid, empty: Boolean:= True;
  begin
    for i in File_Names'Range loop
      empty:= False;
      all_valid:= all_valid and
        Is_valid_Zip_archive(To_UTF_8(GU2G(File_Names(i)))) = valid;
    end loop;
    return all_valid and not empty;
  end All_Zip_files;

  function Confirm_archives_if_all_Zip_files(
    Window : GWindows.Base.Base_Window_Type'Class;
    File_Names : Array_Of_File_Names
  )
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
