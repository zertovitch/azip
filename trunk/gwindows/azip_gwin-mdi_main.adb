with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with Zip;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Static_Controls.Web;      use GWindows.Static_Controls.Web;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Compiler_Version;

package body AZip_GWin.MDI_Main is

  function S2G (Value : String) return GString renames To_GString_From_String;

  procedure Focus_an_already_opened_window(
    Window    : MDI_Main_Type;
    File_Name : GString_Unbounded;
    is_open   : out Boolean )
  is
    use type GString_Unbounded;
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
    GWindows.Base.Enumerate_Children(
      MDI_Client_Window (Window).all,
      Identify'Unrestricted_Access
    );
  end Focus_an_already_opened_window;

  procedure Redraw_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      -- !! some content refresh, dbl buffering
      GWindows.Base.Redraw(Window.all);
    end if;
  end Redraw_Child;

  procedure Redraw_all(Window: in out MDI_Main_Type) is
  begin
    Redraw(Window);
    -- Redraw(Window.Tool_bar);
    GWindows.Base.Enumerate_Children(MDI_Client_Window (Window).all,Redraw_child'Access);
  end Redraw_all;

  procedure Close_extra_first_child(Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      declare
        w: MDI_Child_Type renames MDI_Child_Type(Window.all);
      begin
        if w.Extra_first_doc and Is_file_saved(w) then
          Close(Window.all);
        end if;
      end;
    end if;
  end Close_extra_first_child;

  procedure Close_extra_first_child(Window: in out MDI_Main_Type) is
  begin
    GWindows.Base.Enumerate_Children(MDI_Client_Window (Window).all,Close_extra_first_child'Access);
  end Close_extra_first_child;

  procedure Finish_subwindow_opening(
    m : in out MDI_Main_Type;
    c : in out MDI_Child_Type )
  is
  begin
    m.user_maximize_restore:= True;
    if m.MDI_childen_maximized then
      Zoom(c);
      Redraw_all(m);
    end if;
    -- Show things in the main status bar - effective only after Thaw!
  end Finish_subwindow_opening;

  procedure Open_Child_Window_And_Load (
        Window     : in out MDI_Main_Type;
        File_Name,
        File_Title :        Gwindows.Gstring_Unbounded ) is
    -- Candidate              : TC.Archive;
    is_open: Boolean;
  begin
    Focus_an_already_opened_window( Window, File_Name, is_open );
    if is_open then
      return;        -- nothing to do, archive already in a window
    end if;
    -- TC.Input.Load( Candidate, False, To_GString_From_Unbounded (File_Name));
    declare
      New_Window : constant MDI_Child_Access := new MDI_Child_Type;
    begin
      -- We do here like Excel or Word: close the unused blank window
      Close_extra_first_child(Window);
      --
      Window.user_maximize_restore:= False;
      New_Window.File_Name:= File_Name;
      Create_MDI_Child (New_Window.all,
        Window,
        To_GString_from_Unbounded(File_Title),
        Is_Dynamic => True
      );
      New_Window.Short_Name:= File_Title;
      MDI_Active_Window (Window, New_Window.all);
      Update_Common_Menus(Window, To_GString_from_Unbounded(New_Window.File_Name));
      Reload_archive(New_Window.all);
      Finish_subwindow_opening(Window, New_Window.all);
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

  procedure Open_Child_Window_And_Load (
        Window     : in out MDI_Main_Type;
        File_Name  :        Gwindows.Gstring_Unbounded ) is
  begin
    Open_Child_Window_And_Load(
      Window,
      File_Name,
      File_Name   -- file name used as title (could be nicer)
    );
  end Open_Child_Window_And_Load;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create ( Window : in out MDI_Main_Type ) is
    use Ada.Command_Line;
  begin
    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");

    -- ** Menus and accelerators:

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 2);
    Window.IDM_MRU:=
      (IDM_MRU_1,
       IDM_MRU_2,
       IDM_MRU_3,
       IDM_MRU_4,
       IDM_MRU_5,
       IDM_MRU_6,
       IDM_MRU_7,
       IDM_MRU_8,
       IDM_MRU_9);

    Accelerator_Table (Window, "Main_Menu");

    Dock_Children(Window);
    Show (Window);

    if Argument_count=0 then
      On_File_New (Window, extra_first_doc => True);
      -- ^ The MS Office-like first, empty document
    end if;
    -- !! This works on 1st instance only:
    for I in 1..Argument_Count loop
      Open_Child_Window_And_Load(
        Window,
        To_Gstring_Unbounded(To_GString_from_String(Argument(I)))
      );
    end loop;
    Accept_File_Drag_And_Drop(Window);

  end On_Create;

  -----------------
  -- On_File_New --
  -----------------

  Current_MDI_Window : Natural := 0;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc: Boolean)
  is
    New_Window : constant MDI_Child_Access := new MDI_Child_Type;

    function Suffix return GWindows.Gstring is
    begin
      if Current_MDI_Window = 0 then
        return "";
      else
        return To_GString_From_String(Current_MDI_Window'Img);
      end if;
    end Suffix;

    File_Title: constant GString:= "New Archive" & Suffix;

  begin
    New_Window.extra_first_doc:= extra_first_doc;
    Window.user_maximize_restore:= False;
    Create_MDI_Child (New_Window.all, Window, File_Title, Is_Dynamic => True);
    New_Window.Short_Name:= To_GString_Unbounded(File_Title);
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

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (
        Window : in out MDI_Main_Type ) is
    File_Name, File_Title : GString_Unbounded;
    Success    : Boolean;
  begin
    Open_File (Window, "Open",
      File_Name,
      ((To_GString_Unbounded ("Zip archive (*.zip)"),
          To_GString_Unbounded ("*.zip" )),
        (To_GString_Unbounded ("All files (*.*)"),
          To_GString_Unbounded ("*.*"))),
      ".zip",
      File_Title,
      Success);

    if Success then
      Open_Child_Window_And_Load( Window, File_Name, File_Title );
    end if;
  end On_File_Open;

  ----------------------
  -- My_MDI_Close_All --
  ----------------------

  procedure My_MDI_Close_All (Main_Window : in out MDI_Main_Type) is
    procedure My_Close_Win (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    --  Enumeration call back to close MDI child windows
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class and then
        Main_Window.Success_In_Enumerated_Close then -- no [cancel] up to now
        GWindows.Base.Close (Any_Window.all);
      end if;
    end My_Close_Win;
  begin
    Main_Window.Success_In_Enumerated_Close:= True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Main_Window).all,
                                      My_Close_Win'Unrestricted_Access);
  end My_MDI_Close_All;

  procedure On_About(Window : in out MDI_Main_Type) is
    box: About_Box_Type;
    url_azip, url_gnat, url_gnavi, url_resedit, url_zipada: URL_Type;
    package CVer is new GNAT.Compiler_Version;
  begin
    box.Create_Full_Dialog(Window);
    Create_and_Swap(url_azip, box.AZip_URL, box, "http://sf.net/projects/azip");
    Create_and_Swap(url_gnat, box.GNAT_URL, box, "http://libre.adacore.com");
    Text(box.GNAT_Version, S2G("version " & CVer.Version));
    Create_and_Swap(url_gnavi, box.GNAVI_URL, box, "http://sf.net/projects/gnavi");
    Create_and_Swap(url_resedit, box.ResEdit_URL, box, "http://resedit.net");
    Create_and_Swap(url_zipada, box.ZipAda_URL, box, S2G(Zip.web));
    Text(box.ZipAda_Version, S2G("version " & Zip.version & ", ref. " & Zip.reference));
    box.Center;
    if Show_Dialog (box, Window) = IDOK then
      null;
    end if;
  end On_About;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_NEW_FILE =>
        On_File_New (Window, extra_first_doc => False);
      when IDM_OPEN_FILE =>
        On_File_Open (Window);
      when IDM_ABOUT =>
        On_About (Window);
      when IDM_QUIT  =>
        Close (Window);
      when IDM_Window_Cascade   =>
        MDI_Cascade (Window);
      when IDM_Window_Tile_Horizontal =>
        MDI_Tile_Horizontal (Window);
      when IDM_Window_Tile_Vertical =>
        MDI_Tile_Vertical (Window);
      when IDM_Window_Close_All =>
        My_MDI_Close_All(Window);
      when others =>
        for i_mru in Window.IDM_MRU'Range loop
          if Item = Window.IDM_MRU(i_mru) then
            Open_Child_Window_And_Load(
              Window,
              Window.mru( i_mru )
            );
            exit;
          end if;
        end loop;
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  -------------

  function Minimized(Window: GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left(Window) <= -32000;
  end Minimized;

  procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        ) is
  begin
    Window.MDI_main_maximized:= Zoom(Window);
    if not (Window.MDI_main_maximized or Minimized(Window)) then
      Window.Memorized_Left  := Left(Window);
      Window.Memorized_Top  := Top(Window);
      Window.Memorized_Width := Width(Window);
      Window.Memorized_Height:= Height(Window);
    end if;

    -- TC.GWin.Options.Save;

    My_MDI_Close_All(Window);
    -- ^ Don't forget to save unsaved files !
    -- Operation can be cancelled by user for one unsaved picture.
    Can_Close:= Window.success_in_enumerated_close;
    --
    if Can_Close then
      GWindows.Base.On_Exception_Handler (Handler => null);
      -- !! Trick to remove a strange crash on Destroy_Children
      -- !! on certain Windows platforms - 29-Jun-2012
    end if;
  end On_Close;

  -------------
  -- Add_MRU --
  -------------

  procedure Add_MRU (Window: in out MDI_Main_Type; name: GString) is
    x: Integer:= Window.mru'First-1;
    up_name: GString:= name;
  begin
    To_Upper(up_name);

    -- Search for name in the list
    for m in Window.mru'Range loop
      declare
        up_mru_m: GString:= To_GString_From_Unbounded(Window.mru(m));
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
      for i in x .. Window.mru'Last-1 loop
        Window.mru(i):= Window.mru(i+1);
      end loop;
      Window.mru(Window.mru'Last):= To_GString_Unbounded("");
    end if;

    -- roll down the full list
    for i in reverse Window.mru'First .. Window.mru'Last-1 loop
      Window.mru(i+1):= Window.mru(i);
    end loop;

    -- name exists in list
    Window.mru(Window.mru'First):= To_GString_Unbounded(name);

  end Add_MRU;

  function Shorten_filename( s: GString ) return GString is
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
  end Shorten_filename;

  procedure Update_MRU_Menu(Window: in out MDI_Main_Type; m: in Menu_type) is
  begin
    for i in reverse Window.mru'Range loop
      Text(
        m, command, Window.IDM_MRU(i),
         '&' &
         To_GString_from_String(Ada.Strings.Fixed.Trim(Integer'Image(i),Ada.Strings.Left)) &
         ' ' &
         Shorten_filename(To_GString_from_Unbounded(Window.mru(i)))
      );
      --State(m,command,cmd,Disabled);
    end loop;
  end Update_MRU_Menu;

  procedure Update_Common_Menus_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Child_Type'Class then
      declare
        cw: MDI_Child_Type renames MDI_Child_Type(Window.all);
      begin
        Update_MRU_Menu(cw.parent.all, cw.Menu.Popup_0001);
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

end AZip_GWin.MDI_Main;
