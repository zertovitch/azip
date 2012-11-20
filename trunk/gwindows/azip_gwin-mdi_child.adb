with AZip_Common;                       use AZip_Common;

with Zip;                               use Zip;
with Zip_Streams;
with Time_Display;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Types;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;
with Ada_Directories_Extensions;
with Ada.Environment_Variables;         use Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Numerics.Float_Random;
-- with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Interfaces;

package body AZip_GWin.MDI_Child is

  function S2G (Value : String) return GString renames To_GString_From_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

  procedure Update_display(
    Window : in out MDI_Child_Type;
    need   :        Update_need
  )
  is

    procedure Feed_directory_list(prefix_path: String) is
      row: Natural:= 0;
      Lst: MDI_Child_List_View_Control_Type renames Window.Directory_List;
      --
      procedure Insert_row(
        name             : String; -- 'name' is compressed entry's name
        file_index       : Positive;
        comp_size        : File_size_type;
        uncomp_size      : File_size_type;
        crc_32           : Interfaces.Unsigned_32;
        date_time        : Time;
        method           : PKZip_method;
        unicode_file_name: Boolean;
        read_only        : Boolean
      )
      is
        pragma Unreferenced (file_index);
        simple_name_idx: Positive:= name'First;
        extension_idx: Positive:= name'Last + 1;
        R_mark: constant array (Boolean) of Character:= (' ', 'R');
      begin
        for i in name'Range loop
          case name(i) is
            when '/' | '\' =>
              -- Directory separator, ok with Unicode UTF-8 names
              simple_name_idx:= i + 1;
            when '.' =>
              -- Last dot is the extension separator
              extension_idx:= i + 1;
            when others =>
              null;
          end case;
        end loop;
        if simple_name_idx <= name'Last then -- skip directory entries
          Lst.Insert_Item(S2G(name(simple_name_idx..name'Last)), row);
          Lst.Set_Sub_Item(S2G(name(extension_idx..name'Last)), row, 1);
          begin
            Lst.Set_Sub_Item(S2G(Time_Display(Convert(date_time))), row, 2);
          exception
            when Zip_Streams.Calendar.Time_Error =>
              Lst.Set_Sub_Item("(invalid)", row, 2);
          end;
          Lst.Set_Sub_Item(S2G((1 => R_mark(read_only))), row, 3);
          Lst.Set_Sub_Item(S2G(Pretty_file_size(uncomp_size)), row, 4);
          Lst.Set_Sub_Item(S2G(Pretty_file_size(comp_size)), row, 5);
          Lst.Set_Sub_Item(S2G(Ratio_pct(comp_size, uncomp_size)), row, 6);
          Lst.Set_Sub_Item(S2G(To_Lower(PKZip_method'Image(method))), row, 7);
          Lst.Set_Sub_Item(S2G(Hexadecimal(crc_32)), row, 8);
          Lst.Set_Sub_Item(S2G(name(name'First..simple_name_idx - 1)), row, 9);
          row:= row + 1; -- more subtle with our sorting
        end if;
      end Insert_row;

      procedure Traverse is new Zip.Traverse_verbose(Insert_row);

    begin
      for topic in Entry_topic loop
        case need is
          when first_display =>
            Lst.Clear;
            Lst.Insert_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.current_options.column_width(topic)
            );
          when archive_changed =>
            Lst.Clear;
            Lst.Set_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.current_options.column_width(topic)
            );
          when simple_refresh =>
            null;
        end case;
      end loop;
      if Is_Loaded(Window.zif) and then need <= archive_changed then
        Traverse(Window.zif);
      end if;
    end Feed_directory_list;

    sel: Natural;

  begin
    case Window.current_options.view_mode is
      when Flat =>
        Window.Folder_Tree.Hide;
        Feed_directory_list("");
        Check(Window.Menu.Main, Command, IDM_FLAT_VIEW, True);
        Check(Window.Menu.Main, Command, IDM_TREE_VIEW, False);
      when Tree =>
        -- all stuff
        -- Feed_directory_list([selected path]);
        Window.Folder_Tree.Show;
        Check(Window.Menu.Main, Command, IDM_FLAT_VIEW, False);
        Check(Window.Menu.Main, Command, IDM_TREE_VIEW, True);
    end case;
    if Is_Loaded(Window.zif) then
      sel:= Window.Directory_List.Selected_Item_Count;
      if sel > 0 then
        Text( Window.Status_Bar,
          S2G(Integer'Image(Entries(Window.zif)) &
            " files," & Integer'Image(sel) & " selected"), 0
         );
      else
        Text( Window.Status_Bar,
          S2G(Integer'Image(Entries(Window.zif)) &
            " files, none selected"), 0
         );
      end if;
    else
      Text(Window.Status_Bar,"No archive loaded",0);
    end if;
  end Update_display;

  procedure On_Item_Changed (Control : in out MDI_Child_List_View_Control_Type) is
    PW: MDI_Child_Type renames MDI_Child_Type(Control.Parent.all);
  begin
    Update_display(PW, simple_refresh);
  end On_Item_Changed;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
  begin
    Small_Icon (Window, "Picture_Icon");

    -- Filial feelings:
    Window.parent:= MDI_Main_Access(Controlling_Parent(Window));

    Create(Window.Directory_List, Window, 50,1,20,20, Multiple, Report_View);
    Create(Window.Folder_Tree, Window, 1,1,20,20);

    Create(Window.Status_Bar, Window, "No archive");
    Parts(Window.Status_Bar, (200, -1));
    Dock (Window.Status_Bar, GWindows.Base.At_Bottom);

    -- Window.Draw_Control.parent:=
    --   MDI_Picture_Child_Access(Controlling_Parent(Window.Draw_Control));

    -- Background_Mode (Window.Draw_Control.Drawing_Area, Transparent);
    -- Background_Mode (Window.Draw_Control.Saved_Area, Transparent);

    -- On_Change_Cursor_Handler (Window.Draw_Control, Do_Change_Cursor'Access);
    -- On_Left_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Left_Mouse_Down'Access);
    -- On_Right_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Right_Mouse_Down'Access);
    -- On_Left_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
    -- On_Right_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
    -- On_Mouse_Move_Handler (Window.Draw_Control, Do_Mouse_Move'Access);
    --
    -- On_Character_Down_Handler (Window, Do_Key_Down'Access); -- 14-Oct-2005

    -- Create_Compatible_Bitmap (Window.Draw_Control.Drawing_Area,
    --                           Window.Draw_Control.Saved_Bitmap,
    --                           Desktop_Width,
    --                           Desktop_Height);
    -- Select_Object (Window.Draw_Control.Saved_Area,
    --                Window.Draw_Control.Saved_Bitmap);

    --Refresh_size_dependent_parameters(
    --  Window.Draw_Control.Picture,
    --  objects => True
    --);
    --Subtle_redraw (Window.Draw_Control);

    Dock_Children (Window);

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 5);

    -- Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:= not Window.parent.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Window.Parent.Freeze;
        Window.Zoom;
      end if;
      On_Size(Window,Width(Window),Height(window));
      if memo_unmaximized_children then
        Window.Parent.Thaw; -- Before Zoom, otherwise uncomplete draw.
        Window.Zoom(False);
        -- Window.parent.Tool_Bar.Redraw;
      end if;
    end;

    --Scroll_Position(Window, Vertical, Scroll_Maximum(Window, Vertical));
    --Adjust_Draw_Control_Position(Window);

    Window.Status_deamon.Start;
    Update_display(Window, first_display);
    Window.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset(Window.Temp_name_gen);
  end On_Create;

  procedure On_Save (Window : in out MDI_Child_Type) is
  begin
    null; -- nothing to be saved in this application
  end On_Save;

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean is
    pragma Unreferenced (Window);
  begin
    return True;
  end Is_file_saved;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Child_Type)
  is
     New_File_Name : GWindows.GString_Unbounded;
     File_Title    : GWindows.GString_Unbounded;
     Success       : Boolean;
  begin
    New_File_Name := Window.File_Name;
    Save_File (
      Window,
      "Save as...",
      New_File_Name,
      ((To_GString_Unbounded ("Zip archive (*.zip)"),
        To_GString_Unbounded ("*.zip" )),
       (To_GString_Unbounded ("All files (*.*)"),
        To_GString_Unbounded ("*.*"))),
      ".zip",
      File_Title,
      Success
    );
    if not Success then
      return;
    end if;
    if Zip.Exists(GWindows.GStrings.To_String (
      To_GString_From_Unbounded (New_File_Name))
    ) -- !! conv.
    then
      if Message_Box (
        Window,
        "Save as",
        "The file " & To_GString_From_Unbounded (New_File_Name) &
        " already exists. Replace ?",
        Yes_No_Box,
        Question_Icon
      ) = No
      then
        return;
      end if;
    end if;

    begin
      Ada.Directories.Copy_File(
        GWindows.GStrings.To_String (
          To_GString_From_Unbounded (Window.File_Name)),
        GWindows.GStrings.To_String (
          To_GString_From_Unbounded (New_File_Name))
      );
    exception
      when others =>
        Message_Box(
          Window,
          "'Save as' failed",
          "Copy of archive under new name failed.",
          OK_Box,
          Exclamation_Icon
        );
        return;
    end;
    Window.File_Name := New_File_Name;
    Text (Window, To_GString_From_Unbounded (File_Title));
    Window.Short_Name:= File_Title;
    Update_Common_Menus(Window,To_GString_from_Unbounded(New_File_Name));
    Window.Load_archive_catalogue;
  end On_Save_As;

  procedure Process_archive_GWin(
    Window         : in out MDI_Child_Type;
    operation      : Archive_Operation;
    file_names     : Array_Of_File_Names;
    name_match     : Name_matching_mode;
    base_folder    : String;
    search_pattern : GString;
    output_folder  : String;
    new_temp_name  : String
  )
  is
    az_names: Name_list(File_Names'Range);
    box: Progress_Box_Type;
    --
    procedure Boxed_Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : String;
      is_UTF_8              : Boolean;
      operation             : Entry_Operation
    )
    is
    begin
      box.File_Progress.Position(file_percents_done);
      box.Archive_Progress.Position(archive_percents_done);
      box.Entry_name.Text(S2G(entry_being_processed)); -- !! UTF-8
      case operation is
        when Append =>
          box.Entry_operation_name.Text("Appending...");
        when Replace =>
          box.Entry_operation_name.Text("Replacing...");
        when Copy =>
          box.Entry_operation_name.Text("Copying...");
        when Skip =>
          box.Entry_operation_name.Text("Skipping...");
        when Test =>
          box.Entry_operation_name.Text("Testing...");
        when Extract =>
          box.Entry_operation_name.Text("Extracting...");
        when Search =>
          box.Entry_operation_name.Text("Searching...");
      end case;
      Message_Check;
    end Boxed_Feedback;
    --
    procedure Archive_processing is new Process_archive(Boxed_Feedback);
    --
  begin
    -- Convert GStrings (UTF-16) to Strings with UTF-8
    for i in az_names'Range loop
      az_names(i):=
        (name => To_Unbounded_String(
                   GWindows.GStrings.To_String(GU2G(File_Names(i)))
                 ), -- !!
         utf_8 => False, -- !!
         match => 0
        );
    end loop;
    box.Create_Full_Dialog(Window);
    box.File_Progress.Position(0);
    box.Archive_Progress.Position(0);
    box.Cancel_button.Hide;
    box.Cancel_button_permanent.Show;
    box.Cancel_button_permanent.Disable; -- !!
    box.Center;
    box.Show;
    Window.Parent.Disable;
    begin
      Archive_processing(
        zif            => Window.zif,
        operation      => operation,
        entry_name     => az_names,
        name_match     => name_match,
        base_folder    => base_folder,
        search_pattern => search_pattern,
        output_folder  => output_folder,
        Set_Time_Stamp => Ada_Directories_Extensions.Set_Modification_Time'Access,
        new_temp_name  => new_temp_name
      );
      -- !! after processing we should do something with the counts -> Results col.
      if operation in Modifying_Operation then
        Window.Load_archive_catalogue;
      end if;
    exception
      when Ada.IO_Exceptions.Use_Error =>
        Message_Box(
          Window,
          "Processing failed",
          "Archive cannot be modified - probably read-only",
          OK_Box,
          Exclamation_Icon
        );
    end;
    Window.Parent.Enable;
  end Process_archive_GWin;

  function Temp_AZip_name(Window: MDI_Child_Type) return String is
  begin
    loop
      declare
        num0: constant String:=
          Float'Image(Ada.Numerics.Float_Random.Random(Window.Temp_name_gen));
        num: constant String:= num0(num0'First+1 .. num0'Last);
        -- ^ Skip the @#*% leading space
        test_name: constant String:= Value("TEMP") & "\AZip_Temp_" & num & ".zip";
      begin
        if not Zip.Exists(test_name) then
          return test_name;
        end if;
      end;
    end loop;
  end Temp_AZip_name;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names) is
  begin
    if Confirm_archives_if_all_Zip_files(Window, File_Names) then
      for i in File_Names'Range loop
        Open_Child_Window_And_Load(
          Window.Parent.all,
          File_Names(i)
        );
      end loop;
    elsif Is_Loaded(Window.zif) then
      if Message_Box(
        Window,
        "Files dropped",
        "Add dropped files to archive """ & GU2G(Window.Short_Name) & """ ?",
        Yes_No_Box,
        Question_Icon) = Yes
      then
        Process_archive_GWin(
          Window         => Window,
          operation      => Add,
          file_names     => File_Names,
          name_match     => Exact,
          base_folder    => "",           -- !! only for flat view
          search_pattern => "",
          output_folder  => "",
          new_temp_name  => Temp_AZip_name(Window)
        );
      end if;
    else
      if Message_Box(
        Window,
        "Files dropped",
        "Add dropped files to new archive (" & GU2G(Window.Short_Name) & ") ?",
        Yes_No_Box,
        Question_Icon) = Yes
      then
        null; -- !!
      end if;
    end if;
  end On_File_Drop;

  -- This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" ) is
  begin
    Update_Common_Menus( Window.parent.all, top_entry );
  end Update_Common_Menus;

  procedure Load_archive_catalogue (Window : in out MDI_Child_Type) is
  begin
    if Zip.Is_loaded(Window.zif) then
      Zip.Delete(Window.zif);
    end if;
    Zip.Load(Window.zif, GWindows.GStrings.To_String(To_GString_From_Unbounded(Window.File_Name)));
    Update_display(Window, archive_changed);
    -- Window.Status_deamon.Display(Window'Unchecked_Access);
  end Load_archive_catalogue;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);   -- only client area is considered
    pragma Warnings (Off, Height);  -- only client area is considered
    w: constant Natural:= Window.Client_Area_Width;
    h: constant Natural:= Window.Client_Area_Height - Window.Status_Bar.Height;
  begin
    case Window.current_options.view_mode is
      when Flat =>
        Window.Directory_List.Location(
            GWindows.Types.Rectangle_Type'
            (0, 0, w, h)
        );
      when Tree =>
        Window.Folder_Tree.Location(
            GWindows.Types.Rectangle_Type'
            (0, 0, w/2, h)
        );
        Window.Directory_List.Location(
            GWindows.Types.Rectangle_Type'
            (w/2+1, 0, w, h)
        );
        null;
    end case;
    Dock_Children (Window);
  end On_Size;

  function Get_selected_entry_list(Window: MDI_Child_Type)
  return Array_Of_File_Names
  is
    items: constant Natural:= Window.Directory_List.Item_Count;
    names: Array_Of_File_Names(1..items);
    j: Natural:= 0;
  begin
    for i in 0..items - 1 loop -- 0-based
      if Window.Directory_List.Is_Selected(i) then
        j:= j + 1;
        names(j):= G2GU(
          Window.Directory_List.Text(i, subitem => 9) & -- path
          Window.Directory_List.Text(i, subitem => 0)   -- simple name
          );
      end if;
    end loop;
    return names(1..j);
  end Get_selected_entry_list;

  procedure On_Extract(Window : in out MDI_Child_Type) is
    dir: constant GString:= Get_Directory(Window, "Extract to...");
    sdir: constant String:= GWindows.GStrings.To_String(dir); -- !! lazy conversion
  begin
    if dir /= "" then
      Process_archive_GWin(
        Window         => Window,
        operation      => Extract,
        file_names     => Get_selected_entry_list(Window),
        name_match     => Exact,
        base_folder    => "",
        search_pattern => "",
        output_folder  => sdir,
        new_temp_name  => ""
      );
    end if;
  end On_Extract;

  procedure On_Delete(Window : in out MDI_Child_Type) is
  begin
    if Window.Directory_List.Selected_Item_Count = 0 then
      return;
    end if;
    if Message_Box(
        Window,
        "Delete",
        "Do you want to remove the selected item(s) from the archive ?",
        Yes_No_Box,
        Question_Icon)
      = Yes
    then
      Process_archive_GWin(
        Window         => Window,
        operation      => Remove,
        file_names     => Get_selected_entry_list(Window),
        name_match     => Exact,
        base_folder    => "",
        search_pattern => "",
        output_folder  => "",
        new_temp_name  => Temp_AZip_name(Window)
      );
    end if;
  end On_Delete;

  procedure On_Add_files(Window : in out MDI_Child_Type) is
    Success: Boolean;
    File_Name, File_Title : GString_Unbounded;
  begin
    Open_File (Window, "Add files to archive...",
      File_Name,
       ( 1=>(To_GString_Unbounded ("All files (*.*)"),
             To_GString_Unbounded ("*.*"))),
      "",
      File_Title,
      Success);
    if Success then
      Process_archive_GWin(
        Window         => Window,
        operation      => Add,
        file_names     => (1 => File_Name), -- !! ok for 1 only
        name_match     => Exact,
        base_folder    => "",
        search_pattern => "",
        output_folder  => "",
        new_temp_name  => Temp_AZip_name(Window)
      );
    end if;
  end On_Add_files;

  procedure On_Find(Window : in out MDI_Child_Type) is
    box: Find_box_Type;
    --
    procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      Window.Name_search:= G2GU(box.Name_to_be_searched.Text);
      Window.Content_search:= G2GU(box.Content_to_be_searched.Text);
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog(Window);
    box.Name_to_be_searched.Text(GU2G(Window.Name_search));
    box.Content_to_be_searched.Text(GU2G(Window.Content_search));
    box.Center;
    box.On_Destroy_Handler(Get_Data'Unrestricted_Access);
    if Show_Dialog (box, Window) = IDOK then
      Process_archive_GWin(
        Window         => Window,
        operation      => Search,
        file_names     => (1 => Window.Name_search),
        name_match     => Substring,
        base_folder    => "",
        search_pattern => GU2G(Window.Content_search),
        output_folder  => "",
        new_temp_name  => ""
      );
    end if;
  end On_Find;

  procedure Full_Select(Window: in out MDI_Child_Type; as: Boolean) is
  begin
    for i in 1..Window.Directory_List.Item_Count loop
      Window.Directory_List.Selected(i-1, as); -- Item seems 0-based...
    end loop;
    Window.Directory_List.Focus;
  end Full_Select;

  procedure On_Test(Window : in out MDI_Child_Type) is
  begin
    Process_archive_GWin(
      Window         => Window,
      operation      => Test,
      file_names     => Empty_Array_Of_File_Names,
      name_match     => Exact,
      base_folder    => "",
      search_pattern => "",
      output_folder  => "",
      new_temp_name  => ""
    );
    -- !! Message_Box: say something if failure
    -- !! Display results (0 -> OK -> green; 1 -> KO -> red)
  end On_Test;

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_SAVE_ARCHIVE_AS =>
        Window.On_Save_As;
      when IDM_CLOSE_ARCHIVE =>
        Window.Close;
      when IDM_Select_all =>
        Full_Select(Window, True);
      when IDM_Unselect_all =>
        Full_Select(Window, False);
      when IDM_EXTRACT =>
        On_Extract(Window);
      when IDM_ADD_FILES =>
        On_Add_files(Window);
      when IDM_Delete_selected =>
        On_Delete(Window);
      when IDM_FIND_IN_ARCHIVE =>
        On_Find(Window);
      when IDM_TEST_ARCHIVE =>
        On_Test(Window);
      when others =>
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean) is
  begin
    Can_close:= True;
    if Is_file_saved(Window) then
      Update_Common_Menus(Window,To_GString_from_Unbounded(Window.File_Name));
      Window.Status_deamon.Stop;
    else -- This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                To_GString_from_Unbounded(Window.Short_Name) & "' ?",
                Yes_No_Cancel_Box,
                Question_Icon)
        is
          when Yes    => On_Save(Window);
                         exit when Is_file_saved(Window);
          when No     => exit;
          when Cancel => Window.parent.Success_In_Enumerated_Close:= False;
                         Can_close:= False;
                         exit;
          when others => null;
        end case;
      end loop;
    end if;
  end On_Close;

  package body Daemons is
    ---------------------------------------------------------------------------
    -- This background task displays on demand (also from another task)      --
    -- informations about archive and various status.                        --
    ---------------------------------------------------------------------------

    task body Status_display is
      current_child_window: AZip_GWin.MDI_Child.MDI_Child_Access;
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Display(w: AZip_GWin.MDI_Child.MDI_Child_Access) do
            current_child_window:= w;
          end Display;
          Update_display(current_child_window.all, simple_refresh);
        or
          delay 0.05; -- relax
        end select;
      end loop;
    end Status_display;

    -----------------------------------------------------------------
    -- This background task calls an archive test on demand        --
    -- and ensures display of its progress.                        --
    -----------------------------------------------------------------

    task body Testing_type is
      current_child_window: AZip_GWin.MDI_Child.MDI_Child_Access;
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Test(w: AZip_GWin.MDI_Child.MDI_Child_Access) do
            current_child_window:= w;
          end Test;
          -- (perform test)
        or
          delay 0.05; -- relax
        end select;
      end loop;
    end Testing_type;

  end Daemons;

end AZip_GWin.MDI_Child;
