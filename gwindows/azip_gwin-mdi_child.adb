with AZip_Common;                       use AZip_Common;

with Zip;                               use Zip;
with Zip_Streams;
with UnZip;
with Time_Display;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Calendar;
with Ada.Directories;
with Ada_Directories_Extensions;
with Ada.Environment_Variables;         use Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Sequential_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;

with Interfaces;
with GWindows.Types;

package body AZip_GWin.MDI_Child is

  procedure Update_display(
    Window : in out MDI_Child_Type;
    need   :        Update_need
  )
  is

    procedure Feed_directory_list(prefix_path: String) is
      row: Natural:= 0;
      Lst: MDI_Child_List_View_Control_Type renames Window.Directory_List;
      --
      procedure Process_row(
        name             : String; -- 'name' is compressed entry's name
        file_index       : Positive;
        comp_size        : File_size_type;
        uncomp_size      : File_size_type;
        crc_32           : Interfaces.Unsigned_32;
        date_time        : Time;
        method           : PKZip_method;
        name_encoding    : Zip_name_encoding;
        read_only        : Boolean;
        user_code        : in out Integer
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
        if simple_name_idx > name'Last then -- skip directory entries
          return;
        end if;
        if name'Length < prefix_path'Length or else
          prefix_path /=  name(name'First..name'First+prefix_path'Length-1)
        then -- not in a part of the tree to be displayed
          return;
        end if;
        if need in first_display .. archive_changed then
          Lst.Insert_Item(To_UTF_16(name(simple_name_idx..name'Last), name_encoding), row);
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
          Lst.Set_Sub_Item(To_Lower(PKZip_method'Wide_Image(method)), row, 7);
          Lst.Set_Sub_Item(S2G(Hexadecimal(crc_32)), row, 8);
          Lst.Set_Sub_Item(To_UTF_16(name(name'First..simple_name_idx - 1), name_encoding), row, 9);
          Lst.Set_Sub_Item(Zip_name_encoding'Wide_Image(name_encoding), row, 10);
        end if;
        Lst.Set_Sub_Item(S2G(Result_message(Window.last_operation, user_code)), row, 11);
        row:= row + 1; -- more subtle with our sorting
      end Process_row;

      procedure Traverse is new Zip.Traverse_verbose(Process_row);

    begin
      for topic in Entry_topic loop
        case need is
          when first_display =>
            Lst.Clear;
            Lst.Insert_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.Parent.opt.column_width(topic)
            );
          when archive_changed =>
            Lst.Clear;
            Lst.Set_Column(
              S2G(Image(topic)),
              Entry_topic'Pos(topic),
              Window.Parent.opt.column_width(topic)
            );
          when results_refresh | simple_refresh =>
            null;
        end case;
      end loop;
      if Is_Loaded(Window.zif) and then need <= results_refresh then
        Traverse(Window.zif);
      end if;
    end Feed_directory_list;

    sel: Natural;

  begin
    case Window.opt.view_mode is
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
          Integer'Wide_Image(Window.Directory_List.Item_Count) &
            " files," & Integer'Wide_Image(sel) & " selected", 0
         );
      else
        Text( Window.Status_Bar,
          Integer'Wide_Image(Window.Directory_List.Item_Count) &
            " files, none selected", 0
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
    Small_Icon (Window, "AZip_Doc_Icon_Name");

    -- Filial feelings:
    Window.parent:= MDI_Main_Access(Controlling_Parent(Window));
    -- Copy options to child level:
    Window.opt:= Window.Parent.opt;

    Create(Window.Directory_List, Window, 50,1,20,20, Multiple, Report_View);
    Create(Window.Folder_Tree, Window, 1,1,20,20);

    Create(Window.Status_Bar, Window, "No archive");
    Parts(Window.Status_Bar, (1 => 200, 2 => -1));
    Dock (Window.Status_Bar, GWindows.Base.At_Bottom);

    Dock_Children (Window);

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 5);

    -- Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:=
        not Window.parent.opt.MDI_childen_maximized;
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
    --
    -- If needed, an empty Zip file is created with the contents below.
    --
    package CIO is new Ada.Sequential_IO(Character);
    use CIO;
    empty_zip: File_Type;
    -- This is the empty Zip archive:
    contents: constant String:= "PK" & ASCII.ENQ & ASCII.ACK & 18 * ASCII.NUL;
  begin
    New_File_Name := Window.File_Name;
    Save_File (
      Window, "Save as...", New_File_Name, Zip_archives_filters,
      ".zip", File_Title,
      Success
    );
    if not Success then
      return;
    end if;
    if Zip.Exists(To_UTF_8(GU2G(New_File_Name))) then
      if Message_Box (
        Window,
        "Save as",
        "The file " & GU2G (New_File_Name) & " already exists. Replace ?",
        Yes_No_Box,
        Question_Icon
      ) = No
      then
        return;
      end if;
    end if;

    if Is_loaded(Window.zif) then
      begin
        Ada.Directories.Copy_File(
          To_UTF_8(GU2G (Window.File_Name)),
          To_UTF_8(GU2G (New_File_Name)),
          "encoding=utf8"
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
    else -- we don't have a file yet
      Create(empty_zip, Out_File, To_UTF_8(GU2G(New_File_Name)), "encoding=utf8");
      for i in contents'Range loop
        Write(empty_zip, contents(i));
      end loop;
      Close(empty_zip);
    end if;
    Window.File_Name := New_File_Name;
    Text (Window, GU2G (File_Title));
    Window.Short_Name:= File_Title;
    Update_Common_Menus(Window,GU2G(New_File_Name));
    Window.Load_archive_catalogue(False);
  end On_Save_As;

  procedure Set_Modification_Time_B (Name : in String;
                                     To   : in Ada.Calendar.Time) is
  begin
    Ada_Directories_Extensions.Set_Modification_Time(Name, To);
  exception
    when others =>
      null; -- !! utf-8 or ascii names with characters > pos 127 fail
  end Set_Modification_Time_B;

  procedure Process_archive_GWin(
    Window         : in out MDI_Child_Type;
    operation      : Archive_Operation;
    file_names     : Array_Of_File_Names;
    name_match     : Name_matching_mode;
    base_folder    : String;
    search_pattern : GString;
    output_folder  : Wide_String;
    new_temp_name  : String
  )
  is
    az_names: Name_list(File_Names'Range);
    box: Progress_Box_Type;
    aborted: Boolean:= False;
    --
    procedure Abort_clicked ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      aborted:= True;
    end Abort_clicked;
    --
    procedure Boxed_Feedback(
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : String;
      entry_name_encoding   : Zip_name_encoding;
      operation             : Entry_Operation;
      user_abort            : out Boolean
    )
    is
    begin
      box.File_Progress.Position(file_percents_done);
      box.Archive_Progress.Position(archive_percents_done);
      box.Entry_name.Text(To_UTF_16(entry_being_processed, entry_name_encoding));
      box.Entry_operation_name.Text(S2G(Description(operation)));
      Message_Check;
      user_abort:= aborted;
    end Boxed_Feedback;
    --
    procedure Name_conflict_resolution(
      name            :  in String;
      name_encoding   :  in Zip_name_encoding;
      action          : out UnZip.Name_conflict_intervention;
      new_name        : out String;
      new_name_length : out Natural
    )
    is
    pragma Unreferenced (new_name, new_name_length);
      box: File_exists_box_Type;
      use UnZip;
    begin
      box.Create_Full_Dialog(Window);
      box.Conflict_simple_name.Text(To_UTF_16(Remove_path(name), name_encoding));
      box.Conflict_location.Text(output_folder);
      box.Overwrite_Rename.Disable;
      -- !! ^ Needs some effort to make an idiot-proof name query ;-)
      box.Center;
      case Show_Dialog(box, Window) is
        when Overwrite_Yes    =>  action:= yes;
        when Overwrite_No     =>  action:= no;
        when Overwrite_All    =>  action:= yes_to_all;
        when Overwrite_None   =>  action:= none;
        when Overwrite_Rename =>  action:= rename_it;
        when others           =>  action:= abort_now;
      end case;
    end Name_conflict_resolution;
    --
    procedure Get_password(
      entry_name : in String;
      password   : in out GString_Unbounded
    )
    is
      box: Password_input_box_Type;
      pwd_candidate: GString_Unbounded:= Window.current_password;
      --
      procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
        pragma Warnings(off, dummy);
      begin
        pwd_candidate:= G2GU(box.Password_edit.Text);
        Window.Parent.opt.show_passwords:= box.Show_password_box.State = Checked;
      end Get_Data;
      --
      procedure Show_or_Hide ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
        pragma Warnings(off, dummy);
      begin
        if box.Show_password_box.State = Checked then
          box.Password_edit.Password(Off);
        else
          box.Password_edit.Password('=');
        end if;
        box.Password_edit.Redraw;
        box.Password_edit.Focus;
      end Show_or_Hide;
    begin
      box.Create_Full_Dialog(Window);
      box.Encrypted_entry.Text(S2G(entry_name)); -- !! utf-8
      box.Password_edit.Text(GU2G(password));
      if Window.Parent.opt.show_passwords then
        box.Show_password_box.State(Checked);
      else
        box.Show_password_box.State(Unchecked);
      end if;
      box.Center;
      box.On_Destroy_Handler(Get_Data'Unrestricted_Access);
      box.Show_password_box.On_Click_Handler(Show_or_Hide'Unrestricted_Access);
      Show_or_Hide(Window);
      case Show_Dialog(box, Window) is
        when IDOK =>
          password:= pwd_candidate;
        when others =>
          null; -- abandond pwd change
      end case;
    end Get_password;
    --
    -- Instanciation of the GUI-agnostic processing
    --
    procedure Archive_processing is
      new AZip_Common.Operations.Process_archive(
        Boxed_Feedback,
        Get_password
      );
    --
  begin -- Process_archive_GWin
    -- Neutral conversion: GStrings (UTF-16) to UTF_16_String
    for i in az_names'Range loop
      az_names(i):= File_Names(i);
    end loop;
    box.Create_Full_Dialog(Window);
    box.File_Progress.Position(0);
    box.Archive_Progress.Position(0);
    box.Cancel_button.Hide;
    box.Cancel_button_permanent.Show;
    box.Cancel_button_permanent.On_Click_Handler(Abort_clicked'Unrestricted_Access);
    box.Center;
    box.Show;
    Window.Parent.Disable;
    begin
      Archive_processing(
        zif              => Window.zif,
        operation        => operation,
        entry_name       => az_names,
        name_match       => name_match,
        base_folder      => base_folder,
        search_pattern   => search_pattern,
        output_folder    => output_folder,
        Set_Time_Stamp   => Set_Modification_Time_B'Access,
        new_temp_name    => new_temp_name,
        Name_conflict    => Name_conflict_resolution'Unrestricted_Access,
        password         => Window.current_password
      );
      Window.last_operation:= operation;
      if operation in Modifying_Operation then
        Window.Load_archive_catalogue(operation /= Remove);
      else
        Update_display(Window, results_refresh);
      end if;
    exception
      when E : Ada.IO_Exceptions.Name_Error =>
        Message_Box(
          Window,
          "Processing failed",
          "Either the archive cannot be opened (deleted ? moved ?)," & NL &
          "or a new file cannot be written." &
          NL & "-----" & NL &
          S2G(Ada.Exceptions.Exception_Name (E)) & NL &
          S2G(Ada.Exceptions.Exception_Message (E)),
          OK_Box,
          Exclamation_Icon
        );
      when E : Ada.IO_Exceptions.Use_Error =>
        Message_Box(
          Window,
          "Processing failed",
          "Archive cannot be modified (read-only ?)," & NL &
          "or a new file cannot be written." &
          NL & "-----" & NL &
          S2G(Ada.Exceptions.Exception_Name (E)) & NL &
          S2G(Ada.Exceptions.Exception_Message (E)),
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
        if not Ada.Directories.Exists(test_name) then
          return test_name;
        end if;
      end;
    end loop;
  end Temp_AZip_name;

  procedure Go_for_adding (Window     : in out MDI_Child_Type;
                           File_Names : in     Array_Of_File_Names) is
  begin
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
  end Go_for_adding;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names) is
  begin
    Window.Focus;
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
        "File(s) dropped",
        "Add dropped file(s) to archive """ & GU2G(Window.Short_Name) & """ ?",
        Yes_No_Box,
        Question_Icon) = Yes
      then
        Window.Go_for_adding(File_Names);
      end if;
    else
      if Message_Box(
        Window,
        "File(s) dropped",
          "Add dropped file(s) to new archive (" &  GU2G(Window.Short_Name) &
          ") ?" &  NL &
          "You'll be asked first under which name the archive will be created.",
        Yes_No_Box,
        Question_Icon) = Yes
      then
        Window.On_Save_As;
        if Is_Loaded(Window.zif) then
          Window.Go_for_adding(File_Names);
        end if;
      end if;
    end if;
  end On_File_Drop;

  -- This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" ) is
  begin
    Update_Common_Menus( Window.parent.all, top_entry );
  end Update_Common_Menus;

  procedure Load_archive_catalogue (
    Window     : in out MDI_Child_Type;
    copy_codes :        Boolean
  )
  is
    new_zif: Zip_info;
  begin
    Zip.Load(new_zif, To_UTF_8(GU2G(Window.File_Name)));
    if Zip.Is_loaded(Window.zif) then
      if copy_codes then
        Copy_user_codes(Window.zif, new_zif);
      end if;
      Zip.Delete(Window.zif);
    end if;
    Window.zif:= new_zif;
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
    if Window.Parent.user_maximize_restore then
      Window.Parent.opt.MDI_childen_maximized:= Zoom(Window);
    end if;
    case Window.opt.view_mode is
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
  list: constant Array_Of_File_Names:= Get_selected_entry_list(Window);
    function Archive_extract_msg return GString is
    begin
      if list'Length = 0 then
        return "Extract entire archive to...";
      else
        return
          "Extract the" & Integer'Wide_Image(list'Length) &
          " selected entrie(s) to...";
      end if;
    end;
    dir: constant GString:= Get_Directory(Window, Archive_extract_msg);
  begin
    if dir /= "" then
      Process_archive_GWin(
        Window         => Window,
        operation      => Extract,
        file_names     => list,
        name_match     => Exact,
        base_folder    => "",
        search_pattern => "",
        output_folder  => dir,
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
    File_Title : GString_Unbounded;
    File_Names: Array_Of_File_Names_Access;
    procedure Dispose is new Ada.Unchecked_Deallocation(
      Array_Of_File_Names,
      Array_Of_File_Names_Access
    );
  begin
    Open_Files (
      Window,
      "Add files to archive...",
      File_Names,
       ( 1=>(G2GU ("All files (*.*)"),
             G2GU ("*.*"))),
      "",
      File_Title,
      Success
    );
    if Success then
      if not Is_Loaded(Window.zif) then
        Message_Box(
          Window,
          "New archive",
          "You'll be asked under which name the archive will be created.",
          OK_Box,
          Information_Icon
        );
        Window.On_Save_As;
      end if;
      if Is_Loaded(Window.zif) then -- We test again (in case Save As failed)
        Window.Go_for_adding(File_Names.all);
        Dispose(File_Names);
      end if;
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
    box.Name_to_be_searched.Focus;
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
    -- !! Message_Box: say something if any failure
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
    Can_Close:= True;
    if Is_file_saved(Window) then
      Update_Common_Menus(Window,GU2G(Window.File_Name));
      Window.Status_deamon.Stop;
    else -- This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                GU2G(Window.Short_Name) & "' ?",
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
    if Can_Close then
      -- Remember column widths
      for e in Entry_topic'Range loop
        Window.Parent.opt.column_width(e):=
          Window.Directory_List.Column_Width(Entry_topic'Pos(e));
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
      pragma Unreferenced (current_child_window);
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
