with AZip_Common;                       use AZip_Common;
with AZip_GWin.Columns;                 use AZip_GWin.Columns;
with AZip_GWin.Dragging;                use AZip_GWin.Dragging;
with AZip_GWin.Drop_file_dialog;        use AZip_GWin.Drop_file_dialog;
with AZip_GWin.Installation;
with AZip_GWin.Password_dialogs;        use AZip_GWin.Password_dialogs;
with AZip_GWin.Properties;

with Zip_Streams;
with UnZip;
with Zip_time_display;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Colors;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;
with GWindows.Taskbar;

with Ada.Calendar;
with Ada.Directories;
with Ada_Directories_Extensions;
with Ada.Environment_Variables;         use Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Sequential_IO;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces;

package body AZip_GWin.MDI_Child is

  use AZip_GWin.Directory_Lists, Zip;

  function Folder_Focus (Window : in MDI_Child_Type) return Boolean is
  begin
    return
      Window.opt.view_mode = Tree and then
      Window.Focus = Window.Folder_Tree'Unrestricted_Access;
  end Folder_Focus;

  procedure Update_status_bar (Window : in out MDI_Child_Type) is
    function Destination_image return GString is
    begin
      case Window.MDI_Root.dragging.destination is
        when to_desktop =>
          return "to the Desktop";
        when to_explorer =>
          return "to Windows Explorer : " &
                 GU2G (Window.MDI_Root.dragging.destination_path);
        when others =>
          return "";
      end case;
    end Destination_image;
    sel: Natural;
    is_folder_focused : Boolean;
  begin
    if not Is_loaded(Window.zif) then
      Window.Status_Bar.Text ("No archive loaded", 0, Flat);
      return;
    end if;
    --  Here the non-trivial cases.
    is_folder_focused := Folder_Focus (Window);
    if not is_folder_focused then
      sel:= Window.Directory_List.Selected_Item_Count;
    end if;
    if Window.MDI_Root.dragging.is_dragging then
      --  Here are the dragging-from-tree or dragging-from-list cases.
      if is_folder_focused then
        Window.Status_Bar.Text (
          "Dragging a folder " & Destination_image, 0, Sunken
         );
      else
        Window.Status_Bar.Text (
          "Dragging " & Integer'Wide_Image(sel) &
          " selected file(s) " & Destination_image, 0, Sunken
         );
      end if;
    else
      --  Cases without dragging
      if is_folder_focused then
        Text(Window.Status_Bar,"Folder selected",0, Flat);
      elsif sel > 0 then
        Window.Status_Bar.Text (
          Integer'Wide_Image(Window.Directory_List.Item_Count) &
            " file(s)," & Integer'Wide_Image(sel) & " selected", 0, Flat
         );
      else
        Window.Status_Bar.Text (
           Integer'Wide_Image(Window.Directory_List.Item_Count) &
            " file(s), none selected", 0, Flat
         );
      end if;
    end if;
  end Update_status_bar;

  procedure Update_tool_bar_and_menus (Window : in out MDI_Child_Type) is
    not_empty_archive: constant Boolean:=
      Is_loaded(Window.zif) and then Entries(Window.zif) > 0;
    is_folder_focused : Boolean;
    is_any_to_delete : Boolean;
    sel: Natural;
    bar: MDI_Toolbar_Type renames Window.MDI_Root.Tool_Bar;
    boolean_to_state : constant array (Boolean) of State_Type
      := (True => Enabled, False => Disabled);
  begin
    is_folder_focused := Folder_Focus (Window);
    if not is_folder_focused then
      sel:= Window.Directory_List.Selected_Item_Count;
    end if;
    --
    bar.Enabled(IDM_EXTRACT, not_empty_archive);
    State (Window.Menu.Main,           Command, IDM_EXTRACT, boolean_to_state (not_empty_archive));
    State (Window.context_menu_file,   Command, IDM_EXTRACT, boolean_to_state (not_empty_archive));
    State (Window.context_menu_folder, Command, IDM_EXTRACT, boolean_to_state (not_empty_archive));
    --
    is_any_to_delete := not_empty_archive and then (is_folder_focused or else sel > 0);
    bar.Enabled(IDM_Delete_selected, is_any_to_delete);
    State (Window.Menu.Main,           Command, IDM_Delete_selected, boolean_to_state (is_any_to_delete));
    State (Window.context_menu_file,   Command, IDM_Delete_selected, boolean_to_state (is_any_to_delete));
    State (Window.context_menu_folder, Command, IDM_Delete_selected, boolean_to_state (is_any_to_delete));
    --
    bar.Enabled(IDM_FIND_IN_ARCHIVE,    not_empty_archive);
    bar.Enabled(IDM_TEST_ARCHIVE,       not_empty_archive);
    bar.Enabled(IDM_UPDATE_ARCHIVE,     not_empty_archive);
    bar.Enabled(IDM_RECOMPRESS_ARCHIVE, not_empty_archive);
    --
    State (Window.Menu.Main, Command, IDM_FIND_IN_ARCHIVE,    boolean_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_TEST_ARCHIVE,       boolean_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_UPDATE_ARCHIVE,     boolean_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_RECOMPRESS_ARCHIVE, boolean_to_state (not_empty_archive));
    --
    if not Window.is_closing then
      --  Reactivate buttons that might have been disabled upon
      --  closing of another window. These buttons are valid even
      --  on an undefined (new) or existing, but empty, archive.
      bar.Enabled(IDM_ADD_FILES, True);
      bar.Enabled(IDM_Add_Files_Encryption, True);
      bar.Enabled(IDM_Toggle_Flat_Tree_View, True);
      bar.Enabled(IDM_Properties, True);
    end if;
  end Update_tool_bar_and_menus;

  procedure Update_display(
    Window : in out MDI_Child_Type;
    need   :        Update_need
  )
  is

    cidx: Column_integer_array renames Window.opt.column_index;

    procedure Define_columns is
      Lst: Directory_list_type renames Window.Directory_List;
      --
    begin
      for topic in Entry_topic loop
        case need is
          when first_display =>
            Lst.Clear;
            Lst.Insert_Column(
              Image(topic),
              cidx(topic) - 1,
              Get_column_width_from_options (Window, topic)
            );
          when archive_changed | node_selected =>
            Lst.Clear;
            Lst.Set_Column(
              Image(topic),
              cidx(topic) - 1,
              Get_column_width_from_options (Window, topic)
            );
          when results_refresh | status_bar | toolbar_and_menu =>
            null;
        end case;
      end loop;
    end Define_columns;

    -- Window.zif is assumed to be loaded
    --
    procedure Feed_directory_list(prefix_path: GString) is
      row, last_row: Integer:= -1;
      Lst: Directory_list_type renames Window.Directory_List;
      max_entries: constant Natural:= Entries(Window.zif);
      -- This includes potential invisible entries (directory names from Info-Zip, WinZip)
      sorted_index, unsorted_index, result_code: array(0..max_entries-1) of Integer;
      --
      procedure Process_row(
        name_8_bit       : String; -- 'name' is compressed entry's name, with Zip encoding
        file_index       : Zip_Streams.ZS_Index_Type;
        comp_size        : Zip_32_Data_Size_Type;
        uncomp_size      : Zip_32_Data_Size_Type;
        crc_32           : Interfaces.Unsigned_32;
        date_time        : Time;
        method           : PKZip_method;
        name_encoding    : Zip_name_encoding;
        read_only        : Boolean;
        encrypted_2_x    : Boolean; -- PKZip 2.x encryption
        user_code        : in out Integer
      )
      is
        pragma Unreferenced (file_index);
        name: constant UTF_16_String:= To_UTF_16(name_8_bit, name_encoding);
        simple_name_idx: Positive:= name'First;
        previous_idx: Positive:= name'First;
        extension_idx: Positive:= name'Last + 1;
        R_mark: constant array (Boolean) of Character:= (' ', 'R');
        payload_access: AZip_LV_Ex.Data_Access;
        --
        function Encryption_suffix return GString is
        begin
          if encrypted_2_x then
            return " *";
          else
            return "";
          end if;
        end Encryption_suffix;
        --
        w_node, w_parent: Tree_Item_Node;
      begin -- Process_row
        Scan_for_path:
        for i in name'Range loop
          case name(i) is
            when '/' | '\' =>
              -- Directory separator, ok with Unicode UTF-8 names
              previous_idx:= simple_name_idx;
              simple_name_idx:= i + 1;
              --
              -- Feed eventual folder tree
              --
              if Window.opt.view_mode = Tree and need in first_display .. archive_changed then
                declare
                  --  If name is "zip-ada/zip_lib/zip.ads" and i = 16, the partial_path
                  --  will be "zip-ada/zip_lib".
                  partial_path: UTF_16_String renames name(name'First..i-1);
                  partial_path_u: constant GString_Unbounded:= G2GU(partial_path);
                begin
                  if not Window.path_map.Contains(partial_path_u) then
                    if previous_idx = name'First then
                      w_parent:= Tree_Item_Node(Window.path_map.Element(root_key));
                    else
                      w_parent:= Tree_Item_Node(Window.path_map.Element(G2GU(partial_path(name'First..previous_idx-2))));
                    end if;
                    declare
                      --  From the above example, folder_name will be "zip_lib".
                      --  The folder named "zip-ada" will have been inserted previously to
                      --  the root node, when i=8.
                      folder_name: UTF_16_String renames partial_path(previous_idx..i-1);
                    begin
                      Window.Folder_Tree.Insert_Item(folder_name, w_parent, w_node);
                    end;
                    Window.Folder_Tree.Set_Image(w_node, 2, 3);
                    Window.path_map.Insert(partial_path_u, Integer(w_node));
                    Window.node_map.Insert(Integer(w_node), partial_path_u);
                  end if;
                end;
              end if;
            when '.' =>
              -- Last dot is the extension separator
              extension_idx:= i + 1;
            when others =>
              null;
          end case;
        end loop Scan_for_path;
        --
        if simple_name_idx > name'First then
          Window.any_path_in_zip:= True;
        end if;
        if simple_name_idx > name'Last then -- skip directory entries (names end with '/' or '\')
          return;
        end if;
        if Window.opt.view_mode = Tree and then prefix_path /= name(name'First..simple_name_idx-2) then
          return; -- not in a part of the tree to be displayed
        end if;
        if extension_idx < simple_name_idx then
          -- last dot was in a directory name (like: .svn/entries)
          extension_idx:= name'Last + 1; -- will be empty
        end if;
        row:= row + 1;
        if need in first_display .. node_selected then
          Lst.Insert_Item(name(simple_name_idx..name'Last) & Encryption_suffix, row);
          --
          -- Payload
          --
          payload_access:= new LV_Payload'(index_before_sorting => row);
          Lst.Item_Data(row, payload_access);
          --
          Lst.Set_Sub_Item(name(extension_idx..name'Last), row, cidx(FType)-1);
          begin
            Lst.Set_Sub_Item(S2G(Zip_time_display(date_time)), row, cidx(Modified)-1);
          exception
            when Zip_Streams.Calendar.Time_Error =>
              Lst.Set_Sub_Item("(invalid)", row, cidx(Modified)-1);
          end;
          if read_only then -- any attribute
            Lst.Set_Sub_Item(S2G((1 => R_mark(read_only))), row, cidx(Attributes)-1);
          end if;
          Lst.Set_Sub_Item(File_Size_Image(uncomp_size), row, cidx(Size)-1);
          Lst.Set_Sub_Item(File_Size_Image(comp_size), row, cidx(Packed)-1);
          Lst.Set_Sub_Item(Ratio_pct_Image(comp_size, uncomp_size), row, cidx(Ratio)-1);
          Lst.Set_Sub_Item(S2G(Zip.Image(method)), row, cidx(Format)-1);
          Lst.Set_Sub_Item(Hexadecimal(crc_32), row, cidx(CRC32)-1);
          if simple_name_idx > name'First then
            Lst.Set_Sub_Item(name(name'First..simple_name_idx - 1), row, cidx(Path)-1);
          end if;
          Lst.Set_Sub_Item(Zip_name_encoding'Wide_Image(name_encoding), row, cidx(Encoding)-1);
          --
          -- Show some response if the zip directory is very large
          --
          if row mod 2048 = 0 then
            Message_Check;
          end if;
        end if;
        -- This is equal to row if the list is unsorted.
        unsorted_index(row):= Lst.Item_Data(row).index_before_sorting;
        result_code(row):= user_code;
      end Process_row;

      procedure Traverse is new Zip.Traverse_verbose(Process_row);

      az_color: AZip_Common.Operations.RGB_type;
      gw_color: GWindows.Colors.RGB_Type;
      use GWindows.Colors;
      intensity: Float;
      font_color: Color_Type;
    begin -- Feed_directory_list
      if need > results_refresh then
        return;
      end if;
      Window.refreshing_list:= True;
      if need in first_display .. node_selected then
        -- This will be set to True if there is any path during the listing
        Window.any_path_in_zip:= False;
      end if;
      --  Performance is meant to be better with the All_Items mode.
      Window.Directory_List.Color_Mode(AZip_LV_Ex.All_Items);
      --
      --  List is entirely filled on next instruction:
      --
      Traverse(Window.zif);
      --
      --  Finishing touch: the colours in the "Results" column.
      --
      last_row:= row;
      for i in 0..last_row loop
        sorted_index(unsorted_index(i)):= i; -- Nice one, isn't it ?
      end loop;
      for u in 0..last_row loop
        row:= sorted_index(u);
        Lst.Set_Sub_Item(S2G(Result_message(Window.last_operation, result_code(u))), row, cidx(Result)-1);
        Result_color(Window.last_operation, result_code(u), Window.last_max_code, az_color, intensity);
        gw_color:=
          (Red    => GWindows.Colors.Color_Range(az_color.Red),
           Green  => GWindows.Colors.Color_Range(az_color.Green),
           Blue   => GWindows.Colors.Color_Range(az_color.Blue),
           Unused => 0
          );
        if need = results_refresh or az_color /= AZip_Common.Operations.white then
          -- Ensure user can read the text, given the background color.
          if intensity > 0.58 then
            font_color:= Black;
          else
            font_color:= GWindows.Colors.White;
          end if;
          Lst.Subitem_Color(font_color, To_Color(gw_color), row, cidx(Result)-1);
        end if;
        -- Show some response if the zip directory is very large
        --
        if u mod 2048 = 0 then
          Message_Check;
        end if;
      end loop;
      Window.Directory_List.Color_Mode(AZip_LV_Ex.Subitem);
      Window.refreshing_list:= False;
    end Feed_directory_list;

    w_root: Tree_Item_Node;

  begin
    Define_columns;
    case Window.opt.view_mode is
      when Flat =>
        if Is_loaded(Window.zif) then
          Feed_directory_list("");
        end if;
        Check(Window.Menu.Main, Command, IDM_FLAT_VIEW, True);
        Check(Window.Menu.Main, Command, IDM_TREE_VIEW, False);
      when Tree =>
        if need in first_display .. archive_changed then
          Window.path_map.Clear;
          Window.node_map.Clear;
        end if;
        if Is_loaded(Window.zif) then
          if need in first_display .. archive_changed then
            Window.Folder_Tree.Set_Image_List(Window.MDI_Root.Folders_Images);
            Window.Folder_Tree.Delete_Item(Window.Folder_Tree.Get_Root_Item);
            Window.Folder_Tree.Insert_Item(GU2G(Window.Short_Name), 0, w_root, As_A_Root);
            Window.Folder_Tree.Set_Image(w_root, 0, 1);
            Window.path_map.Insert(root_key, Integer(w_root));
            Window.node_map.Insert(Integer(w_root), root_key);
            Window.selected_path:= Null_GString_Unbounded;
          end if;
          Feed_directory_list(GU2G(Window.selected_path));
        end if;
        Check(Window.Menu.Main, Command, IDM_FLAT_VIEW, False);
        Check(Window.Menu.Main, Command, IDM_TREE_VIEW, True);
    end case;
    if need in first_display .. node_selected and then
      Window.MDI_Root.opt.sort_column >= 0
    then
      --  Peform an initial sorting according to current options.
      Window.Directory_List.Sort(
        Window.MDI_Root.opt.sort_column,
        AZip_LV_Ex.Sort_Direction_Type'Value(
          AZip_Common.User_options.Sort_Direction_Type'Image(
            Window.MDI_Root.opt.sort_direction
          )
        )
      );
    end if;
    Window.Update_status_bar;
    Window.Update_tool_bar_and_menus;
  end Update_display;

  procedure Memorize_splitter(Window: in out MDI_Child_Type) is
  begin
    case Window.opt.view_mode is
      when Flat =>
        null; -- do nothing: the splitter is invisible and not used
      when Tree =>
        Window.opt.tree_portion:=
          Float(Window.Folder_Tree.Width) / Float(Window.Client_Area_Width);
    end case;
  end Memorize_splitter;

  overriding procedure On_Bar_Moved (Window : in out MDI_Child_GSize_Bar_Type) is
  begin
    Memorize_splitter(MDI_Child_Type(Window.Parent.Parent.Parent.all));
    GWindows.GControls.GSize_Bars.GSize_Bar_Type(Window).On_Bar_Moved;
  end On_Bar_Moved;

  procedure Check_path (Window : in out MDI_Child_Type; go_up : Boolean) is
    use AZip_Common.Path_Catalogues;
    go_up_done : Boolean := False;
    sel_node   : Tree_Item_Node;
    curs       : Cursor;
    idx        : Integer;
  begin
    case Window.opt.view_mode is
      when Flat =>
        null;
      when Tree =>
        if Is_loaded (Window.zif) then
          loop
            --  Fix 21-Jul-2019: since GNAT GPL 2016 the pragma Suppress(Container_Checks)
            --  is activated with the -gnatp (Suppress all checks) switch and the
            --  function Element won't raise Constraint_Error when the selected path is not
            --  found (RM: A.18.4, 69/2). Instead, a Program_Error with Access_Violation, or
            --  something else, may occur. The workaround is to use a cursor and
            --  the Find function. For details, see:
            --  https://groups.google.com/forum/#!original/comp.lang.ada/HZsG7Czymto/kY4BjoD0BAAJ
            curs := Window.path_map.Find (Window.selected_path);
            if
                --  The selected path doesn't exist anymore. We'll try again by going one
                --  folder up. This is done by truncating the last folder name from the right.
                curs = No_Element
              or else
                --  Here, we want to go up (at least) one folder.
                (go_up and not go_up_done)
            then
              idx := Integer'Max (
                Index(Window.selected_path, "/", Backward),  --  Regular Zip (and Unix) separator.
                Index(Window.selected_path, "\", Backward)   --  Wrong, but may happen.
              );
              if idx = 0 then
                --  We are at the root.
                Window.selected_path := Null_GString_Unbounded;
              else
                --  Truncation to a non-empty string was successful, e.g.
                --  "zip-ada/zip_lib" becomes "zip-ada".
                Window.selected_path := Unbounded_Slice (Window.selected_path, 1, idx-1);
              end if;
              go_up_done := True;
            else
              sel_node := Tree_Item_Node (Element (curs));
              exit;
            end if;
          end loop;
          Window.Folder_Tree.Select_Item (sel_node);
          Update_display (Window, node_selected);
          Window.Folder_Tree.Expand (sel_node);
          Window.Folder_Tree.Focus;
        end if;
    end case;
  end Check_path;

  procedure Change_View (
        Window   : in out MDI_Child_Type;
        new_view :        View_Mode_Type;
        force    :        Boolean
  )
  is
    mem_sel_path: constant GString_Unbounded:= Window.selected_path;
  begin
    if Window.opt.view_mode = new_view and not force then
      return;
    end if;
    Window.opt.view_mode:= new_view;
    case new_view is
      when Flat =>
        if not force then
          Memorize_splitter(Window);
          -- Remember tree portion for user persistence or for next time we toggle back to tree view.
        end if;
        Window.Splitter.Hide;
        Window.Folder_Tree.Hide;
      when Tree =>
        Window.Splitter.Show;
        Window.Folder_Tree.Show;
    end case;
    Window.On_Size(Window.Width, Window.Height);
    Update_display(Window, archive_changed);
    Window.selected_path:= mem_sel_path;
    Check_path (Window, go_up => False);
  end Change_View;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
    use GWindows.Packing_Boxes;
  begin
    Window.Small_Icon("Box_Closed_Icon_Name");

    -- Filial feelings:
    Window.MDI_Root:= MDI_Main_Access (Controlling_Parent(Window));
    -- We copy options to child level:
    Window.opt:= Window.MDI_Root.opt;

    Window.Tree_Bar_and_List.Create(Window, Direction => Horizontal);
    Window.Tree_Bar_and_List.Dock(At_Top);

    Window.Folder_Tree.Create(
      Window.Tree_Bar_and_List,
      1,1,20,20,
      Lines_At_Root => False
    );
    Window.Folder_Tree.Dock(Fill);

    --  Sliding panel with split bar and list on it:
    Window.Bar_and_List.Create(Window.Tree_Bar_and_List, 1,1,20,20);
    Window.Bar_and_List.Dock(At_Right);
    --  The split bar:
    Window.Splitter.Create(Window.Bar_and_List, At_Left);
    --  The list:
    Window.Directory_List.Create(Window.Bar_and_List, 50,1,20,20, Multiple, Report_View, Sort_Custom);
    Window.Directory_List.Set_Extended_Style (Full_Row_Select);
    Window.Directory_List.Color_Mode(AZip_LV_Ex.Subitem);
    Window.Directory_List.Dock(Fill);

    Window.Status_Bar.Create(Window, "No archive");
    Window.Status_Bar.Parts((0 => 600, 1 => -1));
    Window.Status_Bar.Dock(At_Bottom);

    Window.Dock_Children;
    if Window.opt.view_mode = Tree then
      Change_View(Window, Tree, force => True);
    end if;

    ------------
    --  Menus --
    ------------

    AZip_Resource_GUI.Create_Full_Menu(Window.Menu);
    Window.MDI_Menu(Window.Menu.Main, Window_Menu => 6);
    Append_Item (Window.context_menu_file, "&Extract file(s)", IDM_EXTRACT);
    Append_Item (Window.context_menu_file, "&Delete file(s)",  IDM_Delete_selected);
    Append_Item (Window.context_menu_folder, "&Extract folder", IDM_EXTRACT);
    Append_Item (Window.context_menu_folder, "&Delete folder",  IDM_Delete_selected);

    -- Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:=
        not Window.MDI_Root.opt.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Window.MDI_Root.Freeze;
        Window.Zoom;
      end if;
      On_Size(Window,Width(Window),Height(Window));
      if memo_unmaximized_children then
        Window.MDI_Root.Thaw; -- Before Zoom, otherwise uncomplete draw.
        Window.Zoom(False);
        Window.MDI_Root.Tool_Bar.Redraw;
      end if;
    end;
    Window.Status_deamon.Start;
    Window.Update_display(first_display);
    Window.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset(Window.temp_name_gen);
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
      Window, "Save Zip archive as...", New_File_Name, Zip_archives_filters,
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
    Window.Text(GU2G(File_Title));
    Window.Short_Name:= File_Title;
    Window.Update_Common_Menus(GU2G(New_File_Name));
    Window.Load_archive_catalogue(copy_codes => False);
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
    base_folder    : GString;
    search_pattern : GString;
    output_folder  : Wide_String;
    ignore_path    : Boolean; -- ignore directories upon extraction
    encrypt        : Boolean;
    new_temp_name  : String;
    return_code    : out Operation_return_code
  )
  is
    is_aborted : Boolean := False;
    --
    procedure Abort_clicked ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      is_aborted:= True;  --  Will propagate user_abort upon next Boxed_Feedback.
    end Abort_clicked;
    --
    tick : Ada.Calendar.Time;
    progress_box : Progress_box_Type;
    --
    procedure Boxed_Feedback (
      file_percents_done    : Natural;
      archive_percents_done : Natural;
      entry_being_processed : GString;
      e_operation           : Entry_Operation;
      comment_1, comment_2  : String; -- e.g. #found so far, time elpased,...
      skip_hint             : Boolean;
      user_abort            : out Boolean
    )
    is
      use Ada.Calendar, Ada.Strings.Wide_Fixed;
      now: constant Ada.Calendar.Time:= Clock;
    begin
      -- Display only at most every 4/100 second (i.e. max 25 fps).
      -- Otherwise Windows may be overflown by messages and the operation
      -- takes much more time due to the display. Typical case: an archive
      -- with many small files - GWin.zip or Java run-time's Jar for instance.
      if now - tick >= 0.04 or else archive_percents_done = 100 then
        progress_box.File_Progress.Position (file_percents_done);
        progress_box.Archive_Progress.Position (archive_percents_done);
        Window.MDI_Root.Text (
          Trim (Integer'Wide_Image (archive_percents_done), Left) &
          "% done - " & S2G (AZip_GWin.Installation.AZip_Title)
        );
        if Window.MDI_Root.Task_bar_gadget_ok then
          Window.MDI_Root.Task_bar_gadget.Set_Progress_Value (Window.MDI_Root.all, archive_percents_done, 100);
        end if;
        progress_box.Entry_name.Text(entry_being_processed);
        progress_box.Entry_operation_name.Text(
          Description(e_operation, operation, skip_hint)
        );
        progress_box.Comment_1.Text(S2G(comment_1));
        progress_box.Comment_2.Text(S2G(comment_2));
        if archive_percents_done = 100 and then comment_1 /= "" then
          Window.last_op_comment_1:= G2GU(S2G(comment_1));
          Window.last_op_comment_2:= G2GU(S2G(comment_2));
        end if;
        Message_Check;
        tick:= now;
      end if;
      user_abort:= is_aborted;
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
      box.Create_Full_Dialog(progress_box);
      box.Conflict_simple_name.Text(Remove_path(To_UTF_16(name, name_encoding)));
      box.Conflict_location.Text(output_folder);
      box.Overwrite_Rename.Disable;
      -- !! ^ Needs some effort to make an idiot-proof name query ;-)
      box.Center;
      case Show_Dialog(box, progress_box) is
        when Overwrite_Yes    =>  action:= yes;
        when Overwrite_No     =>  action:= no;
        when Overwrite_All    =>  action:= yes_to_all;
        when Overwrite_None   =>  action:= none;
        when Overwrite_Rename =>  action:= rename_it;
        when others           =>  action:= abort_now;
      end case;
    end Name_conflict_resolution;
    --
    procedure Get_password_decrypt_for_Common(
      entry_name : in     GString;
      password   : in out GString_Unbounded;
      cancelled  :    out Boolean
    )
    is
    begin
      Get_password_for_decryption(
        Window     => Window,
        Parent     => progress_box,
        entry_name => entry_name,
        password   => password,
        cancelled  => cancelled
      );
    end Get_password_decrypt_for_Common;
    --
    --  Instanciation of the GUI-agnostic processing
    --
    procedure Archive_processing is
      new AZip_Common.Operations.Process_archive (
        Boxed_Feedback,
        Get_password_decrypt_for_Common
      );
    --
    function Msg_Name_Error return GString is
    begin
      if operation in Modifying_Operation then
        return
          "Either:" & NL &
          "  - The archive file cannot be opened - " & NL &
          "      perhaps file has been deleted or moved ?" & NL &
          "  - A new file cannot be written.";
      else
        return
          "The archive file cannot be opened - " & NL &
          "  perhaps file has been deleted or moved ?";
      end if;
    end Msg_Name_Error;
    --
    az_names : Name_list (file_names'Range);
    use GWindows.Taskbar, GWin_Util;
  begin  --  Process_archive_GWin
    --  Neutral conversion: GStrings (UTF-16) to UTF_16_String
    for i in az_names'Range loop
      az_names (i).str := file_names (i);
    end loop;
    tick := Ada.Calendar."-" (Ada.Calendar.Clock, 1.0);
    progress_box.Create_Full_Dialog (Window);
    progress_box.File_Progress.Position (0);
    progress_box.Archive_Progress.Position (0);
    if Window.MDI_Root.Task_bar_gadget_ok then
      Window.MDI_Root.Task_bar_gadget.Set_Progress_Value (Window.MDI_Root.all, 0, 100);
    end if;
    progress_box.Cancel_button.Hide;
    progress_box.Cancel_button_permanent.Text (Cross & "   Cancel");
    progress_box.Cancel_button_permanent.Show;
    progress_box.Cancel_button_permanent.On_Click_Handler (Abort_clicked'Unrestricted_Access);
    progress_box.Center;
    progress_box.Redraw;
    progress_box.Show;
    Window.MDI_Root.Disable;
    progress_box.Text (progress_box.Text & " Operation: " & Img (operation));
    begin
      Archive_processing(
        zif              => Window.zif,
        operation        => operation,
        entry_name       => Expand_folders(az_names),
        base_folder      => base_folder,
        search_pattern   => search_pattern,
        output_folder    => output_folder,
        Set_Time_Stamp   => Set_Modification_Time_B'Access,
        new_temp_name    => new_temp_name,
        Name_conflict    => Name_conflict_resolution'Unrestricted_Access,
        password         => Window.current_password,
        ignore_path      => ignore_path,
        encrypt          => encrypt,
        max_code         => Window.last_max_code,
        return_code      => return_code
      );
      Window.MDI_Root.Text (S2G (AZip_GWin.Installation.AZip_Title));  --  Remove progress info.
      Window.last_operation:= operation;
      case return_code is
        when ok =>
          if operation in Modifying_Operation then
            Window.Load_archive_catalogue(copy_codes => operation /= Remove);
          else
            Update_display(Window, results_refresh);
          end if;
        when archive_too_large =>
          Message_Box (Window,
            "Archive change: Zip capacity exceeded",
            "New archive would be larger than possible with the Zip format." & NL &
            "Change has been cancelled.",
          OK_Box,
          Error_Icon);
        when too_many_entries =>
          Message_Box (Window,
            "Archive change: Zip capacity exceeded",
            "New archive would have more entries than possible with the Zip format." & NL &
            "Change has been cancelled.",
          OK_Box,
          Error_Icon);
        when aborted =>
          null;
      end case;
    exception
      when E : Ada.IO_Exceptions.Name_Error =>
        return_code := aborted;
        Message_Box(
          Window,
          "Processing failed",
          Msg_Name_Error &
          NL & "-----" & NL &
          S2G(Ada.Exceptions.Exception_Message (E)),
          OK_Box,
          Exclamation_Icon
        );
      when E : Ada.IO_Exceptions.Use_Error =>
        return_code := aborted;
        Message_Box(
          Window,
          "Processing failed",
          "Archive cannot be modified (perhaps, is it read-only ?)," & NL &
          "or a new file cannot be written." &
          NL & "-----" & NL &
          S2G(Ada.Exceptions.Exception_Message (E)),
          OK_Box,
          Exclamation_Icon
        );
    end;
    if Window.MDI_Root.Task_bar_gadget_ok then
      Window.MDI_Root.Task_bar_gadget.Set_Progress_State (Window.MDI_Root.all, No_Progress);
    end if;
    Window.MDI_Root.Enable;
    Window.MDI_Root.Focus;
  end Process_archive_GWin;

  function Temp_AZip_name(Window: MDI_Child_Type) return String is
  begin
    loop
      declare
        num0: constant String:=
          Float'Image(Ada.Numerics.Float_Random.Random(Window.temp_name_gen));
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

  procedure Go_for_adding (
    Window     : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names;
    Encrypt    : in     Boolean
  )
  is
    function Eventual_folder return GString is
      sel_path: constant GString:= GU2G(Window.selected_path);
    begin
      case Window.opt.view_mode is
        when Flat =>
          return "";
        when Tree =>
          if sel_path = "" then
            return "";  -- root
          else
            return sel_path & '/'; -- need a separator here
          end if;
      end case;
    end Eventual_folder;
    return_code : Operation_return_code;
  begin
    Process_archive_GWin(
      Window         => Window,
      operation      => Add,
      file_names     => File_Names,
      base_folder    => Eventual_folder,
      search_pattern => "",
      output_folder  => "",
      ignore_path    => False,
      encrypt        => Encrypt,
      new_temp_name  => Temp_AZip_name(Window),
      return_code    => return_code
    );
  end Go_for_adding;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names) is
    function Eventual_folder return GString is
      sel_path: constant GString:= GU2G(Window.selected_path);
    begin
      case Window.opt.view_mode is
        when Flat =>
          return "";
        when Tree =>
          if sel_path = "" then
            return NL & NL & "Current folder is archive's root";
          else
            return NL & NL & "Current folder is: " & sel_path;
          end if;
      end case;
    end Eventual_folder;
    encrypt   : Boolean:= False;
    yes       : Boolean;
    cancelled : Boolean;
    parent    : MDI_Main_Access;
  begin
    Window.Focus;
    if Confirm_archives_if_all_Zip_files (Window, File_Names) then
      --  All files are Zip archives (even those without .zip extension).
      --
      --  We save the parent access since Window may have been closed
      --  since the second iteration if Window was a temporary MS-Office-like
      --  blank window - See procedure Close_extra_first_child.
      parent:= Window.MDI_Root;
      --
      for i in File_Names'Range loop
        Open_Child_Window_And_Load (parent.all, File_Names(i));
      end loop;
    else
      Do_drop_file_dialog (
        Parent         => Window,
        archive_name   => GU2G (Window.Short_Name) & Eventual_folder,
        new_archive    => not Is_loaded (Window.zif),
        encrypt        => encrypt,
        yes            => yes
      );
      if yes then
        if not Is_loaded (Window.zif) then
          Window.On_Save_As;
        end if;
        --
        if Is_loaded (Window.zif) then
          if encrypt then
            Get_password_for_encryption (Window, cancelled);
          else
            cancelled := False;
          end if;
          if not cancelled then
            Window.Go_for_adding (File_Names, Encrypt => encrypt);
          end if;
        end if;
      end if;
    end if;
  end On_File_Drop;

  -- This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" ) is
  begin
    Update_Common_Menus( Window.MDI_Root.all, top_entry );
  end Update_Common_Menus;

  procedure Load_archive_catalogue (
    Window     : in out MDI_Child_Type;
    copy_codes :        Boolean
  )
  is
    new_zif: Zip_info;
  begin
    Load_insensitive_if_possible(new_zif, To_UTF_8(GU2G(Window.File_Name)));
    if Zip.Is_loaded(Window.zif) then
      if copy_codes then
        Set_user_codes(new_zif, appended);
        Copy_user_codes(Window.zif, new_zif);
      end if;
    end if;
    Window.zif:= new_zif;
    Change_View(Window, Window.opt.view_mode, force => True);
    -- Update_display(Window, archive_changed); -- included in Change_View
    -- Window.Status_deamon.Display(Window'Unchecked_Access);
  end Load_archive_catalogue;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);   -- only client area is considered
    pragma Warnings (Off, Height);  -- only client area is considered
    w: constant Natural:= Window.Client_Area_Width;
    h: constant Natural:= Integer'Max(2, Window.Client_Area_Height - Window.Status_Bar.Height);
    splitter_w: constant:= 4; -- between tree and list
    tree_w: constant Integer:= Integer(Window.opt.tree_portion * Float(w)) - splitter_w / 2;
    use GWindows.Types;
  begin
    if Window.MDI_Root.User_maximize_restore then
      Window.MDI_Root.opt.MDI_childen_maximized:= Zoom(Window);
    end if;
    Window.Tree_Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
    case Window.opt.view_mode is
      when Flat =>
        Window.Folder_Tree.Location(Rectangle_Type'(0, 0, 1, h));
        Window.Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
        Window.Splitter.Location(Rectangle_Type'(0, 0, 1, h));
        Window.Directory_List.Location(Rectangle_Type'(0, 0, w, h));
      when Tree =>
        Window.Folder_Tree.Location(Rectangle_Type'(0, 0, tree_w, h));
        Window.Bar_and_List.Location(Rectangle_Type'(tree_w, 0, w, h));
        -- Splitter bar and directory list are inside the Bar_and_List panel
        Window.Splitter.Location(Rectangle_Type'(0, 0, splitter_w, h));
        Window.Directory_List.Location(Rectangle_Type'(splitter_w, 0, Window.Bar_and_List.Width, h));
    end case;
    Dock_Children (Window);
  end On_Size;

  function Get_selected_entry_list(Window: MDI_Child_Type) return Array_Of_File_Names is
    items: constant Natural:= Window.Directory_List.Item_Count;
    names: Array_Of_File_Names(1..items);
    j: Natural:= 0;
  begin
    for i in 0..items - 1 loop -- 0-based
      if Window.Directory_List.Is_Selected(i) then
        j:= j + 1;
        names(j):= G2GU(
          Window.Directory_List.Text(i, SubItem => 9) & -- path
          Window.Directory_List.Text(i, SubItem => 0)   -- simple name
          );
        if Element(names(j), Length(names(j))) = '*' then
          -- Remove the " *" appended on encrypted entries.
          names(j):= Unbounded_Slice(names(j), 1, Length(names(j))-2);
        end if;
      end if;
    end loop;
    return names(1..j);
  end Get_selected_entry_list;

  -- Selected folder's and subfolder's contents (Tree view)
  function Get_selected_folder_entry_list(Window: MDI_Child_Type) return Array_Of_File_Names is
    items: constant Natural:= Entries(Window.zif);
    names: Array_Of_File_Names(1..items);
    prefix: constant GString:= GU2G(Window.selected_path) & '/';
    j: Natural:= 0;
    --
    procedure Process_entry(
      name_8_bit       : String; -- 'name' is compressed entry's name, with Zip encoding
      name_encoding    : Zip_name_encoding
    )
    is
      name: constant UTF_16_String:= To_UTF_16(name_8_bit, name_encoding);
    begin
      if name'Length >= prefix'Length and then
         name(name'First..name'First+prefix'Length-1) = prefix
      then
        j:= j+1;
        names(j):= G2GU(name);
      end if;
    end Process_entry;
    procedure Traverse_names is new Zip.Traverse_Unicode(Process_entry);
  begin
    Traverse_names(Window.zif);
    return names(1..j);
  end Get_selected_folder_entry_list;

  function Any_path_in_list(names: Array_Of_File_Names) return Boolean is
  begin
    for i in names'Range loop
      if Index(names(i), "/") > 0 or else Index(names(i), "\") > 0 then
        return True;
      end if;
    end loop;
    return False;
  end Any_path_in_list;

  procedure On_Extract (
    Window  : in out MDI_Child_Type;
    dropped : in     Boolean;
    drop_X  : in     Integer := -1;
    drop_Y  : in     Integer := -1
  )
  is
    sel_list : constant Array_Of_File_Names := Get_selected_entry_list (Window);
    is_folder_focused : constant Boolean := Folder_Focus (Window);
    --
    function Smart_list return Array_Of_File_Names is
    begin
      if is_folder_focused then
        return Get_selected_folder_entry_list (Window);
      else
        return sel_list; -- If the list is empty, whole archive will be extracted
      end if;
    end Smart_list;
    --
    list: constant Array_Of_File_Names:= Smart_list;
    --
    function Archive_extract_msg return GString is
    begin
      if is_folder_focused then
        return "Extract current folder's contents";
      elsif sel_list'Length > 0 then
        return "Extract the" & Integer'Wide_Image(sel_list'Length) & " selected item(s)";
      else
        return "Extract entire archive";
      end if;
    end Archive_extract_msg;
    --
    function Use_path_question return GString is
    begin
      case Window.opt.view_mode is
        when Flat =>
          return "Use archive's directories as seen on the ""Path"" column ?";
        when Tree =>
          return "Use archive's folder names for output ?";
      end case;
    end Use_path_question;
    --
    ask : Boolean;
    box_kind : Message_Box_Type;
    dir : GString_Unbounded;
    return_code : Operation_return_code;
  begin
    if not Is_loaded(Window.zif) then
      return; -- No archive, then nothing to do
    end if;
    if dropped then
      dir:= G2GU(Explorer_Path_At_Location (drop_X, drop_Y));
      if ((list'Length > 4 and then Is_Desktop_At_Location (drop_X, drop_Y))
          or else
          list'Length > 20
         )
        and then (not is_folder_focused)  --  Bunch of individual files, not a whole folder
        and then Message_Box (
          Window, Archive_extract_msg,
          "You are dropping a large amount of files. Continue?",
          Yes_No_Box, Question_Icon ) = No
      then
        return;
      end if;
    else
      dir:= G2GU (Get_Directory (
        Window       => Window,
        Dialog_Title => Archive_extract_msg & " to...",
        Initial_Path => GU2G(Window.extract_dir) ));
    end if;
    if dir = "" then
      return;
    end if;
    Window.extract_dir:= dir;
    if list'Length > 0 then
      --  Bunch of individual files, or a non-empty folder
      ask:= Any_path_in_list(list);
    else
      --  Entire archive
      ask:= Window.any_path_in_zip;
    end if;
    if ask then
      --  We have folders in some of the file names.
      if Window.opt.ignore_extract_path then
        box_kind:= Yes_No_Def_Cancel_Box; -- Previous answer was "No", so we take "No" as default
      else
        box_kind:= Yes_No_Cancel_Box;
      end if;
      case Message_Box (
        Window, Archive_extract_msg,
        Use_path_question, box_kind, Question_Icon )
      is
        when No =>
          Window.opt.ignore_extract_path:= True;
        when Yes =>
          Window.opt.ignore_extract_path:= False;
        when others =>
          return;
      end case;
    end if;
    Process_archive_GWin(
      Window         => Window,
      operation      => Extract,
      file_names     => list,
      base_folder    => "",
      search_pattern => "",
      output_folder  => GU2G(dir),
      ignore_path    => Window.opt.ignore_extract_path,
      encrypt        => False,
      new_temp_name  => "",
      return_code    => return_code
    );
    if return_code = ok and not dropped then
      --  Open destination path's folder
      GWin_Util.Start(G2S(GU2G(dir)));  --  !! not unicode
    end if;
  end On_Extract;

  procedure On_Delete(Window : in out MDI_Child_Type) is
    sel_list : constant Array_Of_File_Names := Get_selected_entry_list (Window);
    is_folder_focused : constant Boolean := Folder_Focus (Window);
    --
    function Smart_list return Array_Of_File_Names is
    begin
      if is_folder_focused then
        return Get_selected_folder_entry_list(Window);
      else
        return sel_list;
      end if;
    end Smart_list;
    --
    function Delete_msg return GString is
    begin
      if is_folder_focused then
        return "Do you want to remove the entire selected FOLDER and subfolders ?";
      else
        return "Do you want to remove the" & Integer'Wide_Image(sel_list'Length) &
          " selected item(s) ?";
      end if;
    end Delete_msg;
    return_code : Operation_return_code;
  begin
    if Window.Directory_List.Selected_Item_Count = 0 and not is_folder_focused then
      return; -- no item, no folder -> do nothing (different from On_Extract's behaviour)
    end if;
    if Message_Box(Window, "Delete", Delete_msg, Yes_No_Box, Question_Icon) = Yes then
      Process_archive_GWin(
        Window         => Window,
        operation      => Remove,
        file_names     => Smart_list,
        base_folder    => "",
        search_pattern => "",
        output_folder  => "",
        ignore_path    => False,
        encrypt        => False,
        new_temp_name  => Temp_AZip_name(Window),
        return_code    => return_code
      );
    end if;
  end On_Delete;

  procedure On_Add_files(Window : in out MDI_Child_Type; encrypted: Boolean) is
    Success, cancelled: Boolean;
    File_Title : GString_Unbounded;
    File_Names: Array_Of_File_Names_Access;
    procedure Dispose is new Ada.Unchecked_Deallocation(
      Array_Of_File_Names,
      Array_Of_File_Names_Access
    );
  begin
    Open_Files (
      Window,
      "Add files to archive (also doable by Drag & Drop)...",
      File_Names,
       ( 1=>(G2GU ("All files (*.*)"),
             G2GU ("*.*"))),
      "",
      File_Title,
      Success
    );
    if Success then
      if not Is_loaded(Window.zif) then
        Message_Box(
          Window,
          "New archive",
          "You'll be asked under which name the archive will be created.",
          OK_Box,
          Information_Icon
        );
        Window.On_Save_As;
      end if;
      if encrypted then
        Get_password_for_encryption(Window, cancelled);
      else
        cancelled:= False;
      end if;
      if Is_loaded(Window.zif) and not cancelled then -- Is_Loaded: we test again (in case Save As failed)
        Window.Go_for_adding(File_Names.all, encrypted);
        Dispose(File_Names);
      end if;
    end if;
  end On_Add_files;

  procedure On_Add_folder(Window : in out MDI_Child_Type; encrypted: Boolean) is
    cancelled: Boolean;
    dir: constant GString:= Get_Directory(
      Window       => Window,
      Dialog_Title => "Add folder to archive (also doable by Drag & Drop)...",
      Initial_Path => GU2G(Window.extract_dir) );
  begin
    if dir = "" then
      return;
    end if;
    if not Is_loaded(Window.zif) then
      Message_Box(
        Window,
        "New archive",
        "You'll be asked under which name the archive will be created.",
        OK_Box,
        Information_Icon
      );
      Window.On_Save_As;
    end if;
    if encrypted then
      Get_password_for_encryption(Window, cancelled);
    else
      cancelled:= False;
    end if;
    if Is_loaded(Window.zif) and not cancelled then -- Is_Loaded: we test again (in case Save As failed)
      Window.Go_for_adding((1 => G2GU(dir)), encrypted);
    end if;
  end On_Add_folder;

  procedure On_Find(Window : in out MDI_Child_Type) is
    box: Find_box_Type;
    --
    procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      Window.name_search:= G2GU(box.Name_to_be_searched.Text);
      Window.content_search:= G2GU(box.Content_to_be_searched.Text);
    end Get_Data;
    --
    return_code : Operation_return_code;
  begin
    box.Create_Full_Dialog(Window);
    box.Name_to_be_searched.Text(GU2G(Window.name_search));
    box.Content_to_be_searched.Text(GU2G(Window.content_search));
    box.Center;
    box.On_Destroy_Handler(Get_Data'Unrestricted_Access);
    box.Name_to_be_searched.Focus;
    if Show_Dialog (box, Window) = IDOK then
      Process_archive_GWin(
        Window         => Window,
        operation      => Search,
        file_names     => (1 => Window.name_search),
        base_folder    => "",
        search_pattern => GU2G(Window.content_search),
        output_folder  => "",
        ignore_path    => False,
        encrypt        => False,
        new_temp_name  => "",
        return_code    => return_code
      );
      if Message_Box(Window,
          "Find in archive",
          "Search completed." & NL & NL &
          GU2G(Window.last_op_comment_1) & NL &
          GU2G(Window.last_op_comment_2) & NL & NL &
          "Do you want to see full results (flat view & result sort) ?",
          Yes_No_Box,
          Question_Icon)
        = Yes
      then
        Change_View(Window, Flat, force => False);
        Window.Directory_List.Sort(Window.opt.column_index(Result) - 1, AZip_LV_Ex.Down);
      end if;
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
    count_ok, count_ko, count_nt: Natural;
    return_code : Operation_return_code;
  begin
    Process_archive_GWin(
      Window         => Window,
      operation      => Test,
      file_names     => Empty_Array_Of_File_Names,
      base_folder    => "",
      search_pattern => "",
      output_folder  => "",
      ignore_path    => False,
      encrypt        => False,
      new_temp_name  => "",
      return_code    => return_code
    );
    Count_test_totals(Window.zif, count_ok, count_ko, count_nt);
    if count_nt > 0 then
      null; -- operation cancelled, nothing to say
    elsif count_ko = 0 then
      Message_Box(Window,
        "Data integrity",
        "All entries in archive are OK.",
        Icon => Information_Icon
      );
    else
      Message_Box(Window,
        "Data integrity: at least one failure",
        Integer'Wide_Image(count_ok) & " entries are OK;" & NL &
        Integer'Wide_Image(count_ko) &
        " entries had errors or could not be processed.",
        Icon => Warning_Icon
      );
    end if;
  end On_Test;

  procedure Stop_msg_on_encrypted_archive(Window : MDI_Child_Type; op_title: GString) is
  begin
      Message_Box(
        Window,
        op_title,
        "Archives with encryption (even on some entries only) are" & NL &
        "currently not supported for this operation (" & op_title & ").",
        OK_Box,
        Stop_Icon
      );
  end Stop_msg_on_encrypted_archive;

  procedure On_Update(Window : in out MDI_Child_Type) is
    mem_dir: constant String:= Ada.Directories.Current_Directory;
    -- !! Not UTF-8 capable
    new_dir: constant String:= Ada.Directories.Containing_Directory(
      To_UTF_8(GU2G (Window.File_Name))
    ); -- !! Not UTF-8 capable
    return_code : Operation_return_code;
  begin
    if not Is_loaded(Window.zif) then
      return;
    end if;
    if Has_Zip_archive_encrypted_entries(Window.zif) then
      Stop_msg_on_encrypted_archive(Window, "Archive update");
      return;
    end if;
    if Message_Box(
      Window,
      "Archive update",
      "You are about to start an archive update." & NL & NL &
      "Files that are newer and different (according to " &
      "their CRC32 code) will replace those in the archive." & NL & NL &
      "Proceed ?",
      Yes_No_Box,
      Question_Icon
    ) = Yes
    then
      Ada.Directories.Set_Directory(new_dir); -- !! Not UTF-8 capable
      Process_archive_GWin(
        Window         => Window,
        operation      => Update,
        file_names     => Empty_Array_Of_File_Names,
        base_folder    => "", -- We update the whole archive
        search_pattern => "",
        output_folder  => "",
        ignore_path    => False,
        encrypt        => False,
        new_temp_name  => Temp_AZip_name(Window),
        return_code    => return_code
      );
      if mem_dir'Length > 0 and then mem_dir(mem_dir'Last) =':' then
        -- Bug in GNAT up to GPL 2011 - cf issue [L216-021 public], fixed in 2012.
        Ada.Directories.Set_Directory(mem_dir & '\'); -- !! Check if UTF-8 capable
      else
        Ada.Directories.Set_Directory(mem_dir); -- !! Check if UTF-8 capable
      end if;
      if return_code = aborted or return_code = archive_too_large then
        null;
      elsif Window.last_max_code <= only_archive then
        Message_Box(Window,
        "Archive update completed",
        "Update completed." & NL & "No entry needed to be updated.",
        OK_Box,
        Information_Icon);
      elsif Message_Box(Window,
          "Archive update completed",
          "Update completed." & NL & NL &
          "Do you want to see full results (flat view & result sort) ?",
          Yes_No_Box,
          Question_Icon)
        = Yes
      then
        Change_View(Window, Flat, force => False);
        Window.Directory_List.Sort(Window.opt.column_index(Result) - 1, AZip_LV_Ex.Down);
      end if;
    end if;
  end On_Update;

  procedure On_Recompress(Window : in out MDI_Child_Type) is
    return_code : Operation_return_code;
  begin
    if not Is_loaded(Window.zif) then
      return;
    end if;
    if Has_Zip_archive_encrypted_entries(Window.zif) then
      Stop_msg_on_encrypted_archive(Window, "Archive recompression");
      return;
    end if;
    if Message_Box(
      Window,
      "Archive recompression",
      "You are about to recompress this archive." & NL & NL &
      "Contents will remain identical, but data compression may be better." & NL &
      "This operation can take a long time depending on data size and content." & NL & NL &
      "Proceed ?",
      Yes_No_Box,
      Question_Icon
    ) = Yes
    then
      Process_archive_GWin(
        Window         => Window,
        operation      => Recompress,
        file_names     => Empty_Array_Of_File_Names,
        base_folder    => "", -- We recompress the whole archive
        search_pattern => "",
        output_folder  => "",
        ignore_path    => False,
        encrypt        => False,
        new_temp_name  => Temp_AZip_name(Window),
        return_code    => return_code
      );
      if return_code = aborted then
        null;
      elsif Window.last_max_code = nothing then
        Message_Box(Window,
        "Archive recompression completed",
        "Recompression completed." & NL & "No entry could be recompressed to a smaller size.",
        OK_Box,
        Information_Icon);
      elsif Message_Box(Window,
        "Archive recompression completed",
        "Recompression completed." & NL & NL &
        "Do you want to see full results (flat view & result sort) ?",
        Yes_No_Box,
        Question_Icon)
      = Yes
      then
        Change_View(Window, Flat, force => False);
        Window.Directory_List.Sort(Window.opt.column_index(Result) - 1, AZip_LV_Ex.Down);
      end if;
    end if;
  end On_Recompress;

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_SAVE_ARCHIVE_AS =>
        Window.On_Save_As;
      when IDM_CLOSE_ARCHIVE =>
        Window.Close;
      when IDM_Properties =>
        AZip_GWin.Properties(Window);
      when IDM_Select_all =>
        Full_Select(Window, True);
      when IDM_Unselect_all =>
        Full_Select(Window, False);
      when IDM_EXTRACT =>
        On_Extract (Window, dropped => False);
      when IDM_ADD_FILES =>
        On_Add_files(Window, encrypted => False);
      when IDM_Add_Files_Encryption =>
        On_Add_files(Window, encrypted => True);
      when IDM_Add_Folder =>
        On_Add_folder(Window, encrypted => False);
      when IDM_Add_Folder_Encryption =>
        On_Add_folder(Window, encrypted => True);
      when IDM_Delete_selected =>
        On_Delete(Window);
      when IDM_TEST_ARCHIVE =>
        On_Test(Window);
      when IDM_UPDATE_ARCHIVE =>
        On_Update(Window);
      when IDM_RECOMPRESS_ARCHIVE =>
        On_Recompress(Window);
      when IDM_FIND_IN_ARCHIVE =>
        On_Find(Window);
      when IDM_FLAT_VIEW =>
        Change_View (Window, Flat, force => False);
      when IDM_TREE_VIEW =>
        Change_View (Window, Tree, force => False);
      when IDM_Toggle_Flat_Tree_View =>
        case Window.opt.view_mode is
          when Flat =>
            Change_View (Window, Tree, force => False);
          when Tree =>
            Change_View (Window, Flat, force => False);
        end case;
      when IDM_No_sorting =>
        Message_Box (Window,
          "No sorting",
          "The lists of newly opened archive windows won't be sorted" & NL &
          "until both following conditions are met (in any order):" & NL &
          " - you sort again some column" & NL &
          " - you restart AZip." & NL & NL &
          "Sorting can be slow on archives with many entries.",
          Icon => Information_Icon);
        Window.MDI_Root.opt.sort_column := AZip_Common.User_options.no_sorting;
        Window.MDI_Root.remember_sorting := False;
      when IDM_Select_columns =>
        --  Propagate current child window settings to main options.
        Set_all_column_widths_to_options (Window);
        Window.MDI_Root.opt.view_mode:= Window.opt.view_mode;
        --  Start dialog.
        Select_columns_dialog (Window.MDI_Root.all);
      when IDM_Up_one_level =>
        Check_path (Window, go_up => True);
      when IDM_Context_menu_key =>
        --  We capture the "context menu key", which has the code VK_APPS (and *not*
        --  VK_MENU, VK_LMENU, VK_RMENU, VK_CONTEXTMENU) through an accelerator which
        --  activates IDM_Context_menu_key defined by us.
        --  NB: WM_CONTEXTMENU (documented on Windows Dev Center -
        --  https://docs.microsoft.com/en-us/windows/win32/menurc/wm-contextmenu )
        --  doesn't work, see commented out code in AZip_GWin.Directory_Lists.
        --
        --  Message_Box ("Key", "Context menu key via IDM_Context_menu_key");
        --
        if Folder_Focus (Window) then
          Immediate_Popup_Menu (Window.context_menu_folder, Window);
        elsif Window.Focus = Window.Directory_List'Unrestricted_Access then
          Immediate_Popup_Menu (Window.context_menu_file, Window);
        end if;
      when others =>
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  overriding procedure On_Focus (Window : in out MDI_Child_Type) is
  begin
    Update_display(Window, toolbar_and_menu);
  end On_Focus;

  overriding procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean) is
    sd: AZip_LV_Ex.Sort_Direction_Type;
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
          when Cancel => Window.MDI_Root.Success_in_enumerated_close:= False;
                         Can_Close:= False;
                         exit;
          when others => null;
        end case;
      end loop;
    end if;
    if Can_Close then
      --  Memorize column widths
      Set_all_column_widths_to_options (Window);
      --
      if Window.MDI_Root.remember_sorting then
        Window.Directory_List.Sort_Info (
          Window.MDI_Root.opt.sort_column,  --  Get column
          sd                                --  Get direction
        );
        --  We pass the Up/Down direction from the GWindows type to ours.
        Window.MDI_Root.opt.sort_direction:=
          AZip_Common.User_options.Sort_Direction_Type'Value(
             AZip_LV_Ex.Sort_Direction_Type'Image(sd)
          );
      end if;
      -- Pass view mode and the tree width portion to parent,
      -- this will memorize choice of last closed window.
      Window.MDI_Root.opt.view_mode:= Window.opt.view_mode;
      Memorize_splitter(Window);
      Window.MDI_Root.opt.tree_portion:= Window.opt.tree_portion;
      --  For the case there is no more child window, disable toolbar items.
      --  This action is reversed as soon as another child window is focused.
      Window.MDI_Root.Tool_Bar.Enabled(IDM_ADD_FILES, False);
      Window.MDI_Root.Tool_Bar.Enabled(IDM_Add_Files_Encryption, False);
      Window.MDI_Root.Tool_Bar.Enabled(IDM_Toggle_Flat_Tree_View, False);
      Window.MDI_Root.Tool_Bar.Enabled(IDM_Properties, False);
      Window.is_closing:= True;
    end if;
  end On_Close;

  procedure On_Mouse_Move (
        Window : in out MDI_Child_Type;
        X      : in     Integer;
        Y      : in     Integer;
        Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
    A : constant GWindows.Types.Point_Type :=
      Point_To_Desktop (Window, (X, Y));
  begin
    if Window.MDI_Root.dragging.is_dragging then
      declare
        expl_path: constant GString := Explorer_Path_At_Location (A.X, A.Y);
      begin
        if expl_path = "" then
          Set_Cursor (Window.MDI_Root.dragging.cursor_drag_no_way);
          Window.MDI_Root.dragging.destination := to_nowhere;
        else
          Set_Cursor (Window.MDI_Root.dragging.cursor_drag_unpack);
          if Is_Desktop_At_Location (A.X, A.Y) then
            Window.MDI_Root.dragging.destination := to_desktop;
          else
            Window.MDI_Root.dragging.destination := to_explorer;
            Window.MDI_Root.dragging.destination_path := G2GU (Explorer_Path_At_Location (A.X, A.Y));
          end if;
        end if;
      end;
      Window.Update_status_bar;
    end if;
  end On_Mouse_Move;

  procedure On_Left_Mouse_Button_Up (
        Window : in out MDI_Child_Type;
        X      : in     Integer;
        Y      : in     Integer;
        Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
    A : constant GWindows.Types.Point_Type :=
      Point_To_Desktop (Window, (X, Y));
  begin
    if Window.MDI_Root.dragging.is_dragging then
      Set_Cursor (Window.MDI_Root.dragging.cursor_arrow);
      Release_Mouse;
      case Window.MDI_Root.dragging.destination is
        when to_desktop | to_explorer =>
          On_Extract (Window, dropped => True, drop_X => A.X, drop_Y => A.Y);
        when others =>
          null;
      end case;
      Window.MDI_Root.dragging.is_dragging := False;
      Window.Update_status_bar;
    end if;
  end On_Left_Mouse_Button_Up;

  package body Daemons is
    ---------------------------------------------------------------------------
    -- This background task displays on demand (also from another task)      --
    -- informations about archive and various status.                        --
    ---------------------------------------------------------------------------

    task body Status_display is
      current_child_window: MDI_Child_Access;
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Display(w: MDI_Child_Access) do
            current_child_window:= w;
          end Display;
          Update_display(current_child_window.all, status_bar);
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
      current_child_window: MDI_Child_Access;
      pragma Unreferenced (current_child_window);
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Test(w: MDI_Child_Access) do
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
