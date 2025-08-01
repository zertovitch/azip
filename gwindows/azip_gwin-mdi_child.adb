with AZip_Common,
     AZip_GWin.Columns,
     AZip_GWin.Dragging,
     AZip_GWin.Drop_File_Dialog,
     AZip_GWin.Installation,
     AZip_GWin.Modal_Dialogs,
     AZip_GWin.Password_Dialogs,
     AZip_GWin.Properties,
     AZip_GWin.Tabs;

with UnZip,
     Zip_Streams;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Colors,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Cursors,
     GWindows.Message_Boxes,
     GWindows.Static_Controls,
     GWindows.Taskbar;

with Ada.Calendar,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Exceptions,
     Ada.IO_Exceptions,
     Ada.Sequential_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Wide_Fixed,
     Ada.Strings.Wide_Unbounded,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation;

with Interfaces;

with Set_Modification_Time_GNAT;
with Zip_time_display;
with Ada.Text_IO;

package body AZip_GWin.MDI_Child is

  use AZip_Common, AZip_Common.Operations, AZip_Common.User_options;

  use AZip_GWin.Columns, AZip_GWin.Directory_Lists,
      AZip_GWin.Dragging, AZip_GWin.MDI_Main, AZip_GWin.Password_Dialogs;

  use AZip_Resource_GUI;

  use Zip;

  use GWindows.Application, GWindows.Base, GWindows.Common_Controls,
      GWindows.Common_Dialogs, GWindows.Menus, GWindows.Message_Boxes,
      GWindows.Windows;

  use Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Wide_Unbounded;

  function Folder_Focus (Window : in MDI_Child_Type) return Boolean is
  begin
    return
      Window.opt.view_mode = Tree and then
      Window.Focus = Window.Folder_Tree'Unrestricted_Access;
  end Folder_Focus;

  procedure Update_status_bar (Window : in out MDI_Child_Type) is
    function Destination_image return GString is
    begin
      case Window.mdi_root.dragging.destination is
        when to_desktop =>
          return "to the Desktop";
        when to_explorer =>
          return "to Windows Explorer : " &
                 GU2G (Window.mdi_root.dragging.destination_path);
        when others =>
          return "";
      end case;
    end Destination_image;
    sel : Natural;
    is_folder_focused : Boolean;
  begin
    if not Is_loaded (Window.zif) then
      Window.Status_Bar.Text ("No archive loaded", 0, Flat);
      return;
    end if;
    --  Here the non-trivial cases.
    is_folder_focused := Folder_Focus (Window);
    if not is_folder_focused then
      sel := Window.Directory_List.Selected_Item_Count;
    end if;
    if Window.mdi_root.dragging.is_dragging then
      --  Here are the dragging-from-tree or dragging-from-list cases.
      if is_folder_focused then
        Window.Status_Bar.Text (
          "Dragging a folder " & Destination_image, 0, Sunken
         );
      else
        Window.Status_Bar.Text (
          "Dragging " & Integer'Wide_Image (sel) &
          " selected file(s) " & Destination_image, 0, Sunken
         );
      end if;
    else
      --  Cases without dragging
      if is_folder_focused then
        Text (Window.Status_Bar, "Folder selected", 0, Flat);
      elsif sel > 0 then
        Window.Status_Bar.Text (
          Integer'Wide_Image (Window.Directory_List.Item_Count) &
            " file(s)," & Integer'Wide_Image (sel) & " selected", 0, Flat
         );
      else
        Window.Status_Bar.Text (
           Integer'Wide_Image (Window.Directory_List.Item_Count) &
            " file(s), none selected", 0, Flat
         );
      end if;
    end if;
  end Update_status_bar;

  procedure Update_tool_bar_and_menus (Window : in out MDI_Child_Type) is
    not_empty_archive : constant Boolean :=
      Is_loaded (Window.zif) and then Entries (Window.zif) > 0;
    is_folder_focused : Boolean;
    is_any_to_delete : Boolean;
    sel : Natural;
    bar : Office_Applications.Classic_Main_Tool_Bar_Type renames Window.mdi_root.Tool_Bar;
  begin
    is_folder_focused := Folder_Focus (Window);
    if not is_folder_focused then
      sel := Window.Directory_List.Selected_Item_Count;
    end if;
    --
    bar.Enabled (IDM_EXTRACT, not_empty_archive);
    State (Window.Menu.Main,           Command, IDM_EXTRACT, bool_to_state (not_empty_archive));
    State (Window.context_menu_file,   Command, IDM_EXTRACT, bool_to_state (not_empty_archive));
    State (Window.context_menu_folder, Command, IDM_EXTRACT, bool_to_state (not_empty_archive));
    --
    is_any_to_delete := not_empty_archive and then (is_folder_focused or else sel > 0);
    bar.Enabled (IDM_Delete_selected, is_any_to_delete);
    State (Window.Menu.Main,           Command, IDM_Delete_selected, bool_to_state (is_any_to_delete));
    State (Window.context_menu_file,   Command, IDM_Delete_selected, bool_to_state (is_any_to_delete));
    State (Window.context_menu_folder, Command, IDM_Delete_selected, bool_to_state (is_any_to_delete));
    --
    bar.Enabled (IDM_FIND_IN_ARCHIVE,    not_empty_archive);
    bar.Enabled (IDM_TEST_ARCHIVE,       not_empty_archive);
    bar.Enabled (IDM_UPDATE_ARCHIVE,     not_empty_archive);
    bar.Enabled (IDM_RECOMPRESS_ARCHIVE, not_empty_archive);
    --
    State (Window.Menu.Main, Command, IDM_FIND_IN_ARCHIVE,        bool_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_TEST_ARCHIVE,           bool_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_UPDATE_ARCHIVE,         bool_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_RECOMPRESS_ARCHIVE,     bool_to_state (not_empty_archive));
    State (Window.Menu.Main, Command, IDM_Open_Containing_Folder, bool_to_state (Length (Window.ID.file_name) > 0));
    --
    if not Window.is_closing then
      --  Reactivate buttons that might have been disabled upon
      --  closing of another window. These buttons are valid even
      --  on an undefined (new) or existing, but empty, archive.
      bar.Enabled (IDM_ADD_FILES, True);
      bar.Enabled (IDM_Add_Files_Encryption, True);
      bar.Enabled (IDM_Toggle_Flat_Tree_View, True);
      bar.Enabled (IDM_Properties, True);
    end if;
  end Update_tool_bar_and_menus;

  procedure Sort_Again (Window : in out MDI_Child_Type) is
  begin
    Window.Directory_List.Sort
      (Window.mdi_root.opt.sort_column,
       AZip_LV_Ex.Sort_Direction_Type'Value
         (Window.mdi_root.opt.sort_direction'Image));
  end Sort_Again;

  procedure Attempt_Remember_Sorting (Window : in out MDI_Child_Type) is
    sd : AZip_LV_Ex.Sort_Direction_Type;
  begin
    if Window.mdi_root.remember_sorting then
      Window.Directory_List.Sort_Info
        (Window.mdi_root.opt.sort_column,  --  Get sorting column
         sd);                              --  Get sorting direction
      --  Forget the sorting column if it is the Result column.
      --  Reason: there is no result on reopening a new archive.
      if Window.mdi_root.opt.sort_column = Window.opt.column_index (Result) - 1 then
        Window.mdi_root.opt.sort_column := AZip_Common.User_options.no_sorting;
      end if;
      --  We pass the Up/Down direction from the GWindows type to ours.
      Window.mdi_root.opt.sort_direction :=
        AZip_Common.User_options.Sort_Direction_Type'Value (sd'Image);
    end if;
  end Attempt_Remember_Sorting;

  procedure Update_Information
    (Window : in out MDI_Child_Type;
     need   :        Update_need)
  is

    cidx : Column_Integer_Array renames Window.opt.column_index;

    procedure Clear_List_and_Define_Columns is
      Lst : Directory_list_type renames Window.Directory_List;
      --
    begin
      case need is
        when first_display | archive_changed | node_selected =>
          Lst.Clear;
        when results_refresh | status_bar | toolbar_and_menu =>
          null;
      end case;
      --
      for topic in Entry_topic loop
        case need is
          when first_display =>
            Lst.Insert_Column (  --  Insert new column
              Image (topic),
              cidx (topic) - 1,
              Get_column_width_from_main_options (Window, topic)
            );
          when archive_changed | node_selected =>
            Lst.Set_Column (     --  Change existing column's properties
              Image (topic),
              cidx (topic) - 1,
              Get_column_width_from_main_options (Window, topic)
            );
          when results_refresh | status_bar | toolbar_and_menu =>
            null;
        end case;
      end loop;
    end Clear_List_and_Define_Columns;

    --  Window.zif is assumed to be loaded
    --
    procedure Feed_directory_list (prefix_path : GString) is
      row, last_row : Integer := -1;
      Lst : Directory_list_type renames Window.Directory_List;
      max_entries : constant Natural := Entries (Window.zif);
      --  This includes potential invisible entries (directory names from Info-Zip, WinZip)
      result_code : array (0 .. max_entries - 1) of Integer;
      --
      procedure Process_Row
        (name_8_bit        : String; -- 'name' is compressed entry's name, with Zip encoding
         file_index        : Zip_Streams.ZS_Index_Type;
         entry_comp_size   : Zip_64_Data_Size_Type;
         entry_uncomp_size : Zip_64_Data_Size_Type;
         crc_32            : Interfaces.Unsigned_32;
         entry_date_time   : Zip_Streams.Time;
         method            : PKZip_method;
         name_encoding     : Zip_Name_Encoding;
         read_only         : Boolean;
         encrypted_2_x     : Boolean; -- PKZip 2.x encryption
         entry_user_code   : in out Integer)
      is
        pragma Unreferenced (file_index);
        name : constant UTF_16_String := To_UTF_16 (name_8_bit, name_encoding);
        simple_name_idx : Positive := name'First;
        previous_idx    : Positive := name'First;
        extension_idx   : Positive := name'Last + 1;
        R_mark : constant array (Boolean) of Character := (' ', 'R');
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
        w_node, w_parent : Tree_Item_Node;
        compression_ratio : Long_Float;
        use type Zip_64_Data_Size_Type;
      begin  --  Process_row
        Scan_for_path :
        for i in name'Range loop
          case name (i) is
            when '/' | '\' =>
              --  Directory separator, ok with Unicode UTF-8 names
              previous_idx := simple_name_idx;
              simple_name_idx := i + 1;
              --
              --  Feed eventual folder tree
              --
              if Window.opt.view_mode = Tree and need in first_display .. archive_changed then
                declare
                  --  If name is "zip-ada/zip_lib/zip.ads" and i = 16, the partial_path
                  --  will be "zip-ada/zip_lib".
                  partial_path : UTF_16_String renames name (name'First .. i - 1);
                begin
                  if not Window.path_map.Contains (partial_path) then
                    if previous_idx = name'First then
                      w_parent := Tree_Item_Node (Window.path_map.Element (root_key));
                    else
                      w_parent := Tree_Item_Node (Window.path_map.Element (partial_path (name'First .. previous_idx - 2)));
                    end if;
                    declare
                      --  From the above example, folder_name will be "zip_lib".
                      --  The folder named "zip-ada" will have been inserted previously to
                      --  the root node, when i=8.
                      folder_name : UTF_16_String renames partial_path (previous_idx .. i - 1);
                    begin
                      Window.Folder_Tree.Insert_Item (folder_name, w_parent, w_node);
                    end;
                    Window.Folder_Tree.Set_Image (w_node, 2, 3);
                    Window.path_map.Insert (partial_path, AZip_Common.Node_ID_Type (w_node));
                    Window.node_map.Insert (AZip_Common.Node_ID_Type (w_node), partial_path);
                  end if;
                end;
              end if;
            when '.' =>
              --  Last dot is the extension separator
              extension_idx := i + 1;
            when others =>
              null;
          end case;
        end loop Scan_for_path;
        --
        if simple_name_idx > name'First then
          Window.any_path_in_zip := True;
        end if;
        if simple_name_idx > name'Last then -- skip directory entries (names end with '/' or '\')
          return;
        end if;
        if Window.opt.view_mode = Tree and then prefix_path /= name (name'First .. simple_name_idx - 2) then
          return; -- not in a part of the tree to be displayed
        end if;
        if extension_idx < simple_name_idx then
          --  last dot was in a directory name (like: .svn/entries)
          extension_idx := name'Last + 1; -- will be empty
        end if;
        row := row + 1;
        if need in first_display .. node_selected then
          declare
            the_simple_name : UTF_16_String renames name (simple_name_idx .. name'Last);
            the_ext         : UTF_16_String renames name (extension_idx .. name'Last);
            the_path        : UTF_16_String renames name (name'First .. simple_name_idx - 1);
          begin
            Lst.Insert_Item (the_simple_name & Encryption_suffix, row);
            if entry_uncomp_size > 0 then
              compression_ratio := Long_Float (entry_comp_size) / Long_Float (entry_uncomp_size);
            else
              compression_ratio := 0.0;
            end if;
            --
            --  Payload
            --
            Lst.Item_Data (
              row,
              new LV_Payload'(
                index_before_sorting => row,
                name                 => To_Unbounded_Wide_String (the_simple_name),
                extension            => To_Unbounded_Wide_String (the_ext),
                path                 => To_Unbounded_Wide_String (the_path),
                uncompressed_size    => Interfaces.Integer_64 (entry_uncomp_size),
                compressed_size      => Interfaces.Integer_64 (entry_comp_size),
                crc_32               => crc_32,
                date_time            => entry_date_time,
                format               => method,
                name_encoding        => name_encoding,
                read_only            => read_only,
                ratio                => compression_ratio,
                result_code          => entry_user_code  --  Updated elsewhere if need = results_refresh
              )
            );
            --
            Lst.Set_Sub_Item (the_ext, row, cidx (FType) - 1);
            begin
              Lst.Set_Sub_Item (S2G (Zip_time_display (entry_date_time)), row, cidx (Modified) - 1);
            exception
              when Zip_Streams.Calendar.Time_Error =>
                Lst.Set_Sub_Item ("(invalid)", row, cidx (Modified) - 1);
            end;
            if read_only then  --  Any attribute
              Lst.Set_Sub_Item (S2G ((1 => R_mark (read_only))), row, cidx (Attributes) - 1);
            end if;
            Lst.Set_Sub_Item (File_Size_Image (entry_uncomp_size), row, cidx (Size) - 1);
            Lst.Set_Sub_Item (File_Size_Image (entry_comp_size), row, cidx (Packed) - 1);
            Lst.Set_Sub_Item (Ratio_pct_Image (entry_comp_size, entry_uncomp_size), row, cidx (Ratio) - 1);
            Lst.Set_Sub_Item (S2G (Zip.Image (method)), row, cidx (Format) - 1);
            Lst.Set_Sub_Item (Hexadecimal_32 (crc_32), row, cidx (CRC32) - 1);
            if simple_name_idx > name'First then
              Lst.Set_Sub_Item (the_path, row, cidx (Path) - 1);
            end if;
            Lst.Set_Sub_Item (Zip_Name_Encoding'Wide_Image (name_encoding), row, cidx (Encoding) - 1);
          end;
          --
          --  Show some response if the zip directory is very large
          --
          if row mod 2048 = 0 then
            Message_Check;
          end if;
        end if;
        result_code (row) := entry_user_code;
      end Process_Row;

      procedure Traverse is new Zip.Traverse_verbose (Process_Row);

      procedure Update_Results_Column is
        az_color : AZip_Common.Operations.RGB_Type;
        gw_color : GWindows.Colors.RGB_Type;
        intensity : Float;
        use GWindows.Colors;
        font_color : Color_Type;
        unsorted_index : Integer;
        da : AZip_LV_Ex.Data_Access;
        use type AZip_LV_Ex.Data_Access;
      begin
        last_row := row;
        for sorted_index in 0 .. last_row loop
          da := Lst.Item_Data (sorted_index);
          --  if da = null then
          --    Ada.Text_IO.Put_Line ("NULL!!");
          --  else
          unsorted_index := da.index_before_sorting;
          --  Ada.Text_IO.put_line (G2S(Lst.Text(sorted_index,0)) &
          --    da.uncompressed_size'image);
          Lst.Set_Sub_Item
            (S2G (Result_Message
             (Window.last_operation, result_code (unsorted_index))),
             sorted_index, cidx (Result) - 1);
          Result_Color
            (Window.last_operation, result_code (unsorted_index), Window.last_max_code, az_color, intensity);
          da.result_code := result_code (unsorted_index);
          if need = results_refresh or az_color /= AZip_Common.Operations.white then
            --  Ensure user can read the text, given the background color.
            if intensity > 0.58 then
              font_color := Black;
            else
              font_color := GWindows.Colors.White;
            end if;
            gw_color :=
              (Red    => GWindows.Colors.Color_Range (az_color.Red),
               Green  => GWindows.Colors.Color_Range (az_color.Green),
               Blue   => GWindows.Colors.Color_Range (az_color.Blue),
               Unused => 0
              );
            Lst.Subitem_Color (font_color, To_Color (gw_color), sorted_index, cidx (Result) - 1);
          end if;
          --  Show some response if the zip directory is very large
          --
          if sorted_index mod 2048 = 0 then
            Message_Check;
          end if;
          --  end if;
        end loop;
      end Update_Results_Column;

      use Ada.Calendar;
      t0, t1, t2 : Ada.Calendar.Time;
    begin  --  Feed_directory_list
      if need > results_refresh then
        return;
      end if;
      Window.Directory_List.refreshing := True;
      if need in first_display .. node_selected then
        --  This will be set to True if there is any path during the listing
        Window.any_path_in_zip := False;
      end if;
      --  Performance is meant to be better with the All_Items mode.
      Window.Directory_List.Color_Mode (AZip_LV_Ex.All_Items);
      if timing then
        t0 := Clock;
      end if;
      --
      --  List is entirely filled on next statement:
      --
      Traverse (Window.zif);
      if timing then
        t1 := Clock;
      end if;
      Update_Results_Column;
      if timing then
        t2 := Clock;
        Window.Status_Bar.Text
          ("List filling:" & Duration'Wide_Image (t1 - t0) &
           "; Results column:" & Duration'Wide_Image (t2 - t1));
      end if;
      Window.Directory_List.Color_Mode (AZip_LV_Ex.Subitem);
      Window.Directory_List.refreshing := False;
    end Feed_directory_list;

    w_root : Tree_Item_Node;

  begin
    Clear_List_and_Define_Columns;
    case Window.opt.view_mode is
      when Flat =>
        if Is_loaded (Window.zif) then
          Feed_directory_list ("");
        end if;
        Check (Window.Menu.Main, Command, IDM_FLAT_VIEW, True);
        Check (Window.Menu.Main, Command, IDM_TREE_VIEW, False);
      when Tree =>
        if need in first_display .. archive_changed then
          Window.path_map.Clear;
          Window.node_map.Clear;
        end if;
        if Is_loaded (Window.zif) then
          if need in first_display .. archive_changed then
            Window.Folder_Tree.Set_Image_List (Window.mdi_root.Folders_Images);
            Window.Folder_Tree.Delete_Item (Window.Folder_Tree.Get_Root_Item);
            Window.Folder_Tree.Insert_Item (GU2G (Window.ID.short_name), 0, w_root, As_A_Root);
            Window.Folder_Tree.Set_Image (w_root, 0, 1);
            Window.path_map.Insert (root_key, AZip_Common.Node_ID_Type (w_root));
            Window.node_map.Insert (AZip_Common.Node_ID_Type (w_root), root_key);
            Window.selected_path := Null_GString_Unbounded;
          end if;
          Feed_directory_list (GU2G (Window.selected_path));
        end if;
        Check (Window.Menu.Main, Command, IDM_FLAT_VIEW, False);
        Check (Window.Menu.Main, Command, IDM_TREE_VIEW, True);
    end case;
    if need in first_display .. node_selected and then
      Window.mdi_root.opt.sort_column >= 0
    then
      --  Peform an initial sorting according to current options.
      Sort_Again (Window);
    end if;
    if not timing then
      Window.Update_status_bar;
    end if;
    Window.Update_tool_bar_and_menus;
  end Update_Information;

  procedure Memorize_Splitter (Window : in out MDI_Child_Type) is
  begin
    case Window.opt.view_mode is
      when Flat =>
        null; -- do nothing: the splitter is invisible and not used
      when Tree =>
        Window.opt.tree_portion :=
          Float (Window.Folder_Tree.Width) / Float (Window.Client_Area_Width);
    end case;
  end Memorize_Splitter;

  procedure Check_Path (Window : in out MDI_Child_Type; go_up : Boolean) is
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
            curs := Window.path_map.Find (GU2G (Window.selected_path));
            if
                --  The selected path doesn't exist anymore. We'll try again by going one
                --  folder up. This is done by truncating the last folder name from the right.
                curs = No_Element
              or else
                --  Here, we want to go up (at least) one folder.
                (go_up and not go_up_done)
            then
              idx := Integer'Max (
                Index (Window.selected_path, "/", Backward),  --  Regular Zip (and Unix) separator.
                Index (Window.selected_path, "\", Backward)   --  Wrong, but may happen.
              );
              if idx = 0 then
                --  We are at the root.
                Window.selected_path := Null_GString_Unbounded;
              else
                --  Truncation to a non-empty string was successful, e.g.
                --  "zip-ada/zip_lib" becomes "zip-ada".
                Window.selected_path := Unbounded_Slice (Window.selected_path, 1, idx - 1);
              end if;
              go_up_done := True;
            else
              sel_node := Tree_Item_Node (Element (curs));
              exit;
            end if;
          end loop;
          Window.Folder_Tree.Select_Item (sel_node);
          Update_Information (Window, node_selected);
          Window.Folder_Tree.Expand (sel_node);
          Window.Folder_Tree.Focus;
        end if;
    end case;
  end Check_Path;

  procedure Change_View
    (Window   : in out MDI_Child_Type;
     new_view :        View_Mode_Type;
     force    :        Boolean)
  is
    mem_sel_path : constant GString_Unbounded := Window.selected_path;
  begin
    if Window.opt.view_mode = new_view and not force then
      return;
    end if;
    Window.opt.view_mode := new_view;
    case new_view is
      when Flat =>
        if not force then
          Memorize_Splitter (Window);
          --  Remember tree portion for user persistence or
          --  for next time we toggle back to tree view.
        end if;
        Window.Splitter.Hide;
        Window.Folder_Tree.Hide;
      when Tree =>
        Window.Splitter.Show;
        Window.Folder_Tree.Show;
    end case;
    Window.On_Size (Window.Width, Window.Height);
    Update_Information (Window, archive_changed);
    Window.selected_path := mem_sel_path;
    Check_Path (Window, go_up => False);
  end Change_View;

  overriding procedure On_Create (Window : in out MDI_Child_GSize_Bar_Type) is
  begin
    Set_Dashes (Window             => Window,
                Dash_Height        => 2,
                Dash_Width         => 2,
                Spacing_Height     => 14,
                Spacing_Width      => 0,
                Number_Of_Dashes_V => 1000,
                Number_Of_Dashes_H => 1);
  end On_Create;

  overriding procedure On_Bar_Moved (Window : in out MDI_Child_GSize_Bar_Type) is
    ch_w : MDI_Child_Type
      renames MDI_Child_Type (Window.Parent.Parent.Parent.all);
    threshold_small : constant := 10;
    threshold_large : constant := 50;
  begin
    pragma Assert (threshold_small < threshold_large);
    if ch_w.Folder_Tree.Width < threshold_small then
      --  User seems to want to hide the tree - we do it right.
      Change_View (ch_w, Flat, force => True);
    elsif ch_w.Folder_Tree.Width < threshold_large then
      --  Don't memorize. Absent this case, the tree will be hidden again
      --  each time we try to enlarge its width with the size bar.
      null;
    else
      Memorize_Splitter (ch_w);
    end if;
    --  Call parent method:
    GWindows.GControls.GSize_Bars.GSize_Bar_Type (Window).On_Bar_Moved;
  end On_Bar_Moved;

  overriding procedure On_Left_Mouse_Button_Double_Click
     (Window : in out MDI_Child_GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
  is
    ch_w : MDI_Child_Type
      renames MDI_Child_Type (Window.Parent.Parent.Parent.all);
  begin
    --  User wants to hide the tree.
    Change_View (ch_w, Flat, force => True);
  end On_Left_Mouse_Button_Double_Click;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
    use GWindows.Packing_Boxes;
  begin
    Window.Small_Icon ("Box_Closed_Icon_Name");

    --  Filial feelings:
    Window.mdi_root := MDI_Main_Access (Controlling_Parent (Window));
    --  We copy options to child level:
    Window.opt := Window.mdi_root.opt;

    Window.Tree_Bar_and_List.Create (Window, Direction => Horizontal);
    Window.Tree_Bar_and_List.Dock (At_Top);

    Window.Folder_Tree.Create (
      Window.Tree_Bar_and_List,
      1, 1, 20, 20,
      Lines_At_Root => False
    );
    Window.Folder_Tree.Dock (Fill);

    --  Sliding panel with split bar and list on it:
    Window.Bar_and_List.Create (Window.Tree_Bar_and_List, 1, 1, 20, 20);
    Window.Bar_and_List.Dock (At_Right);
    --  The split bar:
    Window.Splitter.Create (Window.Bar_and_List, At_Left);
    --  The list:
    Window.Directory_List.Create (Window.Bar_and_List, 50, 1, 20, 20, Multiple, Report_View, Sort_Custom);
    Window.Directory_List.Set_Extended_Style (Full_Row_Select);
    Window.Directory_List.Color_Mode (AZip_LV_Ex.Subitem);
    Window.Directory_List.Dock (Fill);

    Window.Status_Bar.Create (Window, "No archive");
    Window.Status_Bar.Parts ((0 => 600, 1 => -1));
    Window.Status_Bar.Dock (At_Bottom);

    Window.Dock_Children;
    if Window.opt.view_mode = Tree then
      Change_View (Window, Tree, force => True);
    end if;

    ------------
    --  Menus --
    ------------

    AZip_Resource_GUI.Create_Full_Menu (Window.Menu);
    Window.MDI_Menu (Window.Menu.Main, Window_Menu => 6);
    Append_Item (Window.context_menu_file, "&Extract file(s)", IDM_EXTRACT);
    Append_Item (Window.context_menu_file, "&Delete file(s)",  IDM_Delete_selected);
    Append_Item (Window.context_menu_folder, "&Extract folder", IDM_EXTRACT);
    Append_Item (Window.context_menu_folder, "&Delete folder",  IDM_Delete_selected);

    --  Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children : constant Boolean :=
        not Window.mdi_root.opt.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Window.mdi_root.Freeze;
        Window.Zoom;
      end if;
      On_Size (Window, Width (Window), Height (Window));
      if memo_unmaximized_children then
        Window.mdi_root.Thaw; -- Before Zoom, otherwise uncomplete draw.
        Window.Zoom (False);
        Window.mdi_root.Tool_Bar.Redraw;
      end if;
    end;
    Window.Status_deamon.Start;
    Window.Update_Information (first_display);
    Window.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset (Window.temp_name_gen);
  end On_Create;

  procedure Create_AZip_MDI_Child
    (Window : in out MDI_Child_Type;
     Parent : in out MDI_Main.MDI_Main_Type;
     ID     : in     ID_Type)
  is
    procedure Append_Tab is
      title : constant GString := GU2G (Window.ID.short_name);
    begin
      Parent.tab_bar.Insert_Tab (Parent.tab_bar.Tab_Count, Simple_Name (title));
      Parent.tab_bar.Selected_Tab (Parent.tab_bar.Tab_Count - 1);
      Parent.tab_bar.info.Append ((Window.ID, Window'Unrestricted_Access));
    end Append_Tab;
  begin
    Window.ID := ID;
    Create_MDI_Child (Window, Parent, GU2G (ID.short_name), Is_Dynamic => True);
    MDI_Active_Window (Parent, Window);
    Append_Tab;
  end Create_AZip_MDI_Child;

  procedure On_Save (Window : in out MDI_Child_Type) is
  begin
    null; -- nothing to be saved in this application
  end On_Save;

  function Is_Document_Modified (Window : in MDI_Child_Type) return Boolean is
    pragma Unreferenced (Window);
  begin
    return False;
  end Is_Document_Modified;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Child_Type)
  is
    New_File_Name          : GWindows.GString_Unbounded;
    File_Title             : GWindows.GString_Unbounded;
    user_choice_successful : Boolean;
    new_ID : ID_Type;
    tab_bar : Tabs.AZip_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    --
    --  If needed, an empty Zip file is created with the contents below.
    --
    package CIO is new Ada.Sequential_IO (Character);
    use CIO;
    empty_zip : File_Type;
    --  This is the empty Zip archive:
    contents : constant String := "PK" & ASCII.ENQ & ASCII.ACK & 18 * ASCII.NUL;
  begin
    New_File_Name := Window.ID.file_name;
    Save_File (
      Window, "Save Zip archive as...", New_File_Name, Zip_archives_filters,
      ".zip", File_Title,
      user_choice_successful
    );
    if not user_choice_successful then
      return;
    end if;
    if Zip.Exists (To_UTF_8 (GU2G (New_File_Name))) then
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

    if Is_loaded (Window.zif) then
      begin
        Ada.Directories.Copy_File
          (To_UTF_8 (GU2G (Window.ID.file_name)),
           To_UTF_8 (GU2G (New_File_Name)),
           "encoding=utf8");
      exception
        when others =>
          Message_Box (
            Window,
            "'Save as' failed",
            "Copy of archive under new name failed.",
            OK_Box,
            Exclamation_Icon
          );
          return;
      end;
    else  --  we don't have a file yet
      Create (empty_zip, Out_File, To_UTF_8 (GU2G (New_File_Name)), "encoding=utf8");
      for i in contents'Range loop
        Write (empty_zip, contents (i));
      end loop;
      Close (empty_zip);
    end if;
    Window.Text (GU2G (File_Title));
    new_ID := (file_name => New_File_Name, short_name => File_Title);
    --  Change title in the tab bar.
    for index in 0 .. tab_bar.Tab_Count - 1 loop
      if tab_bar.info (index).ID = Window.ID then
        tab_bar.info (index).ID := new_ID;
        tab_bar.Text (index, Simple_Name (GU2G (New_File_Name)));
        exit;
      end if;
    end loop;
    Window.ID := new_ID;
    Window.last_operation := bogus_no_display;
    Window.Update_Common_Menus (GU2G (New_File_Name));
    Window.Load_Archive_Catalogue (copy_codes => False);
  end On_Save_As;

  procedure Set_Modification_Time_B (Name : in String;
                                     To   : in Ada.Calendar.Time) is
  begin
    Set_Modification_Time_GNAT (Name, To);
  exception
    when others =>
      null; -- !! utf-8 or ascii names with characters > pos 127 fail
  end Set_Modification_Time_B;

  procedure Process_Archive_GWin
    (Window         : in out MDI_Child_Type;
     operation      :        Archive_Operation;
     file_names     :        Array_Of_File_Names := Empty_Array_Of_File_Names;
     base_folder    :        GString     := "";
     search_pattern :        GString     := "";
     output_folder  :        Wide_String := "";
     option_flag_1  :        Boolean     := False;  --  recompress: brute-force; extraction: ignore directories
     option_flag_2  :        Boolean     := False;  --  recompress: back-up
     encrypt        :        Boolean     := False;
     new_temp_name  :        String      := "";
     return_code    :    out Operation_return_code)
  is
    is_aborted : Boolean := False;
    --
    procedure Abort_clicked (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      is_aborted := True;  --  Will propagate user_abort upon next Boxed_Feedback.
    end Abort_clicked;
    --
    tick : Ada.Calendar.Time;
    progress : Progress_box_Type;
    --
    procedure Boxed_Feedback
      (file_percents_done           :     Natural;
       archive_percents_done        :     Natural;
       entry_being_processed        :     GString;
       e_operation                  :     Entry_Operation;
       for_comment_1, for_comment_2 :     String;  --  e.g. #found so far, time elpased,...
       skip_hint                    :     Boolean;
       user_abort                   : out Boolean)
    is
      use Ada.Calendar, Ada.Strings.Wide_Fixed;
      now : constant Ada.Calendar.Time := Clock;
    begin
      --  Display only at most every 4/100 second (i.e. max 25 fps).
      --  Otherwise Windows may be overflown by messages and the operation
      --  takes much more time due to the display. Typical case: an archive
      --  with many small files - GWin.zip or Java run-time's Jar for instance.
      if now - tick >= 0.04 or else archive_percents_done = 100 then
        progress.File_Progress.Position (file_percents_done);
        progress.Archive_Progress.Position (archive_percents_done);
        declare
          percents_done : constant GString :=
            Trim (Integer'Wide_Image (archive_percents_done), Left) & '%';
        begin
          Window.mdi_root.Text
            (percents_done & " done - " & S2G (AZip_GWin.Installation.AZip_Title));
          if Window.mdi_root.Task_bar_gadget_ok then
            Window.mdi_root.Task_bar_gadget.Set_Progress_Value
              (Window.mdi_root.all, archive_percents_done, 100);
          end if;
          progress.Percent_Progress.Text (percents_done);
          progress.Entry_name.Text (entry_being_processed);
          progress.Entry_operation_name.Text
            (Description (e_operation, operation, skip_hint));
        end;
        progress.Comment_1.Text (S2G (for_comment_1));
        progress.Comment_2.Text (S2G (for_comment_2));
        if archive_percents_done = 100 and then for_comment_1 /= "" then
          Window.last_op_comment_1 := G2GU (S2G (for_comment_1));
          Window.last_op_comment_2 := G2GU (S2G (for_comment_2));
        end if;
        Message_Check;
        tick := now;
      end if;
      user_abort := is_aborted;
    end Boxed_Feedback;
    --
    procedure Name_conflict_resolution
      (name            :  in String;
       name_encoding   :  in Zip_Name_Encoding;
       action          : out UnZip.Name_Conflict_Intervention;
       new_name        : out String;
       new_name_length : out Natural)
    is
    pragma Unreferenced (new_name, new_name_length);
      box : File_exists_box_Type;
      use UnZip;
    begin
      box.Create_Full_Dialog (progress);
      box.Conflict_simple_name.Text (Remove_path (To_UTF_16 (name, name_encoding)));
      box.Conflict_location.Text (output_folder);
      box.Overwrite_Rename.Disable;
      --  !! ^ Needs some effort to make an idiot-proof name query ;-)
      box.Center (Window);
      case Show_Dialog (box, progress) is
        when Overwrite_Yes    =>  action := yes;
        when Overwrite_No     =>  action := no;
        when Overwrite_All    =>  action := yes_to_all;
        when Overwrite_None   =>  action := none;
        when Overwrite_Rename =>  action := rename_it;
        when others           =>  action := abort_now;
      end case;
    end Name_conflict_resolution;
    --
    procedure Get_password_decrypt_for_Common
      (encrypted_entry_name : in     GString;
       password             : in out GString_Unbounded;
       cancelled            :    out Boolean)
    is
    begin
      Get_password_for_decryption
        (Window     => Window,
         Parent     => progress,
         entry_name => encrypted_entry_name,
         password   => password,
         cancelled  => cancelled);
    end Get_password_decrypt_for_Common;
    --
    --  Instanciation of the GUI-agnostic processing
    --
    procedure Archive_Processing_GWindows is
      new AZip_Common.Operations.Process_Archive
        (Boxed_Feedback,
         Get_password_decrypt_for_Common);
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
    az_names : Name_List (file_names'Range);
    use Ada.Calendar, GWindows.Taskbar, GWin_Util;

  begin  --  Process_archive_GWin

    --  Neutral conversion: GStrings (UTF-16) to UTF_16_String
    for i in az_names'Range loop
      az_names (i).str := file_names (i);
    end loop;
    tick := Clock - 1.0;
    progress.Create_Full_Dialog (Window);
    progress.File_Progress.Position (0);
    progress.Archive_Progress.Position (0);
    if Window.mdi_root.Task_bar_gadget_ok then
      Window.mdi_root.Task_bar_gadget.Set_Progress_Value (Window.mdi_root.all, 0, 100);
    end if;
    progress.Cancel_button.Hide;
    progress.Cancel_button_permanent.Text (Cross & "   Cancel");
    progress.Cancel_button_permanent.Show;
    progress.Cancel_button_permanent.On_Click_Handler (Abort_clicked'Unrestricted_Access);
    progress.Center (Window);
    progress.Redraw;
    progress.Show;
    Window.mdi_root.Disable;
    progress.Text (progress.Text & " Operation: " & Img (operation));

    begin

      Archive_Processing_GWindows
        (zif            => Window.zif,
         operation      => operation,
         entry_name     => Expand_Folders (az_names),
         base_folder    => base_folder,
         search_pattern => search_pattern,
         output_folder  => output_folder,
         Set_Time_Stamp => Set_Modification_Time_B'Access,
         new_temp_name  => new_temp_name,
         Name_conflict  => Name_conflict_resolution'Unrestricted_Access,
         password       => Window.current_password,
         option_flag_1  => option_flag_1,
         option_flag_2  => option_flag_2,
         encrypt        => encrypt,
         max_code       => Window.last_max_code,
         return_code    => return_code);

      Window.mdi_root.Text (S2G (AZip_GWin.Installation.AZip_Title));  --  Remove progress info.
      Window.last_operation := operation;

      case return_code is
        when ok =>
          if operation in Modifying_Operation then
            Attempt_Remember_Sorting (Window);
            Window.Load_Archive_Catalogue (copy_codes => operation /= Remove);
          else
            Update_Information (Window, results_refresh);
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
        Message_Box
          (Window,
           "Processing failed",
           Msg_Name_Error &
           NL & "-----" & NL &
           S2G (Ada.Exceptions.Exception_Message (E)),
           OK_Box,
           Exclamation_Icon);

      when E : Ada.IO_Exceptions.Use_Error =>
        return_code := aborted;
        Message_Box
          (Window,
           "Processing failed",
           "Archive cannot be modified (perhaps, is it read-only ?)," & NL &
           "or a new file cannot be written." &
           NL & "-----" & NL &
           S2G (Ada.Exceptions.Exception_Message (E)),
           OK_Box,
           Exclamation_Icon);
    end;

    if Window.mdi_root.Task_bar_gadget_ok then
      Window.mdi_root.Task_bar_gadget.Set_Progress_State (Window.mdi_root.all, No_Progress);
    end if;
    Window.mdi_root.Enable;
    Window.mdi_root.Focus;
  end Process_Archive_GWin;

  function Temp_AZip_Name (Window : MDI_Child_Type) return String is
    temp_dir : constant String :=
      (if Window.mdi_root.opt.temp_directory /= ""
         and then Is_Temp_Directory_Valid (Window.mdi_root.opt)
       then
         To_UTF_8 (To_Wide_String (Window.mdi_root.opt.temp_directory))
       else
         Ada.Environment_Variables.Value ("TEMP"));
  begin
    loop
      declare
        num0 : constant String :=
          Float'Image (Ada.Numerics.Float_Random.Random (Window.temp_name_gen));
        num : constant String := num0 (num0'First + 1 .. num0'Last);
        --  ^ Skip the @#*% leading space
        test_name : constant String :=
          Ada.Directories.Compose (temp_dir, "AZip_Temp_" & num, "zip");
      begin
        if not Ada.Directories.Exists (test_name) then
          return test_name;
        end if;
      end;
    end loop;
  end Temp_AZip_Name;

  procedure Go_for_adding (
    Window     : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names;
    Encrypt    : in     Boolean
  )
  is
    function Eventual_folder return GString is
      sel_path : constant GString := GU2G (Window.selected_path);
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

    Process_Archive_GWin
      (Window        => Window,
       operation     => Add,
       file_names    => File_Names,
       base_folder   => Eventual_folder,
       encrypt       => Encrypt,
       new_temp_name => Temp_AZip_Name (Window),
       return_code   => return_code);

  end Go_for_adding;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names) is

    function Possible_Folder return GString is
      sel_path : constant GString := GU2G (Window.selected_path);
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
    end Possible_Folder;

    encrypt   : Boolean := False;
    is_yes    : Boolean;
    cancelled : Boolean;
    parent    : MDI_Main_Access;
  begin
    Window.Focus;
    if Confirm_archives_if_all_Zip_files (Window, File_Names) then
      --  All files are Zip archives (even those without .zip extension).
      --
      --  We save the parent access because this Window may be already closed
      --  since the second iteration of the loop below if Window is was a temporary
      --  MS-Office-like blank window - See procedure Close_extra_first_child.
      parent := Window.mdi_root;
      --
      for i in File_Names'Range loop
        Open_Child_Window_And_Load (parent.all, File_Names (i));
      end loop;
    else
      AZip_GWin.Drop_File_Dialog.Do_Drop_File_Dialog
        (Parent         => Window,
         archive_name   => GU2G (Window.ID.short_name) & Possible_Folder,
         new_archive    => not Is_loaded (Window.zif),
         encrypt        => encrypt,
         yes            => is_yes);
      if is_yes then
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

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus (Window    : MDI_Child_Type;
                                 top_entry : GString := "") is
  begin
    Update_Common_Menus (Window.mdi_root.all, top_entry);
  end Update_Common_Menus;

  procedure Load_Archive_Catalogue
    (Window     : in out MDI_Child_Type;
     copy_codes : in     Boolean)
  is
    new_zif : Zip_Info;
  begin
    Load_insensitive_if_possible (new_zif, To_UTF_8 (GU2G (Window.ID.file_name)));
    if Zip.Is_loaded (Window.zif) then
      if copy_codes then
        Set_user_codes (new_zif, appended);
        Copy_user_codes (Window.zif, new_zif);  --  Copy `replaced` labels from old archive's info
      end if;
    end if;
    Window.zif := new_zif;
    Change_View (Window, Window.opt.view_mode, force => True);
    --  Update_Information (Window, archive_changed); -- included in Change_View
    --  Window.Status_deamon.Display (Window'Unchecked_Access);
  end Load_Archive_Catalogue;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer)
  is
    w : constant Natural := Window.Client_Area_Width;
    h : constant Natural := Integer'Max (2, Window.Client_Area_Height - Window.Status_Bar.Height);
    splitter_w : constant := 4;  --  Space between tree and list
    tree_w : constant Integer := Integer (Window.opt.tree_portion * Float (w)) - splitter_w / 2;
    use GWindows.Types;
  begin
    if Window.mdi_root.User_maximize_restore then
      Window.mdi_root.opt.MDI_childen_maximized := Zoom (Window);
    end if;
    Window.Tree_Bar_and_List.Location (Rectangle_Type'(0, 0, w, h));
    case Window.opt.view_mode is
      when Flat =>
        Window.Folder_Tree.Location (Rectangle_Type'(0, 0, 1, h));
        Window.Bar_and_List.Location (Rectangle_Type'(0, 0, w, h));
        Window.Splitter.Location (Rectangle_Type'(0, 0, 1, h));
        Window.Directory_List.Location (Rectangle_Type'(0, 0, w, h));
      when Tree =>
        Window.Folder_Tree.Location (Rectangle_Type'(0, 0, tree_w, h));
        Window.Bar_and_List.Location (Rectangle_Type'(tree_w, 0, w, h));
        --  Splitter bar and directory list are inside the Bar_and_List panel
        Window.Splitter.Location (Rectangle_Type'(0, 0, splitter_w, h));
        Window.Directory_List.Location (Rectangle_Type'(splitter_w, 0, Window.Bar_and_List.Width, h));
    end case;
    Dock_Children (Window);
  end On_Size;

  function Get_selected_entry_list (Window : MDI_Child_Type) return Array_Of_File_Names is
    items : constant Natural := Window.Directory_List.Item_Count;
    names : Array_Of_File_Names (1 .. items);
    j : Natural := 0;
  begin
    for i in 0 .. items - 1 loop  --  0-based
      if Window.Directory_List.Is_Selected (i) then
        j := j + 1;
        names (j) := G2GU (
          Window.Directory_List.Text (i, SubItem => 9) &  --  path
          Window.Directory_List.Text (i, SubItem => 0)    --  simple name
          );
        if Element (names (j), Length (names (j))) = '*' then
          --  Remove the " *" appended on encrypted entries.
          names (j) := Unbounded_Slice (names (j), 1, Length (names (j)) - 2);
        end if;
      end if;
    end loop;
    return names (1 .. j);
  end Get_selected_entry_list;

  --  Selected folder's and subfolder's contents (Tree view)
  function Get_selected_folder_entry_list (Window : MDI_Child_Type) return Array_Of_File_Names is
    items : constant Natural := Entries (Window.zif);
    names : Array_Of_File_Names (1 .. items);
    prefix : constant GString := GU2G (Window.selected_path) & '/';
    j : Natural := 0;
    --
    procedure Process_entry (
      name_8_bit       : String; -- 'name' is compressed entry's name, with Zip encoding
      name_encoding    : Zip_Name_Encoding
    )
    is
      name : constant UTF_16_String := To_UTF_16 (name_8_bit, name_encoding);
    begin
      if name'Length >= prefix'Length and then
         name (name'First .. name'First + prefix'Length - 1) = prefix
      then
        j := j + 1;
        names (j) := G2GU (name);
      end if;
    end Process_entry;
    procedure Traverse_names is new Zip.Traverse_Unicode (Process_entry);
  begin
    Traverse_names (Window.zif);
    return names (1 .. j);
  end Get_selected_folder_entry_list;

  function Any_path_in_list (names : Array_Of_File_Names) return Boolean is
  begin
    for i in names'Range loop
      if Index (names (i), "/") > 0 or else Index (names (i), "\") > 0 then
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
    list : constant Array_Of_File_Names := Smart_list;
    --
    function Archive_extract_msg return GString is
    begin
      if is_folder_focused then
        return "Extract current folder's contents";
      elsif sel_list'Length > 0 then
        return "Extract the" & Integer'Wide_Image (sel_list'Length) & " selected item(s)";
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
    if not Is_loaded (Window.zif) then
      return;  --  No archive, then nothing to do
    end if;
    if dropped then
      dir := G2GU (Explorer_Path_At_Location (drop_X, drop_Y));
      if ((list'Length > 4 and then Is_Desktop_At_Location (drop_X, drop_Y))
          or else
          list'Length > 20
         )
        and then (not is_folder_focused)  --  Bunch of individual files, not a whole folder
        and then Message_Box (
          Window, Archive_extract_msg,
          "You are dropping a large amount of files. Continue?",
          Yes_No_Box, Question_Icon) = No
      then
        return;
      end if;
    else
      declare
        initial_path : constant GString := GU2G (Window.extract_dir);
        title        : constant GString := Archive_extract_msg & " to...";
      begin
        dir := G2GU
          (Get_Directory
            (Window       => Window,
             Dialog_Title => title,
             Initial_Path => initial_path));
      end;
    end if;
    if dir = "" then
      return;
    end if;
    Window.extract_dir := dir;
    if list'Length > 0 then
      --  Bunch of individual files, or a non-empty folder
      ask := Any_path_in_list (list);
    else
      --  Entire archive
      ask := Window.any_path_in_zip;
    end if;
    if ask then
      --  We have folders in some of the file names.
      if Window.opt.ignore_extract_path then
        box_kind := Yes_No_Def_Cancel_Box;  --  Previous answer was "No", so we take "No" as default
      else
        box_kind := Yes_No_Cancel_Box;
      end if;
      case Message_Box (
        Window, Archive_extract_msg,
        Use_path_question, box_kind, Question_Icon)
      is
        when No =>
          Window.opt.ignore_extract_path := True;
        when Yes =>
          Window.opt.ignore_extract_path := False;
        when others =>
          return;
      end case;
    end if;

    Process_Archive_GWin
      (Window        => Window,
       operation     => Extract,
       file_names    => list,
       output_folder => GU2G (dir),
       option_flag_1 => Window.opt.ignore_extract_path,
       return_code   => return_code);

    if return_code = ok and not dropped then
      --  Open destination path's folder
      GWin_Util.Start (G2S (GU2G (dir)));  --  !! not unicode
    end if;
  end On_Extract;

  procedure On_Delete (Window : in out MDI_Child_Type) is
    sel_list : constant Array_Of_File_Names := Get_selected_entry_list (Window);
    is_folder_focused : constant Boolean := Folder_Focus (Window);
    --
    function Smart_list return Array_Of_File_Names is
    begin
      if is_folder_focused then
        return Get_selected_folder_entry_list (Window);
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
        return "Do you want to remove the" & Integer'Wide_Image (sel_list'Length) &
          " selected item(s) ?";
      end if;
    end Delete_msg;
    return_code : Operation_return_code;
  begin
    if Window.Directory_List.Selected_Item_Count = 0 and not is_folder_focused then
      return; -- no item, no folder -> do nothing (different from On_Extract's behaviour)
    end if;

    if Message_Box (Window, "Delete", Delete_msg, Yes_No_Box, Question_Icon) = Yes then

      Process_Archive_GWin
        (Window        => Window,
         operation     => Remove,
         file_names    => Smart_list,
         new_temp_name => Temp_AZip_Name (Window),
         return_code   => return_code);

    end if;
  end On_Delete;

  procedure On_Add_Files (Window : in out MDI_Child_Type; encrypted : Boolean) is
    is_success, cancelled : Boolean;
    File_Title : GString_Unbounded;
    File_Names : Array_Of_File_Names_Access;
    procedure Dispose is new Ada.Unchecked_Deallocation (
      Array_Of_File_Names,
      Array_Of_File_Names_Access
    );
  begin
    Open_Files (
      Window,
      "Add files to archive (also doable by Drag & Drop)...",
      File_Names,
       (1 => (G2GU ("All files (*.*)"),
              G2GU ("*.*"))),
      "",
      File_Title,
      is_success
    );
    if is_success then
      if not Is_loaded (Window.zif) then
        Message_Box (
          Window,
          "New archive",
          "You'll be asked under which name the archive will be created.",
          OK_Box,
          Information_Icon
        );
        Window.On_Save_As;
      end if;
      if encrypted then
        Get_password_for_encryption (Window, cancelled);
      else
        cancelled := False;
      end if;
      if Is_loaded (Window.zif) and not cancelled then -- Is_Loaded: we test again (in case Save As failed)
        Window.Go_for_adding (File_Names.all, encrypted);
        Dispose (File_Names);
      end if;
    end if;
  end On_Add_Files;

  procedure On_Add_Folder (Window : in out MDI_Child_Type; encrypted : Boolean) is
    cancelled : Boolean;
    dir : constant GString := Get_Directory (
      Window       => Window,
      Dialog_Title => "Add folder to archive (also doable by Drag & Drop)...",
      Initial_Path => GU2G (Window.extract_dir));
  begin
    if dir = "" then
      return;
    end if;
    if not Is_loaded (Window.zif) then
      Message_Box (
        Window,
        "New archive",
        "You'll be asked under which name the archive will be created.",
        OK_Box,
        Information_Icon
      );
      Window.On_Save_As;
    end if;
    if encrypted then
      Get_password_for_encryption (Window, cancelled);
    else
      cancelled := False;
    end if;
    if Is_loaded (Window.zif) and not cancelled then -- Is_Loaded: we test again (in case Save As failed)
      Window.Go_for_adding ((1 => G2GU (dir)), encrypted);
    end if;
  end On_Add_Folder;

  procedure On_Find (Window : in out MDI_Child_Type) is
    box : Find_box_Type;
    --
    procedure Get_Data (dummy : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      Window.name_search := G2GU (box.Name_to_be_searched.Text);
      Window.content_search := G2GU (box.Content_to_be_searched.Text);
    end Get_Data;
    --
    return_code : Operation_return_code;
    no_matches : Boolean;
  begin
    box.Create_Full_Dialog (Window);
    box.Name_to_be_searched.Text (GU2G (Window.name_search));
    box.Content_to_be_searched.Text (GU2G (Window.content_search));
    box.Center (Window);
    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    box.Name_to_be_searched.Focus;

    if Show_Dialog (box, Window) = GWindows.Constants.IDOK then

      Process_Archive_GWin
        (Window         => Window,
         operation      => Search,
         file_names     => (1 => Window.name_search),
         search_pattern => GU2G (Window.content_search),
         return_code    => return_code);

      no_matches :=
        Index (Window.last_op_comment_1, " 0") > 0
        and Index (Window.last_op_comment_2, " 0") > 0;
      declare
        msg : constant GString :=
          "Search completed." & NL & NL &
          GU2G (Window.last_op_comment_1) & NL &
          GU2G (Window.last_op_comment_2);
      begin
        if no_matches then
          Message_Box (Window, "Find in archive", msg, OK_Box, Information_Icon);
        elsif Message_Box
          (Window,
           "Find in archive",
           msg & NL & NL &
           "Do you want to see full results (flat view & result sort) ?",
           Yes_No_Box,
           Question_Icon)
          = Yes
        then
          Change_View (Window, Flat, force => False);
          Window.Directory_List.Sort
            (Window.opt.column_index (Result) - 1, AZip_LV_Ex.Down);
        end if;
      end;
    end if;
  end On_Find;

  type Selection_Change_Type is (select_all, unselect_all, invert);

  procedure Change_Selection (Window : in out MDI_Child_Type; select_mode : Selection_Change_Type) is
    DL : Directory_list_type renames Window.Directory_List;
  begin
    DL.refreshing := True;
    --  NB: LV item index is 0-based.
    case select_mode is
      when select_all =>
        for i in reverse 0 .. DL.Item_Count - 1 loop
          DL.Selected (i, True);
        end loop;
      when unselect_all =>
        for i in reverse 0 .. DL.Item_Count - 1 loop
          DL.Selected (i, False);
        end loop;
      when invert =>
        for i in reverse 0 .. DL.Item_Count - 1 loop
          DL.Selected (i, not DL.Is_Selected (i));
        end loop;
    end case;
    DL.refreshing := False;
    DL.On_Item_Changed;
    DL.Focus;
  end Change_Selection;

  procedure On_Test (Window : in out MDI_Child_Type) is
    count_ok, count_ko, count_nt : Natural;
    return_code : Operation_return_code;
  begin

    Process_Archive_GWin
      (Window      => Window,
       operation   => Test,
       return_code => return_code);

    Count_test_totals (Window.zif, count_ok, count_ko, count_nt);
    if count_nt > 0 then
      null;  --  operation cancelled, nothing to say
    elsif count_ko = 0 then
      Message_Box
        (Window,
         "Data integrity",
         "All entries in archive are OK.",
         Icon => Information_Icon);
    else
      Message_Box
        (Window,
         "Data integrity: at least one failure",
         count_ok'Wide_Image & " entries are OK;" & NL &
         count_ko'Wide_Image &
         " entries had errors or could not be processed.",
         Icon => Warning_Icon);
    end if;
  end On_Test;

  procedure Stop_msg_on_encrypted_archive (Window : MDI_Child_Type; op_title : GString) is
  begin
      Message_Box
        (Window,
         op_title,
         "Archives with encryption (even on some entries only) are" & NL &
         "currently not supported for this operation (" & op_title & ").",
         OK_Box,
         Stop_Icon);
  end Stop_msg_on_encrypted_archive;

  procedure On_Update (Window : in out MDI_Child_Type) is
    mem_dir : constant String := Ada.Directories.Current_Directory;
    --  !! Not UTF-8 capable
    new_dir : constant String := Ada.Directories.Containing_Directory
      (To_UTF_8 (GU2G (Window.ID.file_name)));
       --  !! Not UTF-8 capable
    return_code : Operation_return_code;
    answer : Integer;
  begin
    if not Is_loaded (Window.zif) then
      return;
    end if;
    if Has_Zip_archive_encrypted_entries (Window.zif) then
      Stop_msg_on_encrypted_archive (Window, "Archive update");
      return;
    end if;

    Modal_Dialogs.Show_Update_Box (Window, Window.mdi_root.opt.backup_update, answer);

    if answer = GWindows.Constants.IDOK then

      Ada.Directories.Set_Directory (new_dir);  --  !! Not UTF-8 capable

      Process_Archive_GWin
        (Window        => Window,
         operation     => Update,
         option_flag_2 => Window.mdi_root.opt.backup_update,
         new_temp_name => Temp_AZip_Name (Window),
         return_code   => return_code);

      if mem_dir'Length > 0 and then mem_dir (mem_dir'Last) = ':' then
        --  Bug in GNAT up to GPL 2011 - cf issue [L216-021 public], fixed in 2012.
        Ada.Directories.Set_Directory (mem_dir & '\');  --  !! Check if UTF-8 capable
      else
        Ada.Directories.Set_Directory (mem_dir);  --  !! Check if UTF-8 capable
      end if;
      if return_code = aborted or return_code = archive_too_large then
        null;
      elsif Window.last_max_code <= only_archive then
        Message_Box (Window,
        "Archive update completed",
        "Update completed." & NL & "No entry needed to be updated.",
        OK_Box,
        Information_Icon);
      elsif Message_Box (Window,
          "Archive update completed",
          "Update completed." & NL & NL &
          "Do you want to see full results (flat view & result sort) ?",
          Yes_No_Box,
          Question_Icon)
        = Yes
      then
        Change_View (Window, Flat, force => False);
        Window.Directory_List.Sort (Window.opt.column_index (Result) - 1, AZip_LV_Ex.Down);
      end if;
    end if;
  end On_Update;

  procedure On_Recompress (Window : in out MDI_Child_Type) is
    return_code : Operation_return_code;
    answer : Integer;
  begin
    if not Is_loaded (Window.zif) then
      return;
    end if;
    if Has_Zip_archive_encrypted_entries (Window.zif) then
      Stop_msg_on_encrypted_archive (Window, "Archive recompression");
      return;
    end if;

    Modal_Dialogs.Show_Recompress_Box (Window, Window.mdi_root.opt.backup_recomp, answer);

    if answer in ID_Recomp_Single_Pass | ID_Recomp_Brute_Force then

      Process_Archive_GWin
        (Window        => Window,
         operation     => Recompress,
         option_flag_1 => answer = ID_Recomp_Brute_Force,
         option_flag_2 => Window.mdi_root.opt.backup_recomp,
         new_temp_name => Temp_AZip_Name (Window),
         return_code   => return_code);

      if return_code = aborted then
        null;
      elsif Window.last_max_code = nothing then
        Message_Box
          (Window,
           "Archive recompression completed",
           "Recompression completed." & NL & "No entry could be recompressed to a smaller size.",
           OK_Box,
           Information_Icon);
      elsif
        Message_Box
          (Window,
           "Archive recompression completed",
           "Recompression completed." & NL & NL &
           "Do you want to see full results (flat view & result sort) ?",
           Yes_No_Box,
           Question_Icon)
        = Yes
      then
        Change_View (Window, Flat, force => False);
        Window.Directory_List.Sort (Window.opt.column_index (Result) - 1, AZip_LV_Ex.Down);
      end if;
    end if;
  end On_Recompress;

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item   : in     Integer)
  is
  begin
    case Item is
      when IDM_Open_Containing_Folder =>
        if Window.ID.file_name /= "" then
          GWin_Util.Start (Ada.Directories.Containing_Directory (G2S (GU2G (Window.ID.file_name))));
        end if;
      when IDM_SAVE_ARCHIVE_AS =>
        Window.On_Save_As;
      when IDM_CLOSE_ARCHIVE =>
        Window.Close;
      when IDM_Properties =>
        AZip_GWin.Properties (Window);
      --  Edit  --
      when IDM_Select_all       => Change_Selection (Window, select_all);
      when IDM_Unselect_all     => Change_Selection (Window, unselect_all);
      when IDM_Invert_Selection => Change_Selection (Window, invert);
      when IDM_EXTRACT               => On_Extract (Window, dropped => False);
      when IDM_ADD_FILES             => On_Add_Files (Window, encrypted => False);
      when IDM_Add_Files_Encryption  => On_Add_Files (Window, encrypted => True);
      when IDM_Add_Folder            => On_Add_Folder (Window, encrypted => False);
      when IDM_Add_Folder_Encryption => On_Add_Folder (Window, encrypted => True);
      when IDM_Delete_selected       => On_Delete (Window);
      --
      when IDM_TEST_ARCHIVE =>
        On_Test (Window);
      when IDM_UPDATE_ARCHIVE =>
        On_Update (Window);
      when IDM_RECOMPRESS_ARCHIVE =>
        On_Recompress (Window);
      when IDM_FIND_IN_ARCHIVE =>
        On_Find (Window);
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
        Window.mdi_root.opt.sort_column := AZip_Common.User_options.no_sorting;
        Window.mdi_root.remember_sorting := False;
      when IDM_Select_columns =>
        --  Propagate current child window settings to main options.
        Set_all_column_widths_to_main_options (Window);
        Window.mdi_root.opt.view_mode := Window.opt.view_mode;
        --  Start dialog.
        Select_columns_dialog (Window.mdi_root.all);
      when IDM_Up_one_level =>
        Check_Path (Window, go_up => True);
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
    tab_bar : Tabs.AZip_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    tab_index : Integer;
  begin
    Update_Information (Window, toolbar_and_menu);
    tab_index := tab_bar.Tab_Index (Window.ID);
    if tab_index >= 0 then
      tab_bar.Selected_Tab (tab_index);
    end if;
  end On_Focus;

  overriding procedure On_Close (Window    : in out MDI_Child_Type;
                                 Can_Close :    out Boolean)
  is
    bar : Office_Applications.Classic_Main_Tool_Bar_Type
      renames Window.mdi_root.Tool_Bar;
    tab_bar : Tabs.AZip_Tab_Bar_Type renames Window.mdi_root.tab_bar;
  begin
    Can_Close := True;
    if Is_Document_Modified (Window) then
      --  This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                GU2G (Window.ID.short_name) & "' ?",
                Yes_No_Cancel_Box,
                Question_Icon)
        is
          when Yes =>
            On_Save (Window);
            exit when not Is_Document_Modified (Window);
          when No =>
            exit;
          when Cancel =>
            Window.mdi_root.Success_in_enumerated_close := False;
            Can_Close := False;
            exit;
          when others =>
            null;
        end case;
      end loop;
    else
      Update_Common_Menus (Window, GU2G (Window.ID.file_name));
      Window.Status_deamon.Stop;
    end if;
    if Can_Close then
      --  Memorize column widths
      Set_all_column_widths_to_main_options (Window);
      Attempt_Remember_Sorting (Window);
      --  Pass view mode and the tree width portion to parent,
      --  this will memorize choice of last closed window.
      Window.mdi_root.opt.view_mode := Window.opt.view_mode;
      Memorize_Splitter (Window);
      Window.mdi_root.opt.tree_portion := Window.opt.tree_portion;
      --  For the case there is no more child window, disable toolbar items.
      --  This action is reversed as soon as another child window is focused.
      bar.Enabled (IDM_ADD_FILES, False);
      bar.Enabled (IDM_Add_Files_Encryption, False);
      bar.Enabled (IDM_Toggle_Flat_Tree_View, False);
      bar.Enabled (IDM_Properties, False);
      tab_bar.Delete_Tab (tab_bar.Tab_Index (Window.ID));
      Window.is_closing := True;
    end if;
  end On_Close;

  procedure On_Mouse_Move
    (Window : in out MDI_Child_Type;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
    A : constant GWindows.Types.Point_Type :=
      Point_To_Desktop (Window, (X, Y));
  begin
    if Window.mdi_root.dragging.is_dragging then
      declare
        expl_path : constant GString := Explorer_Path_At_Location (A.X, A.Y);
      begin
        if expl_path = "" then
          GWindows.Cursors.Set_Cursor (Window.mdi_root.dragging.cursor_drag_no_way);
          Window.mdi_root.dragging.destination := to_nowhere;
        else
          GWindows.Cursors.Set_Cursor (Window.mdi_root.dragging.cursor_drag_unpack);
          if Is_Desktop_At_Location (A.X, A.Y) then
            Window.mdi_root.dragging.destination := to_desktop;
          else
            Window.mdi_root.dragging.destination := to_explorer;
            Window.mdi_root.dragging.destination_path := G2GU (Explorer_Path_At_Location (A.X, A.Y));
          end if;
        end if;
      end;
      Window.Update_status_bar;
    end if;
  end On_Mouse_Move;

  procedure On_Left_Mouse_Button_Up
    (Window : in out MDI_Child_Type;
     X      : in     Integer;
     Y      : in     Integer;
     Keys   : in     Mouse_Key_States)
  is
  pragma Unreferenced (Keys);
    A : constant GWindows.Types.Point_Type :=
      Point_To_Desktop (Window, (X, Y));
  begin
    if Window.mdi_root.dragging.is_dragging then
      GWindows.Cursors.Set_Cursor (Window.mdi_root.dragging.cursor_arrow);
      Release_Mouse;
      case Window.mdi_root.dragging.destination is
        when to_desktop | to_explorer =>
          On_Extract (Window, dropped => True, drop_X => A.X, drop_Y => A.Y);
        when others =>
          null;
      end case;
      Window.mdi_root.dragging.is_dragging := False;
      Window.Update_status_bar;
    end if;
  end On_Left_Mouse_Button_Up;

  package body Daemons is
    ---------------------------------------------------------------------------
    -- This background task displays on demand (also from another task)      --
    -- informations about archive and various status.                        --
    ---------------------------------------------------------------------------

    task body Status_display is
      current_child_window : MDI_Child_Access;
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Display (w : MDI_Child_Access) do
            current_child_window := w;
          end Display;
          Update_Information (current_child_window.all, status_bar);
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
      current_child_window : MDI_Child_Access;
      pragma Unreferenced (current_child_window);
    begin
      accept Start;
      loop
        select
          accept Stop;
          exit;
        or
          accept Test (w : MDI_Child_Access) do
            current_child_window := w;
          end Test;
          --  (perform test)
        or
          delay 0.05; -- relax
        end select;
      end loop;
    end Testing_type;

  end Daemons;

end AZip_GWin.MDI_Child;
