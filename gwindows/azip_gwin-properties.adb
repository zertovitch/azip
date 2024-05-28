with AZip_Common;
with AZip_Resource_GUI;
with AZip_GWin.Modal_Dialogs;

with Zip;
with Zip_Streams;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Locales;

with Ada.Strings.Wide_Fixed;
with Interfaces;

procedure AZip_GWin.Properties (Window : in out MDI_Child.MDI_Child_Type) is
  box : AZip_Resource_GUI.Properties_box_Type;
  use GWindows.Buttons, Interfaces;
  total_uncompressed : Zip.Zip_64_Data_Size_Type := 0;
  total_compressed   : Zip.Zip_64_Data_Size_Type := 0;
  --  total_entries: Natural:= 0;
  files_per_method : array (Zip.PKZip_method) of Natural := (others => 0);
  uncompressed_per_method : array (Zip.PKZip_method) of Unsigned_64 := (others => 0);
  compressed_per_method : array (Zip.PKZip_method) of Zip.Zip_64_Data_Size_Type := (others => 0);
  sep_str : constant GString := GWindows.Locales.Get_Thousands_Separator;
  sep : constant Wide_Character := sep_str (sep_str'First);

  use Ada.Strings, Ada.Strings.Wide_Fixed, AZip_Common, Interfaces, Zip;

  procedure Show_List is
    row : Integer := -1;
  begin
    box.Stats_list.Clear;
    for m in files_per_method'Range loop
      if (m /= unknown and m /= tokenize)
        --  ^ NB: Tokenize was never implemented: "5.4.1 This method is not used by PKZIP."
        and then (files_per_method (m) > 0 or else box.Show_all_Formats.State = Checked)
      then
        row := row + 1;
        --  Format (""method"")"
        box.Stats_list.Insert_Item (S2G (Zip.Image (m)), row);
        --  Entries
        box.Stats_list.Set_Sub_Item (Integer'Wide_Image (files_per_method (m)), row, 1);
        if uncompressed_per_method (m) > 0 then
          --  % of data
          box.Stats_list.Set_Sub_Item (
            Ratio_pct_Image (uncompressed_per_method (m), total_uncompressed),
            row, 2
          );
          --  Ratio
          box.Stats_list.Set_Sub_Item (
            Ratio_pct_Image (Unsigned_64 (compressed_per_method (m)), uncompressed_per_method (m)),
            row, 3
          );
        end if;
        --  Matches statement in UnZip.Decompress, line ~2035:
        case m is
          when store | shrink | Reduce_Format | implode |
               deflate | deflate_e |
               bzip2 | lzma_meth =>
            null;
          when others =>
            box.Stats_list.Set_Sub_Item ("unsupported", row, 4);
        end case;
      end if;
    end loop;
  end Show_List;

  procedure Box_Show_all_Formats_clicked (dummy : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Show_List;
  end Box_Show_all_Formats_clicked;

  procedure Action (
      name             : String; -- 'name' is compressed entry's name
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : Zip_64_Data_Size_Type;
      uncomp_size      : Zip_64_Data_Size_Type;
      crc_32           : Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      name_encoding    : Zip_Name_Encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    )
  is
  pragma Unreferenced (user_code, encrypted_2_x, read_only, name_encoding, date_time, crc_32, file_index, name);
  begin
    total_uncompressed := total_uncompressed + Unsigned_64 (uncomp_size);
    total_compressed   := total_compressed   + comp_size;
    uncompressed_per_method (method) := uncompressed_per_method (method) + Unsigned_64 (uncomp_size);
    compressed_per_method (method)   := compressed_per_method (method) + comp_size;
    files_per_method (method)        := files_per_method (method) + 1;
  end Action;
  --
  procedure Gather_stats is new Traverse_verbose (Action);
  --
begin
  box.Create_Full_Dialog (Window);
  box.Center;
  box.Stats_list.Insert_Column ("Compression format (""method"")", 0, 180);
  box.Stats_list.Insert_Column ("Entries", 1, 60);
  box.Stats_list.Insert_Column ("% of data", 2, 60);
  box.Stats_list.Insert_Column ("Ratio", 3, 50);
  box.Stats_list.Insert_Column ("Note", 4, 80);
  box.Uncomp_size.Text ("0 byte");
  box.Comp_size.Text   ("0 byte");
  box.Comp_ratio.Text ("");
  box.Show_all_Formats.State (Unchecked);
  box.Show_all_Formats.On_Click_Handler (Box_Show_all_Formats_clicked'Unrestricted_Access);
  --
  if Is_loaded (Window.zif) then
    Gather_stats (Window.zif);
    box.Numb_entries.Text (Trim (Natural'Wide_Image (Entries (Window.zif)), Left));
    box.Uncomp_size.Text (Long_file_size_image (total_uncompressed, sep));
    box.Comp_size.Text (Long_file_size_image (total_compressed, sep));
    if total_uncompressed > 0 then
      box.Comp_ratio.Text (
        "Ratio: " &
        Ratio_pct_Image (Unsigned_64 (total_compressed), total_uncompressed)
      );
    end if;
    Show_List;
  end if;
  case GWindows.Application.Show_Dialog (box, Window) is
    when AZip_Resource_GUI.ID_Button_About_Azip =>
           AZip_GWin.Modal_Dialogs.Show_About_Box (Window.MDI_Root.all);
    when others => null;
  end case;
end AZip_GWin.Properties;
