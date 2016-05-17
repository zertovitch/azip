with AZip_Common;                       use AZip_Common;
with AZip_Resource_GUI;                 use AZip_Resource_GUI;
with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;

with Zip;                               use Zip;
with UnZip;
with Zip_Streams;

with GWindows;                          use GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Locales;

with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;
with Interfaces;

procedure AZip_GWin.Properties(Window: in out MDI_Child_Type) is
  box: Properties_box_Type;
  total_uncompressed, total_compressed: UnZip.File_size_type:= 0;
  -- total_entries: Natural:= 0;
  files_per_method: array(UnZip.PKZip_method) of Natural:= (others => 0);
  uncompressed_per_method,
  compressed_per_method: array(UnZip.PKZip_method) of UnZip.File_size_type:= (others => 0);
  sep_str: constant GString:= GWindows.Locales.Get_Thousands_Separator;
  sep: constant Wide_Character:= sep_str(sep_str'First);

  use Interfaces;

  procedure Action(
      name             : String; -- 'name' is compressed entry's name
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size        : File_size_type;
      uncomp_size      : File_size_type;
      crc_32           : Interfaces.Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      name_encoding    : Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean; -- PKZip 2.x encryption
      user_code        : in out Integer
    )
  is
  pragma Unreferenced (user_code, encrypted_2_x, read_only, name_encoding, date_time, crc_32, file_index, name);
  begin
    total_uncompressed := total_uncompressed + uncomp_size;
    total_compressed   := total_compressed   + comp_size;
    uncompressed_per_method(method) := uncompressed_per_method(method) + uncomp_size;
    compressed_per_method(method)   := compressed_per_method(method) + comp_size;
    files_per_method(method)        := files_per_method(method) + 1;
  end;
  procedure Gather_stats is new Traverse_verbose(Action);
  function Nice_image(format: UnZip.PKZip_method) return GString is
    img_stuffed: GString(1..UnZip.PKZip_method'Width):= (others=> ' ');
    img: constant GString:= UnZip.PKZip_method'Wide_Image(format);
  begin
    img_stuffed(1..img'Length):= To_Lower(img);
    return img_stuffed;
  end Nice_image;
  row: Integer:= -1;
begin
  box.Create_Full_Dialog(Window);
  box.Center;
  box.Stats_list.Insert_Column("Format (""method"")",0,100);
  box.Stats_list.Insert_Column("Entries",1,50);
  box.Stats_list.Insert_Column("% of data",2,60);
  box.Stats_list.Insert_Column("Ratio",3,50);
  box.Uncomp_size.Text("0 byte");
  box.Comp_size.Text("0 byte");
  box.Comp_ratio.Text("");
  if Is_loaded(Window.zif) then
    Gather_stats(Window.zif);
    box.Numb_entries.Text(Trim(Natural'Wide_Image(Entries(Window.zif)), Left));
    box.Uncomp_size.Text(Long_file_size_image(total_uncompressed, sep));
    box.Comp_size.Text(Long_file_size_image(total_compressed, sep));
    if total_uncompressed > 0 then
      box.Comp_ratio.Text("Ratio: " & Ratio_pct_Image(total_compressed, total_uncompressed));
    end if;
    for m in files_per_method'Range loop
      if files_per_method(m) > 0 then
        row:= row + 1;
        box.Stats_list.Insert_Item(Nice_image(m),row);
        box.Stats_list.Set_Sub_Item(Integer'Wide_Image(files_per_method(m)),row,1);
        if uncompressed_per_method(m) > 0 then
          box.Stats_list.Set_Sub_Item(
            Ratio_pct_Image(uncompressed_per_method(m), total_uncompressed),
            row, 2
          );
          box.Stats_list.Set_Sub_Item(
            Ratio_pct_Image(compressed_per_method(m), uncompressed_per_method(m)),
            row, 3
          );
        end if;
      end if;
    end loop;
  else
    box.Numb_entries.Text("0 (empty)");
  end if;
  Show_Dialog(box, Window);
end;
