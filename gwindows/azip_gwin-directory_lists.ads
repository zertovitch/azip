with AZip_Common.User_options;

with Zip;
with Zip_Streams;

with GWindows.Base;
with GWindows.Common_Controls.Ex_List_View;
with GWindows.Types;

with Interfaces;

package AZip_GWin.Directory_Lists is

  type LV_Payload is record
    index_before_sorting : Integer;
    name                 : AZip_Common.UTF_16_Unbounded_String;
    extension            : AZip_Common.UTF_16_Unbounded_String;
    path                 : AZip_Common.UTF_16_Unbounded_String;
    uncompressed_size    : Interfaces.Integer_64;
    compressed_size      : Interfaces.Integer_64;
    crc_32               : Interfaces.Unsigned_32;
    date_time            : Zip_Streams.Time;
    format               : Zip.PKZip_method;
    name_encoding        : Zip.Zip_Name_Encoding;
    read_only            : Boolean;
    ratio                : Long_Float;  --  Compressed / Uncompressed ratio
    result_code          : Integer;
  end record;

  package AZip_LV_Ex is new GWindows.Common_Controls.Ex_List_View (LV_Payload);

  ----------------------------------------------
  --  Directory_list_type                     --
  --  Full or partial archive directory list  --
  ----------------------------------------------

  type Column_topic_array is
    array (0 .. AZip_Common.User_options.Column_Integer_Array'Length)
      of AZip_Common.Entry_topic;

  type Directory_list_type is
    new AZip_LV_Ex.Ex_List_View_Control_Type with
  record
    --  During a refresh, we skip status display.
    --  Otherwise, the status would be updated for each item's change!
    refreshing     : Boolean := False;
    --  Inverse of parent window's opt.column_index,
    --  for a bit quicker sorting (see On_Compare).
    curr_col_topic : Column_topic_array;
  end record;

  overriding procedure On_Item_Changed
    (Control : in out Directory_list_type);

  overriding function On_Compare
    (Control   : in Directory_list_type;
     Column    : in Natural;
     Payload_1 : in LV_Payload;
     Payload_2 : in LV_Payload) return Integer;

  overriding procedure Sort
    (Control    : in out Directory_list_type;
     Column     : in     Natural;
     Direction  : in     AZip_LV_Ex.Sort_Direction_Type;
     Show_Icon  : in     Boolean := True;
     Technique  : in     AZip_LV_Ex.Comparison_Technique_Type := AZip_LV_Ex.As_Strings);

  overriding procedure On_Focus (Control : in out Directory_list_type);

  overriding procedure On_Notify
    (Window       : in out Directory_list_type;
     Message      : in     GWindows.Base.Pointer_To_Notification;
     Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
     Return_Value : in out GWindows.Types.Lresult);

  overriding procedure On_Free_Payload
    (Control : in out Directory_list_type;
     Payload :    out AZip_LV_Ex.Data_Access);

  overriding procedure On_Right_Click (Control : in out Directory_list_type);

end AZip_GWin.Directory_Lists;
