--------------------------------
--  Hiding & showing columns  --
--------------------------------

with AZip_Common;                       use AZip_Common;
with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;
with AZip_GWin.MDI_Main;                use AZip_GWin.MDI_Main;

package AZip_GWin.Columns is

  function Get_column_width_from_options (Window : MDI_Child_Type; topic : Entry_topic) return Natural;
  --  NB: if the column is hidden, the width returned is 0.

  procedure Set_column_width_to_options (Window : in out MDI_Child_Type; topic : Entry_topic; new_width : Natural);
  --  NB: under a certain threshold, the column is considered as hidden and options are changed accordingly.

  --
  --  Automatic version (all column widths in the list view are read/written).
  --
  procedure Get_all_column_widths_from_options (Window : in out MDI_Child_Type);
  procedure Set_all_column_widths_to_options (Window : in out MDI_Child_Type);

  --  Automatic version, for *all* child windows.
  procedure Get_all_column_widths_from_options (Window : MDI_Main_Type);

  procedure Select_columns_dialog (Window : in out MDI_Main_Type);

end AZip_GWin.Columns;
