--------------------------------
--  Hiding & showing columns  --
--------------------------------

with AZip_Common;

with AZip_GWin.MDI_Child,
     AZip_GWin.MDI_Main;

package AZip_GWin.Columns is

  function Get_column_width_from_main_options
    (Window : MDI_Child.MDI_Child_Type;
     topic  : AZip_Common.Entry_topic) return Natural;
  --  NB: if the column is hidden, the width returned is 0.

  procedure Set_column_width_to_main_options
    (Window    : in out MDI_Child.MDI_Child_Type;
     topic     :        AZip_Common.Entry_topic;
     new_width :        Natural);
  --  NB: under a certain threshold, the column is considered as hidden and options are changed accordingly.

  --
  --  Automatic version (all column widths in the list view are read/written).
  --
  procedure Get_all_column_widths_from_main_options (Window : in out MDI_Child.MDI_Child_Type);
  procedure Set_all_column_widths_to_main_options (Window : in out MDI_Child.MDI_Child_Type);

  --  Automatic version, for *all* child windows.
  procedure Get_all_column_widths_from_options (Window : MDI_Main.MDI_Main_Type);

  procedure Select_columns_dialog (Window : in out MDI_Main.MDI_Main_Type);

end AZip_GWin.Columns;
