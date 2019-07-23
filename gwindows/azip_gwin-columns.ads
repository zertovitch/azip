--------------------------------
--  Hiding & showing columns  --
--------------------------------

with AZip_Common;                       use AZip_Common;
with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;

package AZip_GWin.Columns is

  function Smart_column_width(Window : MDI_Child_Type; topic : Entry_topic) return Natural;

end AZip_GWin.Columns;
