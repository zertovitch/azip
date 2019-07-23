--------------------------------
--  Hiding & showing columns  --
--------------------------------

package body AZip_GWin.Columns is

  --  Number of pixels under which we consider the column as hidden.
  --
  threshold : constant := 20;

  function Smart_column_width(Window : MDI_Child_Type; topic : Entry_topic) return Natural
  is
  begin
    if topic = Path and Window.opt.view_mode = Tree then
      return 0;
    elsif Window.MDI_Root.opt.visible_column(topic) then
      return Integer'Max(threshold, Window.MDI_Root.opt.column_width(topic));
    else
      return 0;
    end if;
  end Smart_column_width;

end AZip_GWin.Columns;
