---------------------------
--  Dragging operations  --
---------------------------

with GWindows.Cursors;

package AZip_GWin.Dragging is

  type Kind_of_dragging_destination is (to_azip, to_explorer, to_desktop, to_nowhere);

  use GWindows.Cursors;

  type Dragging_info is record
    is_dragging        : Boolean := False;
    destination        : Kind_of_dragging_destination;
    destination_path   : GString_Unbounded;
    cursor_drag_unpack : Cursor_Type;  --  Drop to Explorer / Desktop
    cursor_drag_no_way : Cursor_Type;  --  Cannot drop here
    cursor_arrow       : Cursor_Type;  --  Back to normal
  end record;

end AZip_GWin.Dragging;
