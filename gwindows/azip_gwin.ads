with GWindows;                          use GWindows;
with GWindows.Constants;
with GWindows.GStrings;                 use GWindows.GStrings;

package AZip_GWin is

  type View_Mode_Type is (Flat, Tree);

  type Option_Pack_Type is record
    view_mode: View_Mode_Type:= Flat;
  end record;

end AZip_GWin;
