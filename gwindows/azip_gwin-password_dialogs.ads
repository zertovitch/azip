with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;

with GWindows;                          use GWindows;
with GWindows.Base;                     use GWindows.Base;

package AZip_GWin.Password_dialogs is

  procedure Get_password_decrypt(
    Window     : in     MDI_Child_Type;
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
                 --  Immediate UI parent
    entry_name : in     GString;
    password   : in out GString_Unbounded
  );

end AZip_GWin.Password_dialogs;
