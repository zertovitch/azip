with AZip_GWin.MDI_Child;               use AZip_GWin.MDI_Child;

with GWindows.Base;

package AZip_GWin.Password_dialogs is

  procedure Get_password_for_decryption(
    Window     : in     MDI_Child_Type;
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
                 --  Immediate UI parent
    entry_name : in     GString;
    password   : in out GString_Unbounded;
    cancelled  :    out Boolean
  );

  procedure Get_password_for_encryption(
    Window     : in out MDI_Child_Type;
    cancelled  :    out Boolean
  );

end AZip_GWin.Password_dialogs;
