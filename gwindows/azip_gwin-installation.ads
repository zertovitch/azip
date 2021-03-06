with AZip_GWin.MDI_Main;

package AZip_GWin.Installation is

  type Executable_Location_Choice is (All_Users, Current_User, Elsewhere);

  function Executable_Location return Executable_Location_Choice;

  subtype Installation_Mode is Executable_Location_Choice range All_Users .. Current_User;

  function Is_Installed return Boolean is (Executable_Location in Installation_Mode);

  function AZip_Title return String is (
    (if AZip_GWin.Installation.Is_Installed then "AZip" else "AZip (non-installed)")
  );

  procedure Installation_Dialog
   (Main_Window : in out MDI_Main.MDI_Main_Type;
    First_Visit : in     Boolean
   );

end AZip_GWin.Installation;
