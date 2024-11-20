with AZip_Common;

with AZip_Resource_GUI;

with Zip;

with GWindows.Application,
     GWindows.Constants,
     GWindows.Static_Controls.Web,
     GWindows.Types;

with Ada.Strings.Wide_Unbounded;

with GNAT.Compiler_Version;

package body AZip_GWin.Modal_Dialogs is

  procedure Show_About_Box (Window : in out MDI_Main.MDI_Main_Type) is
    use GWindows.Application, GWindows.Constants, GWindows.Static_Controls.Web;
    box : AZip_Resource_GUI.About_box_Type;
    url_azip, url_gnat, url_gnavi, url_ini, url_zipada : URL_Type;
    --
    procedure Credits_clicked (dummy : in out GWindows.Base.Base_Window_Type'Class) is
      credits_box : AZip_Resource_GUI.Credits_box_Type;
    begin
      credits_box.Create_Full_Dialog (box);
      credits_box.Small_Icon ("AZip_Doc_Icon_Name");
      credits_box.Center;
      Show_Dialog (credits_box, box);
    end Credits_clicked;
    --
    procedure Sponsoring_clicked (Button : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      Show_Sponsoring_Box (Button, First_Visit => False);
    end Sponsoring_clicked;
    --
    function GNAT_Version_string return String is
      package CVer is new GNAT.Compiler_Version;
      v : constant String := CVer.Version;
    begin
      return v;
    end GNAT_Version_string;
    --
  begin
    box.Create_Full_Dialog (Window);
    box.Copyright_label.Text (S2G (AZip_Resource_GUI.Version_info.LegalCopyright));
    box.Version_label.Text
      (S2G (AZip_Resource_GUI.Version_info.FileVersion) & ", built as" &
      GWindows.GStrings.To_GString_From_String (Integer'Image (GWindows.Types.Wparam'Size)) &
      " bit app.");
    Create_and_Swap (url_azip, box.AZip_URL, box, S2G (AZip_Common.azip_web_page));
    Create_and_Swap (url_gnat, box.GNAT_URL, box, "https://www.adacore.com/community");
    box.GNAT_Version.Text (S2G ("version " & GNAT_Version_string));
    Create_and_Swap (url_gnavi,   box.GNAVI_URL,     box, "http://sf.net/projects/gnavi");
    Create_and_Swap (url_ini,     box.Ini_files_URL, box, "http://sf.net/projects/ini-files");
    Create_and_Swap (url_zipada,  box.ZipAda_URL,    box, S2G (Zip.web));
    box.ZipAda_Version.Text (S2G ("version " & Zip.version & ", ref. " & Zip.reference));
    box.Credits_button_permanent.Show;
    box.Credits_button.Hide;
    box.Credits_button_permanent.On_Click_Handler (Credits_clicked'Unrestricted_Access);
    box.Sponsoring_button_permanent.Show;
    box.Sponsoring_button.Hide;
    box.Sponsoring_button_permanent.On_Click_Handler (Sponsoring_clicked'Unrestricted_Access);
    box.Center;
    if Show_Dialog (box, Window) = IDOK then
      null;
    end if;
  end Show_About_Box;

  procedure Show_Recompress_Box
    (Window : in out GWindows.Base.Base_Window_Type'Class;
     Answer :    out Integer)
  is
    use GWindows.Application, GWindows.Constants, GWindows.Static_Controls.Web;
    box : AZip_Resource_GUI.Recompress_Box_Type;  --  Possible addition: check box for backup.
  begin
    box.Create_Full_Dialog (Window);
    box.Center;
    Answer := Show_Dialog (box, Window);
  end Show_Recompress_Box;

  procedure Show_Sponsoring_Box
    (Window      : in out GWindows.Base.Base_Window_Type'Class;
     First_Visit : in     Boolean)
  is
    use GWindows.Application, GWindows.Constants, GWindows.Static_Controls.Web;
    box : AZip_Resource_GUI.Sponsoring_box_Type;
    url_payal : URL_Type;
    url : String :=
      "8,,0-fqq)))r0?'0?4r=13q=97s>72q);>-=.a7,;3A2?3;c\12?,712u,1u" &
      ",8;u?+,81.u1:u_F70z=3<cA<12?,712-z>+-72;--c;-93{lp>4+;)72r=8";
    --
    procedure CCY_Changed (Link : in out GWindows.Base.Base_Window_Type'Class) is
    pragma Unreferenced (Link);
      use Ada.Strings.Wide_Unbounded;
    begin
      url_payal.URL := G2GU (S2G (url)) & "&currency_code=" & box.Currency_box.Text;
    end CCY_Changed;

  begin
    box.Create_Full_Dialog (Window);
    if First_Visit then
      box.Text ("Important note");
      box.Sponsoring_label.Text (
        "Welcome to AZip! This Zip archive manager contains cool, original features " &
        "such as a built-in search function, an archive updater, and a recompression tool. " &
        "Moreover, AZip is FREE of charge! If you appreciate this software " &
        "and would like to support its development, your financial contribution is crucial. " &
        "Thank you. You can also reach this box later through the Help -> Sponsoring menu."
      );
    end if;
    for c of url loop
      c := Character'Val (160 - Character'Pos (c));
    end loop;
    Create_and_Swap (url_payal, box.Label_Paypal, box, S2G (url));
    box.Currency_box.On_Update_Handler (CCY_Changed'Unrestricted_Access);
    box.Currency_box.Text ("CHF");
    box.Center;
    if Show_Dialog (box, Window) = IDOK then
      null;
    end if;
  end Show_Sponsoring_Box;

end AZip_GWin.Modal_Dialogs;
