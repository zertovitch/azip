---------------------------------------------------------------------------
--  GUI contents of resource script file: AZip.rc
--  Transcription time: 2025/07/06  13:27:25
--  GWenerator project file: azip.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 29-Jul-2022
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_Drawn;      use GWindows.Buttons.Owner_Drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

pragma Warnings ("U");  --  turn off warnings for unused entity

package AZip_Resource_GUI is

  type Fake_menu_for_commands_in_no_real_menu_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "Fake"
  end record;  --  Fake_menu_for_commands_in_no_real_menu_Type

  --  Menu at line 48
  procedure Create_Full_Menu (New_Menu : in out Fake_menu_for_commands_in_no_real_menu_Type);

  type Menu_MDI_Child_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "&File"
    Popup_0002 : Menu_Type;   --  Popup level: 2; title: "Open &recent"
    Popup_0003 : Menu_Type;   --  Popup level: 1; title: "&Edit"
    Popup_0004 : Menu_Type;   --  Popup level: 1; title: "&Tools"
    Popup_0005 : Menu_Type;   --  Popup level: 1; title: "&View"
    Popup_0006 : Menu_Type;   --  Popup level: 1; title: "&Options"
    Popup_0007 : Menu_Type;   --  Popup level: 1; title: "&Window"
    Popup_0008 : Menu_Type;   --  Popup level: 1; title: "&Help"
  end record;  --  Menu_MDI_Child_Type

  --  Menu at line 134
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Child_Type);

  type Menu_MDI_Main_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "&File"
    Popup_0002 : Menu_Type;   --  Popup level: 2; title: "Open &recent"
    Popup_0003 : Menu_Type;   --  Popup level: 1; title: "&Options"
    Popup_0004 : Menu_Type;   --  Popup level: 1; title: "&Window"
    Popup_0005 : Menu_Type;   --  Popup level: 1; title: "&Help"
  end record;  --  Menu_MDI_Main_Type

  --  Menu at line 181
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Main_Type);

  type About_box_Type is new Window_Type with record

    Static_0001 : Icon_Type;
    --  Label: IDC_STATIC
    Copyright_label : Label_Type;
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    AZip_URL : Label_Type;
    --  Label: IDC_STATIC
    Version_label : Label_Type;
    Static_0006 : Group_Box_Type;
    GNAT_URL : Label_Type;
    GNAT_Version : Label_Type;
    GNAVI_URL : Label_Type;
    ResEdit_URL : Label_Type;
    --  Label: IDC_STATIC
    ZipAda_URL : Label_Type;
    ZipAda_Version : Label_Type;
    Ini_files_URL : Label_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    Credits_button : Dialog_Button_Type;    --  Closes parent window after click
    Credits_button_permanent : Button_Type;  --  Doesn't close parent window after click
    Sponsoring_button : Dialog_Button_Type;    --  Closes parent window after click
    Sponsoring_button_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- About_box_Type

  --  Dialog at resource line 214

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out About_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "About AZip";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out About_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Credits_box_Type is new Window_Type with record

    Static_0001 : Group_Box_Type;
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    Static_0004 : Group_Box_Type;
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    Static_0008 : Group_Box_Type;
    --  Label: 0
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Credits_box_Type

  --  Dialog at resource line 234

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Credits_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Credits";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Credits_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Drop_files_box_Type is new Window_Type with record

    Static_0001 : Icon_Type;
    Encrypt_check_box : Check_Box_Type;
    --  Label: 0
    Static_0002 : Icon_Type;
    Drop_archive_name : Label_Type;
    New_archive_msg : Label_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Drop_files_box_Type

  --  Dialog at resource line 252

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Drop_files_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Drop_files_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Non-Zip file(s) dropped, right-clicked, or opened from Explorer";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Drop_files_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type File_exists_box_Type is new Window_Type with record

    Conflict_simple_name : Label_Type;
    Conflict_location : Label_Type;
    Static_0001 : Group_Box_Type;
    --  Label: IDC_STATIC
    Overwrite_Yes : Dialog_Button_Type;    --  Closes parent window after click
    Overwrite_Yes_permanent : Button_Type;  --  Doesn't close parent window after click
    Overwrite_No : Default_Dialog_Button_Type;    --  Closes parent window after click
    Overwrite_No_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    Overwrite_All : Dialog_Button_Type;    --  Closes parent window after click
    Overwrite_All_permanent : Button_Type;  --  Doesn't close parent window after click
    Overwrite_None : Dialog_Button_Type;    --  Closes parent window after click
    Overwrite_None_permanent : Button_Type;  --  Doesn't close parent window after click
    Overwrite_Rename : Dialog_Button_Type;    --  Closes parent window after click
    Overwrite_Rename_permanent : Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- File_exists_box_Type

  --  Dialog at resource line 272

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out File_exists_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out File_exists_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "File already exists";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out File_exists_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Find_box_Type is new Window_Type with record

    Static_0001 : Icon_Type;
    --  Label: IDC_STATIC
    Name_to_be_searched : Edit_Box_Type;
    Static_0003 : Icon_Type;
    --  Label: IDC_STATIC
    Content_to_be_searched : Edit_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Find_box_Type

  --  Dialog at resource line 290

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Find_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Find_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Find";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Find_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Install_box_Type is new Window_Type with record

    Group_box_exe_location : Group_Box_Type;
    Check_box_installed_all_users : Check_Box_Type;
    Check_box_installed_current_user : Check_Box_Type;
    Check_box_not_installed : Check_Box_Type;
    Label_Installed_All_Users : Label_Type;
    Label_Installed_Current_User : Label_Type;
    Label_NOT_Installed : Label_Type;
    Group_box_pref_location : Group_Box_Type;
    Radio_button_registry : Radio_Button_Type;
    Radio_button_stealth : Radio_Button_Type;
    Label_Registry : Label_Type;
    Label_Stealth : Label_Type;
    Group_box_Desktop_Explorer_integration : Group_Box_Type;
    --  Label: 0
    Button_create_shortcut : Dialog_Button_Type;    --  Closes parent window after click
    Button_create_shortcut_permanent : Button_Type;  --  Doesn't close parent window after click
    --  Label: 0
    Button_context_add : Dialog_Button_Type;    --  Closes parent window after click
    Button_context_add_permanent : Button_Type;  --  Doesn't close parent window after click
    --  Label: 0
    Button_extension_choose : Dialog_Button_Type;    --  Closes parent window after click
    Button_extension_choose_permanent : Button_Type;  --  Doesn't close parent window after click
    Label_Install_note_first_visit : Label_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    ID_Install_all_users : Dialog_Button_Type;    --  Closes parent window after click
    ID_Install_all_users_permanent : Button_Type;  --  Doesn't close parent window after click
    ID_Install_current_user : Dialog_Button_Type;    --  Closes parent window after click
    ID_Install_current_user_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Install_box_Type

  --  Dialog at resource line 323

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Install_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Install_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "AZip installation";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Install_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Option_box_Type is new Window_Type with record

    Extract_Directory_Group_Box : Group_Box_Type;
    Extract_Directory_Edit_Box : Edit_Box_Type;
    Choose_Extract_Directory_Button : Dialog_Button_Type;    --  Closes parent window after click
    Choose_Extract_Directory_Button_permanent : Button_Type;  --  Doesn't close parent window after click
    Temp_Directory_Group_Box : Group_Box_Type;
    Choose_Temp_Directory_Button : Dialog_Button_Type;    --  Closes parent window after click
    Choose_Temp_Directory_Button_permanent : Button_Type;  --  Doesn't close parent window after click
    Temp_Directory_Edit_Box : Edit_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Option_box_Type

  --  Dialog at resource line 341

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Option_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "General Options";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Option_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Password_decryption_box_Type is new Window_Type with record

    Static_0001 : Group_Box_Type;
    Encrypted_entry : Label_Type;
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    Static_0004 : Icon_Type;
    Password_edit : Edit_Box_Type;
    Show_password_box : Check_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Password_decryption_box_Type

  --  Dialog at resource line 360

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Password_decryption_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Password_decryption_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Password for decryption";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Password_decryption_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Password_encryption_box_Type is new Window_Type with record

    Static_0001 : Icon_Type;
    --  Label: IDC_STATIC
    Password_edit : Edit_Box_Type;
    Confirm_Icon : Icon_Type;
    Confirm_Password_Label : Label_Type;
    Password_confirm_edit : Edit_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
    Show_password_box : Check_Box_Type;
  end record; -- Password_encryption_box_Type

  --  Dialog at resource line 379

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Password_encryption_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Password_encryption_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Password for encryption";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Password_encryption_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Progress_box_Type is new Window_Type with record

    Entry_operation_name : Label_Type;
    Entry_name : Label_Type;
    Comment_1 : Label_Type;
    Comment_2 : Label_Type;
    File_Progress : Progress_Control_Type;
    Archive_Progress : Progress_Control_Type;
    Percent_Progress : Label_Type;
    Cancel_button : Dialog_Button_Type;    --  Closes parent window after click
    Cancel_button_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Progress_box_Type

  --  Dialog at resource line 397

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Progress_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Progress_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "AZip is busy.";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Progress_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Properties_box_Type is new Window_Type with record

    --  Label: 0
    Uncomp_size : Edit_Box_Type;
    --  Label: 1
    Comp_size : Edit_Box_Type;
    Comp_ratio : Label_Type;
    --  Label: 02
    Numb_entries : Label_Type;
    Stats_list : List_View_Control_Type;
    Show_all_Formats : Check_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    ID_Button_About_Azip : Dialog_Button_Type;    --  Closes parent window after click
    ID_Button_About_Azip_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Properties_box_Type

  --  Dialog at resource line 418

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Properties_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Properties_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Archive properties";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Properties_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Quick_help_box_Type is new Window_Type with record

    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Quick_help_box_Type

  --  Dialog at resource line 429

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Quick_help_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Quick_help_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "AZip Quick Help - a couple of tips and hints...";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Quick_help_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Quick_help_tab_command_Type is new Window_Type with record

    RC_item_0 : Group_Box_Type;
    --  Label: 0
    Static_0001 : Group_Box_Type;
    Static_0002 : Bitmap_Type;
    --  Label: 0
  end record; -- Quick_help_tab_command_Type

  --  Dialog at resource line 443

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Quick_help_tab_command_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Quick_help_tab_command_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Quick_help_tab_command_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Quick_help_tab_gui_Type is new Window_Type with record

    Static_0001 : Group_Box_Type;
    Static_0002 : Icon_Type;
    --  Label: 0
    Static_0003 : Group_Box_Type;
    Static_0004 : Icon_Type;
    --  Label: 0
  end record; -- Quick_help_tab_gui_Type

  --  Dialog at resource line 458

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Quick_help_tab_gui_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Quick_help_tab_gui_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Quick_help_tab_gui_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Quick_help_tab_install_Type is new Window_Type with record

    --  Label: 0
    Static_0001 : Group_Box_Type;
    Static_0002 : Bitmap_Type;
    --  Label: 0
  end record; -- Quick_help_tab_install_Type

  --  Dialog at resource line 471

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Quick_help_tab_install_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Quick_help_tab_install_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Quick_help_tab_install_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Select_column_box_Type is new Window_Type with record

    Dummy_check_box_1 : Check_Box_Type;
    Dummy_check_box_2 : Check_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Select_column_box_Type

  --  Dialog at resource line 484

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Select_column_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Select_column_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Select displayed columns";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Select_column_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Sponsoring_box_Type is new Window_Type with record

    Sponsoring_label : Label_Type;
    --  Label: 0
    Label_Paypal : Label_Type;
    --  Label: 0
    Currency_box : Edit_Box_Type;
    Static_0001 : Bitmap_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Sponsoring_box_Type

  --  Dialog at resource line 501

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Sponsoring_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Sponsoring_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Sponsoring AZip";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Sponsoring_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Wait_refresh_box_Type is new Window_Type with record

    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    null;  --  empty!
  end record; -- Wait_refresh_box_Type

  --  Dialog at resource line 513

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Wait_refresh_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Please wait";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Wait_refresh_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Recompress_Box_Type is new Window_Type with record

    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    Recomp_Backup_Check_Box : Check_Box_Type;
    --  Label: IDC_STATIC
    ID_Recomp_Single_Pass : Default_Dialog_Button_Type;    --  Closes parent window after click
    ID_Recomp_Single_Pass_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    ID_Recomp_Brute_Force : Dialog_Button_Type;    --  Closes parent window after click
    ID_Recomp_Brute_Force_permanent : Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Recompress_Box_Type

  --  Dialog at resource line 529

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Recompress_Box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Recompress_Box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Archive Recompression";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Recompress_Box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Update_Box_Type is new Window_Type with record

    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    Update_Backup_Check_Box : Check_Box_Type;
    --  Label: IDC_STATIC
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Update_Box_Type

  --  Dialog at resource line 544

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Update_Box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Update_Box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Archive Update";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Update_Box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  package Version_info is
    Authors : constant String := "Gautier de Montmollin";
    FileDescription : constant String := "AZip - A portable Zip Archive Manager - Free, MIT license";
    FileVersion : constant String := "2.7.1";
    LegalCopyright : constant String := "Copyright � Gautier de Montmollin 2012 .. 2025";
    ProductName : constant String := "AZip";
    Translation : constant := 1033;
  end Version_info;

  --------------------------------------------------
  --  Defined resource symbols --> Ada constants  --
  --------------------------------------------------

  --  NB: only items with a defined symbol get a constant here
  --  These constants are needed for getting button and menu feedbacks.

  IDC_STATIC                             : constant :=     -1;
  About_box                              : constant :=  40000;
  Archive_Progress                       : constant :=  40001;
  AZip_Doc_Icon                          : constant :=  40002;
  AZip_Icon                              : constant :=  40003;
  AZip_URL                               : constant :=  40004;
  Binoculars_Icon                        : constant :=  40005;
  Cancel_button                          : constant :=  40006;
  Comment_1                              : constant :=  40007;
  Comment_2                              : constant :=  40008;
  Comp_ratio                             : constant :=  40009;
  Comp_size                              : constant :=  40010;
  Confirm_Icon                           : constant :=  40011;
  Confirm_Password_Label                 : constant :=  40012;
  Conflict_location                      : constant :=  40013;
  Conflict_simple_name                   : constant :=  40014;
  Content_to_be_searched                 : constant :=  40015;
  Copyright_label                        : constant :=  40016;
  Credits_box                            : constant :=  40017;
  Credits_button                         : constant :=  40018;
  Drag_Unpack_Icon                       : constant :=  40019;
  Drop_archive_name                      : constant :=  40020;
  Drop_files_box                         : constant :=  40021;
  Encrypt_check_box                      : constant :=  40022;
  Encrypted_entry                        : constant :=  40023;
  Entry_name                             : constant :=  40024;
  Entry_operation_name                   : constant :=  40025;
  File_exists_box                        : constant :=  40026;
  File_Progress                          : constant :=  40027;
  Find_box                               : constant :=  40028;
  Folders_BMP                            : constant :=  40029;
  GNAT_URL                               : constant :=  40030;
  GNAT_Version                           : constant :=  40031;
  GNAVI_URL                              : constant :=  40032;
  ID_Button_About_Azip                   : constant :=  40033;
  IDM_ABOUT                              : constant :=  40034;
  IDM_CLOSE_ARCHIVE                      : constant :=  40035;
  IDM_FLAT_VIEW                          : constant :=  40036;
  IDM_NEW_ARCHIVE                        : constant :=  40037;
  IDM_OPEN_ARCHIVE                       : constant :=  40038;
  IDM_QUIT                               : constant :=  40039;
  IDM_RECOMPRESS_ARCHIVE                 : constant :=  40040;
  IDM_TREE_VIEW                          : constant :=  40041;
  IDM_ADD_FILES                          : constant :=  40042;
  IDM_Add_Files_Encryption               : constant :=  40043;
  IDM_Add_Folder                         : constant :=  40044;
  IDM_Add_Folder_Encryption              : constant :=  40045;
  IDM_AZip_Web_news                      : constant :=  40046;
  IDM_COMPARE_ARCHIVES                   : constant :=  40047;
  IDM_Delete_selected                    : constant :=  40048;
  IDM_Encrypt_Archive                    : constant :=  40049;
  IDM_EXTRACT                            : constant :=  40050;
  IDM_FIND_IN_ARCHIVE                    : constant :=  40051;
  IDM_MERGE_ARCHIVES                     : constant :=  40052;
  IDM_MRU_1                              : constant :=  40053;
  IDM_MRU_2                              : constant :=  40054;
  IDM_MRU_3                              : constant :=  40055;
  IDM_MRU_4                              : constant :=  40056;
  IDM_MRU_5                              : constant :=  40057;
  IDM_MRU_6                              : constant :=  40058;
  IDM_MRU_7                              : constant :=  40059;
  IDM_MRU_8                              : constant :=  40060;
  IDM_MRU_9                              : constant :=  40061;
  IDM_Properties                         : constant :=  40062;
  IDM_Quick_Help                         : constant :=  40063;
  IDM_SAVE_ARCHIVE_AS                    : constant :=  40064;
  IDM_Select_all                         : constant :=  40065;
  IDM_TEST_ARCHIVE                       : constant :=  40066;
  IDM_Touch_Time_Stamps                  : constant :=  40067;
  IDM_Unselect_all                       : constant :=  40068;
  IDM_UPDATE_ARCHIVE                     : constant :=  40069;
  IDM_Web                                : constant :=  40070;
  IDM_WINDOW_CASCADE                     : constant :=  40071;
  IDM_WINDOW_CLOSE_ALL                   : constant :=  40072;
  IDM_WINDOW_TILE_HORIZONTAL             : constant :=  40073;
  IDM_WINDOW_TILE_VERTICAL               : constant :=  40074;
  Ini_files_URL                          : constant :=  40075;
  Key_Icon                               : constant :=  40076;
  Menu_MDI_Child                         : constant :=  40077;
  Menu_MDI_Main                          : constant :=  40078;
  Name_to_be_searched                    : constant :=  40079;
  New_archive_msg                        : constant :=  40080;
  Numb_entries                           : constant :=  40081;
  Overwrite_All                          : constant :=  40082;
  Overwrite_No                           : constant :=  40083;
  Overwrite_None                         : constant :=  40084;
  Overwrite_Rename                       : constant :=  40085;
  Overwrite_Yes                          : constant :=  40086;
  Password_confirm_edit                  : constant :=  40087;
  Password_decryption_box                : constant :=  40088;
  Password_edit                          : constant :=  40089;
  Password_encryption_box                : constant :=  40090;
  Plus_icon                              : constant :=  40091;
  Progress_box                           : constant :=  40092;
  Properties_box                         : constant :=  40093;
  Quick_help_box                         : constant :=  40094;
  ResEdit_URL                            : constant :=  40095;
  Show_password_box                      : constant :=  40096;
  Stats_list                             : constant :=  40097;
  Toolbar_BMP                            : constant :=  40098;
  Uncomp_size                            : constant :=  40099;
  Version_label                          : constant :=  40100;
  Wait_refresh_box                       : constant :=  40101;
  ZipAda_URL                             : constant :=  40102;
  ZipAda_Version                         : constant :=  40103;
  IDM_Toggle_Flat_Tree_View              : constant :=  40104;
  Fake_menu_for_commands_in_no_real_menu : constant :=  40105;
  IDM_No_sorting                         : constant :=  40106;
  Select_column_box                      : constant :=  40107;
  IDM_Select_columns                     : constant :=  40108;
  Dummy_check_box_1                      : constant :=  40109;
  Dummy_check_box_2                      : constant :=  40110;
  IDM_General_options                    : constant :=  40111;
  Option_box                             : constant :=  40112;
  Extract_Directory_Edit_Box             : constant :=  40113;
  IDM_Up_one_level                       : constant :=  40114;
  IDM_Context_menu_key                   : constant :=  40115;
  Choose_Extract_Directory_Button        : constant :=  40116;
  Quick_help_tab_gui                     : constant :=  40117;
  Quick_help_tab_install                 : constant :=  40118;
  Quick_help_tab_command                 : constant :=  40119;
  ZA_console_BMP                         : constant :=  40122;
  No_regedit_BMP                         : constant :=  40123;
  Donate_BMP                             : constant :=  40124;
  Sponsoring_box                         : constant :=  40125;
  Label_Paypal                           : constant :=  40126;
  IDM_Sponsoring                         : constant :=  40127;
  Sponsoring_button                      : constant :=  40128;
  Sponsoring_label                       : constant :=  40129;
  Currency_box                           : constant :=  40130;
  Install_box                            : constant :=  40131;
  Check_box_installed_all_users          : constant :=  40132;
  Check_box_installed_current_user       : constant :=  40133;
  Check_box_not_installed                : constant :=  40134;
  Radio_button_registry                  : constant :=  40135;
  Radio_button_stealth                   : constant :=  40136;
  Button_create_shortcut                 : constant :=  40137;
  Button_context_add                     : constant :=  40138;
  Button_extension_choose                : constant :=  40139;
  ID_Install_all_users                   : constant :=  40140;
  ID_Install_current_user                : constant :=  40141;
  Group_box_exe_location                 : constant :=  40142;
  Group_box_pref_location                : constant :=  40143;
  Group_box_Desktop_Explorer_integration : constant :=  40144;
  Label_Install_note_first_visit         : constant :=  40145;
  IDM_Install                            : constant :=  40146;
  Label_Installed_All_Users              : constant :=  40147;
  Label_Installed_Current_User           : constant :=  40148;
  Label_NOT_Installed                    : constant :=  40149;
  Label_Registry                         : constant :=  40150;
  Label_Stealth                          : constant :=  40151;
  IDM_Invert_Selection                   : constant :=  40152;
  Show_all_Formats                       : constant :=  40153;
  Percent_Progress                       : constant :=  40154;
  IDM_Open_Containing_Folder             : constant :=  40155;
  Temp_Directory_Edit_Box                : constant :=  40156;
  Choose_Temp_Directory_Button           : constant :=  40157;
  Extract_Directory_Group_Box            : constant :=  40158;
  Temp_Directory_Group_Box               : constant :=  40159;
  ID_Recomp_Single_Pass                  : constant :=  40160;
  ID_Recomp_Brute_Force                  : constant :=  40161;
  Recomp_Backup_Check_Box                : constant :=  40162;
  Update_Backup_Check_Box                : constant :=  40163;

  --  ** Some helper utilities (spec).

  procedure Dlg_to_Scn
    (xd, yd, wd, hd :  in Integer;
     xs, ys, ws, hs : out Integer);

  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource (id : Natural) return GString;  --  Just turn 123 into "#123".

  --  Last line of resource script file: 660

end AZip_Resource_GUI;
