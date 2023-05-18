-----------------------------------------------------------------------------------
--  Persistence using either the registry or a configuration file, if available  --
-----------------------------------------------------------------------------------

with AZip_Common.User_options;

with Windows_Persistence_IO;

package AZip_GWin.Persistence is

  package Key_IO is
    new Windows_Persistence_IO
      (app_display_name => "AZip",
       app_file_name    => "azip",
       app_url          => AZip_Common.azip_web_page,
       Persistence_Key  => AZip_Common.User_options.Persistence_Key);

  package Blockwise_IO is
    new AZip_Common.User_options.Persistence
      (Read_Key  => Key_IO.Read_Key,
       Write_Key => Key_IO.Write_Key);

end AZip_GWin.Persistence;
