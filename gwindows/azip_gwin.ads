with GWindows,
     GWindows.Common_Dialogs,
     GWindows.GStrings;

package AZip_GWin is

  use GWindows, GWindows.GStrings;

  function S2G (Value : String) return GString renames To_GString_From_String;
  function G2S (Value : GString) return String renames To_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

  NL : constant GString := S2G ((1 => ASCII.LF));

  Zip_archives_filters : GWindows.Common_Dialogs.Filter_Array :=
    ((G2GU ("Zip archives (*.zip)"),        G2GU ("*.zip")),
     (G2GU ("JAR (Java archives) (*.jar)"), G2GU ("*.jar")),
     (G2GU ("All files (*.*)"),             G2GU ("*.*")));

private

  timing : constant Boolean := False;

end AZip_GWin;
