with Ada.Text_IO;

package body AZip_GWin is

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean is
    --  Copied from LEA.
    --  Possibly overkill in AZip's context.
    --
    F1 : GString := GU2G (Id_1.file_name);
    F2 : GString := GU2G (Id_2.file_name);
    S1 : GString := GU2G (Id_1.short_name);
    S2 : GString := GU2G (Id_2.short_name);
    trace : constant Boolean := False;
    result : Boolean;
    use Ada.Text_IO;
  begin
    if trace then
      Put_Line ("F1 = [" & G2S (F1) & ']');
      Put_Line ("F2 = [" & G2S (F2) & ']');
      Put_Line ("S1 = [" & G2S (S1) & ']');
      Put_Line ("S2 = [" & G2S (S2) & ']');
    end if;
    if F1 = "" or else F2 = "" then
      To_Upper (S1);
      To_Upper (S2);
      result := S1 = S2;
    else
      To_Upper (F1);
      To_Upper (F2);
      result := F1 = F2;
    end if;
    if trace then
      Put_Line ("Equivalent: " & result'Image);
    end if;
    return result;
  end Equivalent;

  function Simple_Name (path : GString) return GString is
    start : Natural := path'First;
  begin
    for i in reverse path'Range loop
      if path (i) = '\' then
        start := i + 1;
        exit;
      end if;
    end loop;
    return path (start .. path'Last);
  end Simple_Name;

end AZip_GWin;
