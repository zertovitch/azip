--  Testing how exactly the folders are expanded into a file list (including subfolder).
--  FSF GNAT MingGW 4.7.2 seems to include folders instead of only files.
--
--  Shows names in argument not expanded, then expanded.
--  Windows: use Drag & Drop on the executable.

with AZip_Common;                       use AZip_Common;
with AZip_Common.Operations;            use AZip_Common.Operations;

with Zip;                               use Zip;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;                  use Ada.Wide_Text_IO;

procedure Test_Expand_folders is

  l1: Name_list(1..Argument_Count);

  procedure Show(l: Name_list; title: Wide_String) is
  begin
    Put_Line(title);
    New_Line;
    for i in l'Range loop
      Put_Line("   " & To_Wide_String(l(i).str));
    end loop;
    New_Line;
  end;

begin
  for i in l1'Range loop
    l1(i).str:= To_Unbounded_Wide_String(To_UTF_16(Argument(i), IBM_437));
  end loop;
  Show(l1, "1) Not expanded");
  declare
    l2: constant Name_list:= Expand_folders(l1);
  begin
    Show(l2, "2) Expanded");
  end;
  Put("Press [return]");
  Skip_Line;
end;
