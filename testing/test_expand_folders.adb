--  Testing how exactly the folders are expanded into a file list (including subfolder).
--  FSF GNAT MingGW 4.7.2 seems to include folders instead of only files.
--
--  Shows names in argument not expanded, then expanded.
--  Windows: use Drag & Drop on the executable.

with AZip_Common.Operations;

with Zip;

with Ada.Command_Line,
     Ada.Strings.Wide_Unbounded,
     Ada.Wide_Text_IO;

procedure Test_Expand_folders is
  use AZip_Common, AZip_Common.Operations;
  use Ada.Command_Line, Ada.Strings.Wide_Unbounded, Ada.Wide_Text_IO;

  l1 : Name_List (1 .. Argument_Count);

  procedure Show (l : Name_List; title : Wide_String) is
  begin
    Put_Line (title);
    New_Line;
    for i in l'Range loop
      Put_Line ("   " & To_Wide_String (l (i).str));
    end loop;
    New_Line;
  end Show;

begin
  for i in l1'Range loop
    l1 (i).str := To_Unbounded_Wide_String (To_UTF_16 (Argument (i), Zip.IBM_437));
  end loop;
  Show (l1, "1) Not expanded");
  declare
    l2 : constant Name_List := Expand_Folders (l1);
  begin
    Show (l2, "2) Expanded");
  end;
  Put ("Press [return]");
  Skip_Line;
end Test_Expand_folders;
