with Ada.Characters.Handling;           use Ada.Characters.Handling;

package body AZip_Common is

  function Image(topic: Entry_topic) return String is
  u: constant String:= Entry_topic'Image(topic);
  l: constant String:= To_Lower(u);
  begin
    case topic is
      when FType =>
        return "Type";
      when others =>
        return u(u'First) & l(l'First+1..l'Last);
    end case;
  end Image;

end AZip_Common;
