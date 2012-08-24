-- AZip elements that are common to all GUI systems / toolkits.

package AZip_Common is

  type View_Mode_Type is (Flat, Tree);

  type Entry_topic is (
    Name, FType, Modified, Attributes,
    Size, Packed, Ratio, Format, Path
  );

  function Image(topic: Entry_topic) return String;

  type Column_width_array is array(Entry_topic) of Natural;

  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Flat;
    column_width : Column_width_array:= (others => 100);
  end record;

end AZip_Common;
