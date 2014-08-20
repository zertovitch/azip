with Zip_Streams;

function Zip_time_display(
  T        : Zip_Streams.Time;
  Seconds  : Boolean      := True;
  Intra_day: Boolean      := True
)
  return String
is
  Year, Month, Day: Integer;
  Seconds_in_day: Duration;
begin
  Zip_Streams.Calendar.Split(T, Year, Month, Day, Seconds_in_day);
  declare
    subtype Sec_int is Long_Integer; -- must contain 86_400
    s : constant Sec_int:= Sec_int( Seconds_in_day );
    m : constant Sec_int:= s / 60;
    -- + 100: trick for obtaining 0x
    sY : constant String:= Integer'Image( Year);
    sM : constant String:= Integer'Image( Month + 100);
    sD : constant String:= Integer'Image(  Day  + 100);
    shr: constant String:= Sec_int'Image( m  /  60 + 100);
    smn: constant String:= Sec_int'Image( m mod 60 + 100);
    ssc: constant String:= Sec_int'Image( s mod 60 + 100);
    --
    function Optional_seconds return String is
    begin
      if Seconds then
        return ':' & ssc( ssc'Last-1 .. ssc'Last );
      else
        return "";
      end if;
    end Optional_seconds;
    --
    function Optional_intra_day return String is
    begin
      if Intra_day then
        return
          "  " &
          shr( shr'Last-1 .. shr'Last ) & ':' &
          smn( smn'Last-1 .. smn'Last ) & Optional_seconds;
      else
        return "";
      end if;
    end Optional_intra_day;

  begin
    return
      sY( sY'Last-3 .. sY'Last ) & '/' &  -- not Year 10'000 compliant.
      sM( sM'Last-1 .. sM'Last ) & '/' &
      sD( sD'Last-1 .. sD'Last ) &
      Optional_intra_day;
  end;
end Zip_time_display;
