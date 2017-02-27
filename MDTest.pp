{$mode objfpc}
{$codepage utf8}
{$h+}
Program MDTest;

Uses
  Hash.Md,
  SysUtils,
  DateUtils,
  Classes;
 
var
	Hash : Md;
	
begin
Hash := Md.Create;
Writeln('MD2 : ', Hash.md2(DateTimeToStr(Now)));
Writeln('MD4 : ', Hash.md4(DateTimeToStr(Now)));
Writeln('MD5 : ', Hash.md5(DateTimeToStr(Now)));
Hash.Free;
end.