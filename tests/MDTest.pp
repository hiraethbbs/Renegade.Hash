{$mode objfpc}
{$codepage utf8}
{$h+}
Program MDTest;

Uses
  Hash.Md,
  SysUtils,
  DateUtils,
  Classes;
 
begin
	Writeln('MD2 : ', Md.md2(DateTimeToStr(Now)));
	Writeln('MD4 : ', Md.md4(DateTimeToStr(Now)));
	Writeln('MD5 : ', Md.md5(DateTimeToStr(Now)));
end.