{$codepage utf8}
{$h+}
{$mode objfpc}
Unit Hash.Md;

interface

Uses
	md5,
	SysUtils,
	Classes;

Type
	Md = class(TObject)
		public
		function md2(S : UTF8String) : AnsiString;
		function md4(S : UTF8String) : AnsiString;
		function md5(S : UTF8STring) : AnsiString;
	end;

implementation

function Md.md2(S : UTF8String) : AnsiString;
begin
  Result := MD2Print(MDString(S, MD_VERSION_2));
end;

function Md.md4(S : UTF8String) : AnsiString;
begin
  Result := MD4Print(MDString(S, MD_VERSION_4));
end;

function Md.md5(S : UTF8String) : AnsiString;
begin
  Result := MD5Print(MDString(S, MD_VERSION_5));
end;

End.