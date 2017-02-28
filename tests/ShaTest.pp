{$codepage utf8}
{$h+}{$mode objfpc}
Program ShaTest;

Uses
	SysUtils,
	Classes,
	Hash.Sha;
var
   Hash : RTSha;	
   S : UTF8String;	
begin
 Hash := RTSha.Create;
 S := 'testing';
 WriteLn(Hash.Sha1(S));
 Writeln(Hash.Sha224(S));
 Writeln(Hash.Sha256(S));
 Writeln(Hash.Sha384(S));
 Writeln(Hash.Sha512(S));
 Hash.Free;
 Exit;
end.