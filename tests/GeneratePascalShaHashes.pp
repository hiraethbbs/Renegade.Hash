{$codepage utf8}
{$h+}{$mode objfpc}
program GeneratePascalShaHashes;

uses
  SysUtils,
  Classes,
  fpJson,
  TestFramework,
  Hash.Sha;

  function ConstToString(const ShaConst: RTSupportedShaHashTypes): ansistring;
  begin
    case ShaConst of
      rtSha0: Result := 'Sha-Sha0';
      rtSha1: Result := 'Sha1';
      rtSha224: Result := 'Sha224';
      rtSha256: Result := 'Sha256';
      rtSha384: Result := 'Sha384';
      rtSha512_224: Result := 'Sha512-224';
      rtSha512_256: Result := 'Sha512-256';
      rtSha512: Result := 'Sha512';
      rtSha3_224: Result := 'Sha3-224';
      rtSha3_256: Result := 'Sha3-256';
      rtSha3_384: Result := 'Sha3-384';
      rtSha3_512: Result := 'Sha3-512';
      else

        Result := 'Unknown';
    end;
  end;

  function stripAlpha(S: ansistring): ansistring;
  var
    C: set of char;
    i: integer;
  begin

    C := ['a'..'z'];
    for i := length(S) downto 1 do
    begin
      if LowerCase(S[i]) in C then
      begin
        Delete(S, i, 1);
      end;
    end;

    if S = '-0' then
    begin
      Result := '0';
    end
    else
    begin
      Result := S;
    end;

  end;

var
  S: UTF8String;
  Hash, ConstString, JSONResult: ansistring;
  JObjectResult, JObject: TJSONOBject;
  ConstType: RTSupportedShaHashTypes;
  FileStream: TFileStream;
begin
  JObject := TJSONObject.Create;
  JObjectResult := TJSONObject.Create;
  S := 'testing';
  FileStream := TFileStream.Create('pascal.json', fmOpenWrite or fmCreate);

  for ConstType in RTSupportedShaHashTypesSet do
  begin

    ConstString := ConstToString(ConstType);
    Hash := RTSha.CreateSha(S, ConstType);

    JObject.Add('sha', LowerCase(StripAlpha(ConstString)));
    JObject.Add('length', Length(Hash));
    JObject.Add('word', S);
    JObject.Add('hash', Hash);

    JOBjectResult.Add(LowerCase(ConstString), JObject);
    JObject := TJSONObject.Create;
  end;
  FileStream.Seek(0, soBeginning);
  JSONResult := JObjectResult.FormatJSON(DefaultFormat, 4);

  FileStream.Write(JSONResult[1], Length(JSONResult));
  FileStream.Free;
  JObject.Free;
  JObjectResult.Free;


end.
