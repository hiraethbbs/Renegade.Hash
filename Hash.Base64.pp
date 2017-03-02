{*******************************************************}

{   Renegade BBS                                        }

{   Copyright (c) 1990-2013 The Renegade Dev Team       }
{   Copyleft  (ↄ) 2016-2017 Renegade BBS                }

{   This file is part of Renegade BBS                   }

{   Renegade is free software: you can redistribute it  }
{   and/or modify it under the terms of the GNU General }
{   Public License as published by the Free Software    }
{   Foundation, either version 3 of the License, or     }
{   (at your option) any later version.                 }

{   Renegade is distributed in the hope that it will be }
{   useful, but WITHOUT ANY WARRANTY; without even the  }
{   implied warranty of MERCHANTABILITY or FITNESS FOR  }
{   A PARTICULAR PURPOSE.  See the GNU General Public   }
{   License for more details.                           }

{   You should have received a copy of the GNU General  }
{   Public License along with Renegade.  If not, see    }
{   <http://www.gnu.org/licenses/>.                     }

{*******************************************************}
{   _______                                  __         }
{  |   _   .-----.-----.-----.-----.---.-.--|  .-----.  }
{  |.  l   |  -__|     |  -__|  _  |  _  |  _  |  -__|  }
{  |.  _   |_____|__|__|_____|___  |___._|_____|_____|  }
{  |:  |   |                 |_____|                    }
{  |::.|:. |                                            }
{  `--- ---'                                            }
{*******************************************************}

unit Hash.Base64;

{$codepage utf8}
{$h+}{$mode objfpc}
interface

uses
  Classes,
  SysUtils,
  Base64;

const
  BSDEncodeTable: array[0..63] of char =
    { 0:} './' +
    { 2:} 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    {28:} 'abcdefghijklmnopqrstuvwxyz' +
    {54:} '0123456789';

  BSDDecodeTable: array[#0..#127] of integer = (
    {  0:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    { 16:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    { 32:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1,
    { 48:} 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1,
    { 64:} -1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
    { 80:} 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1,
    { 96:} -1, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
    {113:} 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, -1, -1, -1, -1, -1
    );

type
  RTBase64 = object
  private
    function Char64(Character: AnsiChar): SizeInt;
    procedure AppendChar(var WorkingResult: TBytes; Value: byte);
  public
    function Encode(S: UTF8String): ansistring; static;
    function Decode(S: ansistring): UTF8String; static;
    function BSDEncode(S: UTF8STring): ansistring; static;
    function BSDDecode(S: ansistring): UTF8STring; static;
    function BSDEncodeBytes(const RawByteData: TBytes;
      CharacterLength: Sizeint): ansistring;
    function BSDDecodeBytes(const EncodedString: ansistring): TBytes;
  end;

implementation

function RTBase64.Char64(Character: AnsiChar): SizeInt;
begin
  if Ord(Character) > Length(BSDDecodeTable) then
  begin
    Result := -1;
  end
  else
  begin
    Result := BSDDecodeTable[Character];
  end;
end;

procedure RTBase64.AppendChar(var WorkingResult: TBytes; Value: byte);
var
  i: SizeUint;
begin
  i := Length(WorkingResult);
  SetLength(WorkingResult, i + 1);
  WorkingResult[i] := Value;
end;


function RTBase64.Encode(S: UTF8String): ansistring;
begin
  Result := EncodeStringBase64(S);
end;

function RTBase64.Decode(S: ansistring): UTF8String;
begin
  Result := DecodeStringBase64(S);
end;

function RTBase64.BSDEncode(S: UTF8String): ansistring;
var
  TBaseBytes: TBytes;
begin

  SetLength(TBaseBytes, Length(S) + 1);
  Move(S[1], TBaseBytes[0], Length(S) + 1);
  Result := RTBase64.BSDEncodeBytes(TBaseBytes, Length(TBaseBytes));
end;

function RTBase64.BSDDecode(S: ansistring): UTF8String;
var
  TBaseBytes: TBytes;
begin
  SetLength(TBaseBytes, Length(S) + 1);

  TBaseBytes := RTBase64.BSDDecodeBytes(S);
  SetLength(Result, Length(TBaseBytes));
  Move(TBaseBytes[0], Result[1], Length(TBaseBytes));
end;

function RTBase64.BSDEncodeBytes(const RawByteData: TBytes;
  CharacterLength: Sizeint): ansistring;
var
  i, b1, b2: SizeInt;
begin
  Result := '';
  if (CharacterLength <= 0) or (CharacterLength > Length(RawByteData)) then
  begin
    Exit;
  end;

  i := 0;
  while i < CharacterLength do
  begin
    b1 := RawByteData[i] and $ff;
    Inc(i);

    Result := Result + BSDEncodeTable[(b1 shr 2) and $3f];
    b1 := (b1 and $03) shl 4;
    if i >= CharacterLength then
    begin
      Result := Result + BSDEncodeTable[b1 and $3f];
      Exit;
    end;

    b2 := RawByteData[i] and $ff;
    Inc(i);
    b1 := b1 or ((b2 shr 4) and $0f);

    Result := Result + BSDEncodeTable[b1 and $3f];
    b1 := (b2 and $0f) shl 2;
    if i >= CharacterLength then
    begin
      Result := Result + BSDEncodeTable[b1 and $3f];
      Exit;
    end;

    b2 := RawByteData[i] and $ff;
    Inc(i);
    b1 := b1 or ((b2 shr 6) and $03);
    Result := Result + BSDEncodeTable[b1 and $3f];
    Result := Result + BSDEncodeTable[b2 and $3f];
  end;

end;

function RTBase64.BSDDecodeBytes(const EncodedString: ansistring): TBytes;
var
  i, EncodedStringLength, c1, c2, c3, c4: Sizeint;

begin
  SetLength(Result, 0);
  i := 1;
  EncodedStringLength := Length(EncodedString);
  while (i < EncodedStringLength) and (Length(Result) < EncodedStringLength) do
  begin
    c1 := self.Char64(EncodedString[i]);
    Inc(i);
    c2 := self.Char64(EncodedString[i]);
    Inc(i);
    if (c1 = -1) or (c2 = -1) then
    begin
      Exit;
    end;

      {
        Now we have at least one byte in c1|c2
        c1 = ..111111
        c2 = ..112222
      }
    self.AppendChar(Result, (c1 shl 2) or ((c2 and $30) shr 4));
    //If there's a 3rd character, then we can use c2|c3 to form the second byte
    if (i > EncodedStringLength) or (Length(Result) >= EncodedStringLength) then
    begin
      Break;
    end;

    c3 := self.Char64(EncodedString[i]);
    Inc(i);
    if (c3 = -1) then
    begin
      Exit;
    end;

      {
        Now we have the next byte in c2|c3
        c2 = ..112222
        c3 = ..222233
      }
    self.AppendChar(Result, ((c2 and $0f) shl 4) or ((c3 and $3c) shr 2));
    //If there's a 4th caracter, then we can use c3|c4 to form the third byte
    if (i > EncodedStringLength) or (Length(Result) >= EncodedStringLength) then
    begin
      Break;
    end;

    c4 := self.Char64(EncodedString[i]);
    Inc(i);
    if c4 = -1 then
    begin
      Exit;
    end;

      {
        Now we have the next byte in c3|c4
        c3 = ..222233
        c4 = ..333333
      }
    self.AppendChar(Result, ((c3 and $03) shl 6) or c4);
  end; { While }
end;

end.
