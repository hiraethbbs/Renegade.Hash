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
{$mode objfpc}
{$codepage utf8}
{$h+}

unit Hash.Sha;

interface

uses
  SysUtils,
  Classes,
  OpenSSL,
  CTypes;

const
  SHA512_DIGEST_LENGTH = 64;
  SHA384_DIGEST_LENGTH = 48;
  SHA256_DIGEST_LENGTH = 32;
  SHA224_DIGEST_LENGTH = 28;

type
  RTSha = class(TObject)
  public
    constructor Create();
    destructor Destroy();override;
    function Sha1(S: UTF8String): ansistring;
    function Sha224(S: UTF8String): ansistring;
    function Sha256(S: UTF8String): ansistring;
    function Sha384(S: UTF8String): ansistring;
    function Sha512(S: UTF8String): ansistring;
  end;

implementation

constructor RTSha.Create();
var
  HaveOpenSSL : Boolean;
begin
  HaveOpenSSL := InitSSLInterface;
  if not HaveOpenSSL then
  begin
    raise Exception.Create('Please install OpenSSL.') at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
    Fail;
  end;
end;

destructor RTSha.Destroy();
begin
  inherited Destroy;
  EVPcleanup;
  DestroySSLInterface;
end;

function RTSha.Sha1(S: UTF8String): ansistring;
var
  Digest : PEVP_MD;
  ShaCTX : PEVP_MD_CTX;
  HexValue : AnsiString;
  BinValue : PChar;
  Hash : PByte;
  DigestLength: pcuint;
begin

  GetMem(Hash, SHA_DIGEST_LENGTH);
  GetMem(DigestLength, SHA_DIGEST_LENGTH);
  GetMem(ShaCTX, SizeOf(PEVP_MD_CTX));
  GetMem(BinValue, SHA_DIGEST_LENGTH*2);
  SetLength(HexValue, SHA_DIGEST_LENGTH*2);

  try
     Digest := EvpGetDigestByName('sha1');
     EVP_DigestInit(ShaCTX, Digest);
     EVP_DigestUpdate(ShaCTX, @S[1], Length(S));
     EVP_DigestFinal(ShaCTX, Hash, DigestLength);
  except

   On e: Exception do
      begin
           WriteLn(e.Message);
           Writeln(e.HelpContext);
           Free;
           exit;
      end;
   end;

  Move(Hash[0], BinValue[0], SHA_DIGEST_LENGTH);

  BinToHex(BinValue, PChar(HexValue), SHA_DIGEST_LENGTH);
  // Cleanup
  FreeMem(Hash);
  FreeMem(DigestLength);
  FreeMem(BinValue);
  Result := LowerCase(HexValue);
end;

function RTSha.Sha224(S: UTF8String): ansistring;
var
  Digest : PEVP_MD;
  ShaCTX : PEVP_MD_CTX;
  HexValue : AnsiString;
  BinValue : PChar;
  Hash : PByte;
  DigestLength: pcuint;
begin

  GetMem(Hash, SHA224_DIGEST_LENGTH);
  GetMem(DigestLength, SHA224_DIGEST_LENGTH);
  GetMem(ShaCTX, SizeOf(PEVP_MD_CTX));
  GetMem(BinValue, SHA224_DIGEST_LENGTH);
  SetLength(HexValue, SHA224_DIGEST_LENGTH*2);
  try
     Digest := EvpGetDigestByName('sha224');
     EVP_DigestInit(ShaCTX, Digest);
     EVP_DigestUpdate(ShaCTX, @S[1], Length(S));
     EVP_DigestFinal(ShaCTX, Hash, DigestLength);
  except

   On e: Exception do
      begin
           WriteLn(e.Message);
           Writeln(e.HelpContext);
           Free;
           exit;
      end;
  end;

  Move(Hash[0], BinValue[0], SHA224_DIGEST_LENGTH);

  BinToHex(BinValue, PChar(HexValue), SHA224_DIGEST_LENGTH);
  // Cleanup
  FreeMem(Hash);
  FreeMem(DigestLength);
  FreeMem(BinValue);

  Result := LowerCase(HexValue);
end;

function RTSha.Sha256(S: UTF8String): ansistring;
var
  Digest : PEVP_MD;
  ShaCTX : PEVP_MD_CTX;
  HexValue : AnsiString;
  BinValue : PChar;
  Hash : PByte;
  DigestLength: pcuint;
begin

  GetMem(Hash, SHA256_DIGEST_LENGTH);
  GetMem(DigestLength, SHA256_DIGEST_LENGTH);
  GetMem(ShaCTX, SizeOf(PEVP_MD_CTX));
  GetMem(BinValue, SHA256_DIGEST_LENGTH);
  SetLength(HexValue, SHA256_DIGEST_LENGTH*2);
  try
     Digest := EvpGetDigestByName('sha256');
     EVP_DigestInit(ShaCTX, Digest);
     EVP_DigestUpdate(ShaCTX, @S[1], Length(S));
     EVP_DigestFinal(ShaCTX, Hash, DigestLength);
  except

   On e: Exception do
      begin
           WriteLn(e.Message);
           Writeln(e.HelpContext);
           Free;
           exit;
      end;
  end;

  Move(Hash[0], BinValue[0], SHA256_DIGEST_LENGTH);

  BinToHex(BinValue, PChar(HexValue), SHA256_DIGEST_LENGTH);
  // Cleanup
  FreeMem(Hash);
  FreeMem(DigestLength);
  FreeMem(BinValue);

  Result := LowerCase(HexValue);

end;

function RTSha.Sha384(S: UTF8String): ansistring;
var
  Digest : PEVP_MD;
  ShaCTX : PEVP_MD_CTX;
  HexValue : AnsiString;
  BinValue : PChar;
  Hash : PByte;
  DigestLength: pcuint;
begin

  GetMem(Hash, SHA384_DIGEST_LENGTH);
  GetMem(DigestLength, SHA384_DIGEST_LENGTH);
  GetMem(ShaCTX, SizeOf(PEVP_MD_CTX));
  GetMem(BinValue, SHA384_DIGEST_LENGTH);
  SetLength(HexValue, SHA384_DIGEST_LENGTH*2);
  try
     Digest := EvpGetDigestByName('sha384');
     EVP_DigestInit(ShaCTX, Digest);
     EVP_DigestUpdate(ShaCTX, @S[1], Length(S));
     EVP_DigestFinal(ShaCTX, Hash, DigestLength);
  except

   On e: Exception do
      begin
           WriteLn(e.Message);
           Writeln(e.HelpContext);
           Free;
           exit;
      end;
  end;

  Move(Hash[0], BinValue[0], SHA384_DIGEST_LENGTH);

  BinToHex(BinValue, PChar(HexValue), SHA384_DIGEST_LENGTH);
  // Cleanup
  FreeMem(Hash);
  FreeMem(DigestLength);
  FreeMem(BinValue);

  Result := LowerCase(HexValue);

end;

function RTSha.Sha512(S: UTF8String): ansistring;
var
  Digest : PEVP_MD;
  ShaCTX : PEVP_MD_CTX;
  HexValue : AnsiString;
  BinValue : PChar;
  Hash : PByte;
  DigestLength: pcuint;
begin

  GetMem(Hash, SHA512_DIGEST_LENGTH);
  GetMem(DigestLength, SHA512_DIGEST_LENGTH);
  GetMem(ShaCTX, SizeOf(PEVP_MD_CTX));
  GetMem(BinValue, SHA512_DIGEST_LENGTH);
  SetLength(HexValue, SHA512_DIGEST_LENGTH*2);
  try
     Digest := EvpGetDigestByName('sha512');
     EVP_DigestInit(ShaCTX, Digest);
     EVP_DigestUpdate(ShaCTX, @S[1], Length(S));
     EVP_DigestFinal(ShaCTX, Hash, DigestLength);
  except

   On e: Exception do
      begin
           WriteLn(e.Message);
           Writeln(e.HelpContext);
           Free;
           exit;
      end;
  end;

  Move(Hash[0], BinValue[0], SHA512_DIGEST_LENGTH);

  BinToHex(BinValue, PChar(HexValue), SHA512_DIGEST_LENGTH);
  // Cleanup
  FreeMem(Hash);
  FreeMem(DigestLength);
  FreeMem(BinValue);

  Result := LowerCase(HexValue);

end;

end.
