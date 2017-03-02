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

unit Renegade.Hash;
{$codepage utf8}
{$h+}{$mode objfpc}

interface

uses
  Classes,
  SysUtils,
  Hash.Md,
  Hash.Sha,
  Hash.Base64,
  Hash.Util,
  Hash.BCrypt;

type
  TBCrypt = object
     public
       function Hash(const Password : AnsiString) : AnsiString; overload;static;
       function Hash(const Password : AnsiString; HashType : THashTypes) : AnsiString; overload;
       function Hash(const Password : AnsiString; HashType : THashTypes; Cost : Byte) : AnsiString; overload;
       function Verify(const Password, BCryptHash : AnsiString) : Boolean;
       function NeedsRehash(const BCryptHash : AnsiString) : Boolean; overload;
       function NeedsRehash(const BCryptHash : AnsiString; Cost : Byte) : Boolean; overload;
       function GetInfo(const BCryptHash : AnsiString) : RTPasswordInformation;
  end;

  RTHash = object
    public
      Base64 : RTBase64;static;
      Sha : RTSha;static;
      Md : RTMd;static;
      Util : RTHashUtil;static;
      BCrypt : TBCrypt;static;
      function HashEquals(KnownHash, CheckedHash : ansistring): boolean;static;
  end;

implementation

{ TBCrypt }
function TBCrypt.Hash(const Password : AnsiString) : AnsiString; overload;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.CreateHash(Password, bcPHP, BCRYPT_DEFAULT_COST);
  BCrypt.Free;
end;

function TBCrypt.Hash(const Password : AnsiString; HashType : THashTypes) : AnsiString; overload;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.CreateHash(Password, HashType, BCRYPT_DEFAULT_COST);
  BCrypt.Free;
end;

function TBCrypt.Hash(const Password : AnsiString; HashType : THashTypes; Cost : Byte) : AnsiString; overload;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.CreateHash(Password, HashType, Cost);
  BCrypt.Free;
end;

function TBCrypt.Verify(const Password, BCryptHash : AnsiString) : Boolean;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.VerifyHash(Password, BCryptHash);
  BCrypt.Free;
end;

function TBCrypt.NeedsRehash(const BCryptHash : AnsiString) : Boolean; overload;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.NeedsRehash(BCryptHash, BCRYPT_DEFAULT_COST);
  BCrypt.Free;
end;

function TBCrypt.NeedsRehash(const BCryptHash : AnsiString; Cost : Byte) : Boolean; overload;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.NeedsRehash(BCryptHash, Cost);
  BCrypt.Free;

end;

function TBCrypt.GetInfo(const BCryptHash : AnsiString) : RTPasswordInformation;
var
  BCrypt : RTBCrypt;
begin
  BCrypt := RTBCrypt.Create;
  Result := BCrypt.HashGetInfo(BCryptHash);
  BCrypt.Free;
end;

function RTHash.HashEquals(KnownHash, CheckedHash : ansistring): boolean;
begin
      Result := Util.HashEquals(KnownHash, CheckedHash);
end;

end.


