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

unit Hash.Util;
{$codepage utf8}
{$h+}{$mode objfpc}

interface

type
  RTHashUtil = object
    public
      function HashEquals(KnownHash, CheckedHash : ansistring): boolean;static;
  end;

implementation

function RTHashUtil.HashEquals(KnownHash, CheckedHash : ansistring): boolean;
var
  HashCounter,
  ResultStatus : SizeInt;
begin
      ResultStatus := 0;
      if Length(KnownHash) <> Length(CheckedHash) then
        begin
          Result := False;
          Exit;
        end;
      for HashCounter := 1 to Length(KnownHash) do
      begin
        {
          From ext/standard/password.c php_password_verify line 244
          We're using this method instead of = in order to provide
          resistance towards timing attacks. This is a constant time
          equality check that will always check every byte of both
          values.

         }
         ResultStatus := ResultStatus or
         (ord(CheckedHash[HashCounter]) xor
         ord(KnownHash[HashCounter]));

      end;

      Result := (ResultStatus = 0);
end;

end.
