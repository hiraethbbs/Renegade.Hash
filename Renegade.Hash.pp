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
  Hash.Util;

type
  RTHash = object
    public
      Base64 : RTBase64;static;
      Sha : RTSha;static;
      Md : RTMd;static;
      Util : RTHashUtil;static;
      function HashEquals(KnownHash, CheckedHash : ansistring): boolean;static;
  end;

implementation

function RTHash.HashEquals(KnownHash, CheckedHash : ansistring): boolean;
begin
      Result := Util.HashEquals(KnownHash, CheckedHash);
end;

end.


