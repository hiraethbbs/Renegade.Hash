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
{$codepage utf8}
{$h+}
{$mode objfpc}
unit Hash.Md;

interface

uses
  md5,
  SysUtils,
  Classes;

type
  Md = class(TObject)
  public
    function md2(S: UTF8String): ansistring;
    function md4(S: UTF8String): ansistring;
    function md5(S: UTF8STring): ansistring;
  end;

implementation

function Md.md2(S: UTF8String): ansistring;
begin
  Result := MD2Print(MDString(S, MD_VERSION_2));
end;

function Md.md4(S: UTF8String): ansistring;
begin
  Result := MD4Print(MDString(S, MD_VERSION_4));
end;

function Md.md5(S: UTF8String): ansistring;
begin
  Result := MD5Print(MDString(S, MD_VERSION_5));
end;

end.
