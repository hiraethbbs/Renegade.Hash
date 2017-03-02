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
{                                                       }
{  This unit contains code from the HashLib Library     }
{  by Ugochukwu Mmaduekwe licensed under the MIT        }
{  license. (https://github.com/Xor-el/HashLib4Pascal)  }
{                                                       }
{*******************************************************}
{                                                       }
{                    HashLib Library                    }
{     Copyright (c) Ugochukwu Mmaduekwe 2016 - 2017     }
{                                                       }
{*******************************************************}
{                                                       }
{ The MIT License (MIT)                                 }
{                                                       }
{ Copyright (c) 2016 Ugochukwu Mmaduekwe                }
{                                                       }
{ Permission is hereby granted, free of charge, to any  }
{ person obtaining a copy of this software and          }
{ associated documentation files (the "Software"), to   }
{ deal in the Software without restriction, including   }
{ without limitation the rights to use, copy, modify,   }
{ merge, publish, distribute, sublicense, and/or sell   }
{ copies of the Software, and to permit persons to whom }
{ the Software is furnished to do so, subject to the    }
{ following conditions:                                 }
{                                                       }
{ The above copyright notice and this permission notice }
{ shall be included in all copies or substantial        }
{ portions of the Software.                             }
{                                                       }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF }
{ ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT       }
{ LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS }
{ FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO   }
{ EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE       }
{ LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,     }
{ WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,  }
{ ARISING FROM, OUT OF OR IN CONNECTION WITH THE        }
{ SOFTWARE OR THE USE OR OTHER DEALINGS IN THE          }
{ SOFTWARE.                                             }
{                                                       }
{*******************************************************}

{$mode objfpc}
{$codepage utf8}
{$h+}

unit Hash.Sha;

interface

uses
  SysUtils,
  Classes,
  HlpSHA0,
  HlpSHA1,
  HlpSHA2_224,
  HlpSHA2_256,
  HlpSHA2_384,
  HlpSHA2_512,
  HlpSHA2_512_224,
  HlpSHA2_512_256,
  HlpSHA3;


type
  RTSupportedShaHashTypes = (
    rtSha0,
    rtSha1,
    rtSha224,
    rtSha256,
    rtSha384,
    rtSha512_224,
    rtSha512_256,
    rtSha512,
    rtSha3_224,
    rtSha3_256,
    rtSha3_384,
    rtSha3_512
    );

  RTSupportedShaHashTypesSet = set of RTSupportedShaHashTypes;

  RTSha = object
  public
    function CreateSha(S : UTF8String; ShaType : RTSupportedShaHashTypes): ansistring; static;
    function Sha(S: UTF8String): ansistring; static;
    function Sha0(S: UTF8String): ansistring; static;
    function Sha1(S: UTF8String): ansistring; static;
    function Sha224(S: UTF8String): ansistring; static;
    function Sha256(S: UTF8String): ansistring; static;
    function Sha384(S: UTF8String): ansistring; static;
    function Sha512(S: UTF8String): ansistring; static;
    function Sha512_224(S: UTF8String): ansistring; static;
    function Sha512_256(S: UTF8String): ansistring; static;
    function Sha3_224(S: UTF8String): ansistring; static;
    function Sha3_256(S: UTF8String): ansistring; static;
    function Sha3_384(S: UTF8String): ansistring; static;
    function Sha3_512(S: UTF8String): ansistring; static;
  end;

implementation

function RTSha.CreateSha(S : UTF8String; ShaType : RTSupportedShaHashTypes) : ansistring;
begin
  case ShaType of
    rtSha0 : Result := Sha(S);
    rtSha1 : Result := Sha1(S);
    rtSha224 : Result := Sha224(S);
    rtSha256 : Result := Sha256(S);
    rtSha384 : Result := Sha384(S);
    rtSha512_224 : Result := Sha512_224(S);
    rtSha512_256 : Result := Sha512_256(S);
    rtSha512 : Result := Sha512(S);
    rtSha3_224 : Result := Sha3_224(S);
    rtSha3_256 : Result := Sha3_256(S);
    rtSha3_384 : Result := Sha3_384(S);
    rtSha3_512  : Result := Sha3_512(S);
  end;

end;

function RTSha.Sha(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA0.Create().ComputeString(PChar(S), TEncoding.UTF8).ToString());
end;

function RTSha.Sha0(S: UTF8String): ansistring;
begin
  Result := Sha(S);
end;

function RTSha.Sha1(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA1.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha224(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_224.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha256(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_256.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha384(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_384.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha512(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_512.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha512_224(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_512_224.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;


function RTSha.Sha512_256(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA2_512_256.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha3_224(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA3_224.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha3_256(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA3_256.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha3_384(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA3_384.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

function RTSha.Sha3_512(S: UTF8String): ansistring;
begin
  Result := System.LowerCase(TSHA3_512.Create().ComputeString(PChar(S),
    TEncoding.UTF8).ToString());
end;

end.
