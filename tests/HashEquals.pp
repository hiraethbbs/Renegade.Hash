Program test;

Uses
   SysUtils,
   Classes,
   Renegade.Hash;
 var
   R : Boolean;  
 begin
   Writeln(RTHash.Base64.Encode('testing'));
   R := RTHash.HashEquals('881c7d6ba98678bcd96e253086c4048c3ea15306d0d13ff48341c6285ee71102a47b6f16e20e4d65c0c3d677be689dfda6d326695609cbadfafa1800e9eb7fc1','881c7d6ba98678bcd96e253086c4048c3ea15306d0d13ff48341c6285ee71102a47b6f16e20e4d65c0c3d677be689dfda6d326695609cbadfafa1800e9eb7fc1');
   Writeln(R);
end.