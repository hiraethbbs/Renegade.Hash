Program Base64Test;

{$mode objfpc}{$H+}
{$codepage utf8}


uses
  Classes, SysUtils, Hash.Base64, Math;
var
  TestString, Teststring2 : UTF8String;

begin

  TestString := 'Humans are creative beings. People tend to read writing. This text will not appear in a consistent order. After Hours Programming created this application. Default text is for web developers and designers that need default text quickly. Thank you for using this application. Your design looks awesome by the way. JavaScript has the awesome power to manipulate DOM elements on the fly. This string is randomly  generated. I hope you enjoyed the fake text.';
  TestString2 := 'Humans are creative beings. People tend to read writing. This text will not appear in a consistent order. After Hours Programming created this application. Default text is for web developers and designers that need default text quickly. Thank you for using this application. Your design looks awesome by the way. JavaScript has the awesome power to manipulate DOM elements on the fly. This string is randomly  generated. I hope you enjoyed the fake text.';
  writeln(RTBase64.Encode(TestString));
  writeln(RTBase64.Decode('SHVtYW5zIGFyZSBjcmVhdGl2ZSBiZWluZ3MuIFBlb3BsZSB0ZW5kIHRvIHJlYWQgd3JpdGluZy4gVGhpcyB0ZXh0IHdpbGwgbm90IGFwcGVhciBpbiBhIGNvbnNpc3RlbnQgb3JkZXIuIEFmdGVyIEhvdXJzIFByb2dyYW1taW5nIGNyZWF0ZWQgdGhpcyBhcHBsaWNhdGlvbi4gRGVmYXVsdCB0ZXh0IGlzIGZvciB3ZWIgZGV2ZWxvcGVycyBhbmQgZGVzaWduZXJzIHRoYXQgbmVlZCBkZWZhdWx0IHRleHQgcXVpY2tseS4gVGhhbmsgeW91IGZvciB1c2luZyB0aGlzIGFwcGxpY2F0aW9uLiBZb3VyIGRlc2lnbiBsb29rcyBhd2Vzb21lIGJ5IHRoZSB3YXkuIEphdmFTY3JpcHQgaGFzIHRoZSBhd2Vzb21lIHBvd2VyIHRvIG1hbmlwdWxhdGUgRE9NIGVsZW1lbnRzIG9uIHRoZSBmbHkuIFRoaXMgc3RyaW5nIGlzIHJhbmRvbWx5ICBnZW5lcmF0ZWQuIEkgaG9wZSB5b3UgZW5qb3llZCB0aGUgZmFrZSB0ZXh0Lg=='));
  writeln(RTBase64.BSDEncode(TestString2));
  writeln(RTBase64.BSDDecode('QFTrWU3xGEDwXQ/hakTfbEj0XQ/gXUjsX1KsGD/jZ1/qXQ/yXU3iGFPtGFHjWUOeb1HnbEjsXw2eTEfnaw/yXVfyGFbnZEueZk7yGEDuaETfag/nZg/fGELtZlLna1PjZlOeZ1HiXVGsGCDkbETwGCftbVHxGD/wZ0bwWUzrYU3lGELwXUDyXUOebEfnaw/faF/qYULfbEjtZg2ePETkWVTqbA/yXVfyGEjxGEXtag/1XUGeXET0XUvtaETwaw/fZkOeXETxYUbsXVHxGFPmWVOeZkTjXA/iXUXfbUvyGFPjcFOeaVTnW0rqcQ2eTEffZkqecU7zGEXtag/za0jsXw/yYEjxGEDuaEvnW0DyYU7sJg/XZ1TwGEPja0jlZg/qZ07paw/fb0TxZ0zjGEH3GFPmXQ/1WVisGCnfbkDRW1HnaFOeYEDxGFPmXQ/fb0TxZ0zjGF/tb0TwGFPtGEzfZkjubUvfbESePC7LGETqXUzjZlPxGE7sGFPmXQ/kZFisGDPmYVKea1PwYU3lGEjxGFHfZkPtZUv3GA/lXU3jakDyXUOsGCieYE7uXQ/3Z1SeXU3oZ1jjXA/yYESeXkDpXQ/yXVfyJe.'));
end.

