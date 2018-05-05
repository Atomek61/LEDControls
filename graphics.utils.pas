unit Graphics.Utils;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

function InterpolateColor(Col1, Col2 :TColor; f1 :single) :TColor; // only "natural" colors (highbyte=0)

implementation

type
  TRgb = packed record
    case integer of
      0: (AsUint32 :UInt32);
      1: (r, g, b, a :byte);
      2: (AsColor :TColor);
  end;

function InterpolateColor(Col1, Col2 :TColor; f1 :single) :TColor;
var
  c1, c2 :TRgb;
begin
  c1 := TRgb(Col1);
  c2 := TRgb(Col2);
  result := 0;
  with TRgb(result) do begin
    r := Round(c1.r+(c2.r-c1.r)*f1);
    g := Round(c1.g+(c2.g-c1.g)*f1);
    b := Round(c1.b+(c2.b-c1.b)*f1);
  end;
end;

end.

