unit Types.Led;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TColorSet = record
    Background :TColor;
    Bright :TColor;
    Dark :TColor;
  end;

const
  LEDBRIGHTRED      = clRed;
  LEDDARKRED        = $000040;
  LEDBKGNDRED       = clBlack;

  LEDBRIGHTGREEN    = $91FF96;
  LEDDARKGREEN      = $1E3019;
  LEDBKGNDGREEN     = clBlack;

  LEDBRIGHTYELLOW   = $0FE4F0;
  LEDDARKYELLOW     = $002040;
  LEDBKGNDYELLOW    = clBlack;

  LEDBRIGHTBLUE     = $FFFFA4;
  LEDDARKBLUE       = $303018;
  LEDBKGNDBLUE      = clBlack;

  LEDBRIGHTORANGE   = $54BFFE;
  LEDDARKORANGE     = $000810;
  LEDBKGNDORANGE    = clBlack;

  LEDBRIGHTWHITE    = $FEFEFE;
  LEDDARKWHITE      = $181818;
  LEDBKGNDWHITE     = clBlack;

  LEDBRIGHTLCD      = $000000;
  LEDDARKLCD        = $ABBEB9;
  LEDBKGNDLCD       = $B6CBC5;

  LEDBRIGHTNEGBLUE  = $FFF2DD;
  LEDDARKNEGBLUE    = $FF2D2D;
  LEDBKGNDNEGBLUE   = $FF0000;

  COLORSETS :array[0..7] of TColorSet = (
    (Background: LEDBKGNDRED; Bright: LEDBRIGHTRED; Dark: LEDDARKRED),
    (Background: LEDBKGNDGREEN; Bright: LEDBRIGHTGREEN; Dark: LEDDARKGREEN),
    (Background: LEDBKGNDYELLOW; Bright: LEDBRIGHTYELLOW; Dark: LEDDARKYELLOW),
    (Background: LEDBKGNDBLUE; Bright: LEDBRIGHTBLUE; Dark: LEDDARKBLUE),
    (Background: LEDBKGNDORANGE; Bright: LEDBRIGHTORANGE; Dark: LEDDARKORANGE),
    (Background: LEDBKGNDWHITE; Bright: LEDBRIGHTWHITE; Dark: LEDDARKWHITE),
    (Background: LEDBKGNDLCD; Bright: LEDBRIGHTLCD; Dark: LEDDARKLCD),
    (Background: LEDBKGNDNEGBLUE; Bright: LEDBRIGHTNEGBLUE; Dark: LEDDARKNEGBLUE)
  );

  procedure ColorSetsToStrings(Strings :TStrings);

implementation

procedure ColorSetsToStrings(Strings :TStrings);
var
  i, j :integer;
begin
  for i:=0 to High(COLORSETS) do begin
    j := i*2;
    Strings.Add(Format('Color%s=%6.6x', [Char(65+j), COLORSETS[i].Dark]));
    Strings.Add(Format('Color%s=%6.6x', [Char(66+j), COLORSETS[i].Bright]));
  end;
end;

end.

