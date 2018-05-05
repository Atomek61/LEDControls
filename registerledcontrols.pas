unit registerledcontrols;

{$mode objfpc}{$H+}

{$R blinker.res}
{$R segmentdisplay.res}
{$R ledmeter.res}
{$R signalstrength.res}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  Controls.Blinker, Controls.SegmentDisplay, Controls.LedMeter, Controls.SignalStrength;

procedure Register;
begin
  RegisterComponents('Atomek', [TBlinker, TSegmentDisplay, TLedMeter, TSignalStrength]);
end;

initialization
end.

