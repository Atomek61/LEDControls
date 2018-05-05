{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ledcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  registerledcontrols, Types.Led, Controls.Blinker, Controls.LedMeter, Controls.SegmentDisplay, 
  controls.signalstrength, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerledcontrols', @registerledcontrols.Register);
end;

initialization
  RegisterPackage('ledcontrols', @Register);
end.
