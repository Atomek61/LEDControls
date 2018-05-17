unit controls.signalstrength;

///////////////////////////////////////////////////////////////////////////////
//
//  Controls.SignalStrength (c) 2018 Jan Schirrmacher, www.atomek.de
//
//  License: See package LedControls license file LedControlsLicense.txt
//
//  TSignalStrength shows a bar diagram representing a signals strength.
//
//  Usage example: Place the control on your form and set these properties:
//    MySignalStrength.Level := 0.5; // 50%
//
///////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, Graphics, Controls, SysUtils, Types.Led;

type

  { TSignalStrength }

  TSignalStrength = class(TGraphicControl)
  private type
    TLayout = record
      r :TRect;
      cu :integer;
      s :integer;
      du :integer;
    end;
  private
    FLevel :single;
    FMinLevel :single;
    FMaxLevel :single;
    FBarCount :integer;
    FGapSize :integer;
    FOnColor :TColor;
    FOffColor :TColor;
    FScreenLayout :TLayout;
    FOnChanged :TNotifyEvent;
    FLayoutRequired :boolean;
    procedure SetBarCount(AValue: integer);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure SetGapSize(AValue: integer);
    procedure SetLevel(AValue: single);
    procedure DoLayout(const ARect :TRect; out ALayout :TLayout);
    procedure DoDraw(ACanvas :TCanvas; const Layout :TLayout);
    procedure SetMaxLevel(AValue: single);
    procedure SetMinLevel(AValue: single);
  protected
    procedure Changed; virtual;
    procedure DoOnResize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner :TComponent); override;
    procedure SetRange(AMinLevel, AMaxLevel :single);
    procedure Draw(ACanvas :TCanvas; const ARect :TRect);
  published
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property BarCount :integer read FBarCount write SetBarCount;
    property MinLevel :single read FMinLevel write SetMinLevel;
    property MaxLevel :single read FMaxLevel write SetMaxLevel;
    property Level :single read FLevel write SetLevel;
    property GapSize :integer read FGapSize write SetGapSize;
    property OnColor :TColor read FOnColor write SetOnColor;
    property OffColor :TColor read FOffColor write SetOffColor;
  end;

implementation

const
  DEFAULTMINLEVEL = 0.0;
  DEFAULTMAXLEVEL = 1.0;
  DEFAULTLEVEL    = 0.0;
  DEFAULTBARCOUNT = 4;
  DEFAULTGAPSIZE  = 2;
  DEFAULTONCOLOR  = LEDBRIGHTBLUE;
  DEFAULTOFFCOLOR = LEDDARKBLUE;

{ TSignalStrength }

constructor TSignalStrength.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 32;
  Height := 32;
  FLayoutRequired := true;
  FLevel    := DEFAULTLEVEL;
  FMinLevel := DEFAULTMINLEVEL;
  FMaxLevel := DEFAULTMAXLEVEL;
  FBarCount := DEFAULTBARCOUNT;
  FGapSize  := DEFAULTGAPSIZE;
  FOnColor  := DEFAULTONCOLOR;
  FOffColor := DEFAULTOFFCOLOR;
end;

procedure TSignalStrength.Changed;
begin
  Invalidate;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

procedure TSignalStrength.DoOnResize;
begin
  DoLayout(ClientRect, FScreenLayout);
  inherited DoOnResize;
end;

procedure TSignalStrength.Paint;
begin
  if FLayoutRequired then begin
    DoLayout(ClientRect, FScreenLayout);
    FLayoutRequired := false;
  end;
  DoDraw(Canvas, FScreenLayout);
end;

procedure TSignalStrength.DoLayout(const ARect: TRect; out ALayout: TLayout);
var
  w, d :integer;
  n :integer;
begin
  with ALayout do begin
    r := ARect;
    w := r.Width;
    s := FGapSize;
    n := FBarCount;
    cu := round((w-(n-1)*s)/n);
    d := (w-(cu*n + (n-1)*s)) div 2;
    inc(r.Left, d);
  end;
end;

procedure TSignalStrength.DoDraw(ACanvas: TCanvas; const Layout :TLayout);
var
  ci, i, n :integer;
  x, y, h :integer;
  d, l :single;
begin
  with ACanvas, Layout do begin
    h := r.Height;
    n := FBarCount;
    d := FMaxLevel-FMinLevel;
    ci := round(n*(FLevel-FMinLevel)/d);
    ACanvas.Brush.Color := FOnColor;
    for i:=0 to n-1 do begin
      x := r.Left + i*(cu+s);
      l := (i+1)*d/n;
      y := round(r.Bottom - h*l/d);
      if ci=i then
        ACanvas.Brush.Color := FOffColor;
      ACanvas.FillRect(x, y, x+cu, r.Bottom);
    end;
  end;
end;

procedure TSignalStrength.SetLevel(AValue: single);
begin
  if FLevel=AValue then Exit;
  FLevel:=AValue;
  Changed;
end;

procedure TSignalStrength.SetGapSize(AValue: integer);
begin
  if (FGapSize=AValue) or (AValue<0) then Exit;
  FGapSize:=AValue;
  FLayoutRequired := true;
  Changed;
end;

procedure TSignalStrength.SetOnColor(AValue: TColor);
begin
  if FOnColor=AValue then Exit;
  FOnColor:=AValue;
  Changed;
end;

procedure TSignalStrength.SetBarCount(AValue: integer);
begin
  if (FBarCount=AValue) and (AValue<2) then Exit;
  FBarCount:=AValue;
  FLayoutRequired := true;
  Changed;
end;

procedure TSignalStrength.SetOffColor(AValue: TColor);
begin
  if FOffColor=AValue then Exit;
  FOffColor:=AValue;
  Changed;
end;

procedure TSignalStrength.SetMaxLevel(AValue: single);
begin
  if (FMaxLevel=AValue) or (AValue=MinLevel) then Exit;
  FMaxLevel:=AValue;
  Changed;
end;

procedure TSignalStrength.SetMinLevel(AValue: single);
begin
  if (FMinLevel=AValue) and (AValue=FMaxLevel) then Exit;
  FMinLevel:=AValue;
  Changed;
end;

procedure TSignalStrength.SetRange(AMinLevel, AMaxLevel: single);
begin
  if (AMinLevel=FMinLevel) and (AMaxLevel=FMaxLevel) or (AMinLevel<>AMaxLevel) then Exit;
  FMinLevel := AMinLevel;
  FMaxLevel := AMaxLevel;
  Changed;
end;

procedure TSignalStrength.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  Layout :TLayout;
begin
  DoLayout(ARect, Layout);
  DoDraw(ACanvas, Layout);
end;

end.

