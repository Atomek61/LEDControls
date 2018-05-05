unit Controls.LedMeter;

///////////////////////////////////////////////////////////////////////////////
//
//  Controls.LedMeter (c) 2018 Jan Schirrmacher, www.atomek.de
//
//  License: See package LedControls license file LedControlsLicense.txt
//
//  TLedMeter is a single or group of bars, each made of a number of colored
//  Led rectangle segments displaying a level (or an array of levels).
//
//  SimpleExample for usage:
//
//  LedMeter := TLedMeter.Create(self);
//  LedMeter.Parent := self;
//  LedMeter.Visible := true;
//  LedMeter.BarCount := 1;
//  LedMeter.Levels[0] := 0.4;
//
//  To define the colors, edit the ColorNodes collection. You must define
//  one ColorNode for minimum. The nodes levels must be in ascending order.
//  The dark color is made from the Color and the darkness-factor.
//
//  Note the "Orientation"- and the "Style"-Properties.
//
///////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, Graphics, Controls, SysUtils, ExtCtrls;

const
  DEFAULTDARKNESS = 0.20; // 0..1, 0 ist schwarz
  DEFAULTAUTOZEROINTERVAL = 50;
  DEFAULTSEGMENTSIZE = 8;
  DEFAULTGAPSIZE = 2;

type

  TColorScheme = (lcsSimple, lcsSound, lcsRainbow);

  TLedMeter = class;

  { TColorNode }

  TColorNode = class(TCollectionItem)
  private
    FOnColor :TColor;
    FOffColor :TColor;
    FLevel :single;
    function GetLedMeter: TLedMeter;
    procedure SetColor(const Value: TColor);
    procedure SetLevel(const Value: single);
  protected
    property LedMeter :TLedMeter read GetLedMeter;
  public
    constructor Create(ACollection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    property OffColor :TColor read FOffColor;
  published
    property Color :TColor read FOnColor write SetColor;
    property Level :single read FLevel write SetLevel;
  end;

  TColorNodeCollection = class(TOwnedCollection)
  private
    function GetColorNode(Index: integer): TColorNode;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Items[Index :integer] :TColorNode read GetColorNode; default;
  end;

  { TLedMeter }

  TLedMeter = class(TGraphicControl)
  public type
    TOrientation = (loVertical, loHorizontal);
    TStyle = (lsNormal, lsBiDirectional);
    TMetrics = record   // Note: v is virtually vertical
      r :TRect;        // surrounding rect
      v0 :integer;     // Pixeloffset for centering in vertical space
      xu, xv :integer; // Width/Height
      nv, nu :integer; // Number of rows, number of columns
      s :integer;      // slit size           v
      cu, cv :integer; // cell metrics        |
      eu, ev :integer; // led metrics         +-u
      d :single;       // level/cell
      cn :array of integer; // Nodes of cells
      bic :integer;    // Bidirectional: number of segments half (= nv div 2)
    end;
  private
    FColorNodes :TColorNodeCollection;
    FMinLevel :single;
    FLevels :array of single;
    FSegmentSize :integer;
    FGapSize :integer;
    FOrientation :TOrientation;
    FStyle :TStyle;
    FDarkness :single;
    FWindowMetrics :TMetrics;
    FWindowMetricsRequired :boolean;
    FOnChange :TNotifyEvent;
    FAutoZeroTimer :TTimer;
    FAutoZeroInterval :integer;
    function GetAutoZero: boolean;
    function GetMaxLevel: single;
    function GetMinLevel: single;
    procedure SetAutoZero(AValue: boolean);
    procedure SetAutoZeroInterval(AValue: integer);
    procedure SetDarkness(AValue: single);
    procedure WindowMetricsRequired;
    function GetRange: single;
    function GetLevel0: single;
    function GetBarCount: integer;
    procedure SetGapSize(const Value: integer);
    function GetLevel :single;
    procedure SetLevel(AValue: single);
    procedure SetBarCount(AValue: integer);
    procedure SetSegmentSize(AValue: integer);
    procedure SetLevels(Index :integer; const Value: single);
    function GetLevels(Index :integer) :single;
    procedure SetMinLevel(const Value: single);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetStyle(const Value: TStyle);
    procedure SetColorNodes(const Value: TColorNodeCollection);
    procedure CalcMetrics(const Rect :TRect; var Layout :TMetrics);
    procedure DoDraw(const Layout :TMetrics; ACanvas :TCanvas);
    procedure OnAutoZeroTimer(Sender :TObject);
    procedure CheckAutoZero;
  protected
    procedure DoOnResize; override;
    procedure Changed;
    procedure SetEnabled(Value: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    function ItemAt(x, y :integer; out Col, Row :integer; out ALevel :single) :boolean;
    procedure SetColorScheme(Value :TColorScheme);
    property Levels[Index :integer] :single read GetLevels write SetLevels;
    property Range :single read GetRange;
    property Max :single read GetMaxLevel;
  published
    property ColorNodes :TColorNodeCollection read FColorNodes write SetColorNodes;
    property MinLevel :single read GetMinLevel write SetMinLevel;
    property Level :single read GetLevel write SetLevel;
    property BarCount :integer read GetBarCount write SetBarCount default 1;
    property SegmentSize :integer read FSegmentSize write SetSegmentSize default DEFAULTSEGMENTSIZE;
    property GapSize :integer read FGapSize write SetGapSize;
    property Orientation :TOrientation read FOrientation write SetOrientation default loVertical;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Style :TStyle read FStyle write SetStyle default lsNormal;
    property Darkness :single read FDarkness write SetDarkness;
    property AutoZero :boolean read GetAutoZero write SetAutoZero;
    property AutoZeroInterval :integer read FAutoZeroInterval write SetAutoZeroInterval;
    property Anchors;
    property Align;
    property Color;
    property ParentColor default true;
    property DragCursor;
    property DragKind;
    property DragMode;
    property MouseCapture;
    property ParentShowHint;
    property SessionProperties;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnTripleClick;
    property OnQuadClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnEditingDone;
  end;

implementation

uses
  Graphics.Utils;

{ TColorNode }

procedure TColorNode.AssignTo(Dest: TPersistent);
begin
  if Dest is TColorNode then begin
    TColorNode(Dest).Color := Color;
    TColorNode(Dest).Level := Level;
  end else
    inherited;
end;

constructor TColorNode.Create(ACollection: TCollection);
begin
  inherited;
  FOnColor := clGreen;
  FOffColor := InterpolateColor(clBlack, ColorToRGB(FOnColor), LedMeter.FDarkness);
  FLevel := 1.0;
end;

function TColorNode.GetDisplayName: string;
begin
  result := Format('Level %d', [Index+1]);
end;

procedure TColorNode.SetColor(const Value: TColor);
begin
  if Value<>FOnColor then begin
    FOnColor := Value;
    FOffColor := InterpolateColor(clBlack, ColorToRGB(Value), LedMeter.FDarkness);
    Changed(false);
  end;
end;

function TColorNode.GetLedMeter: TLedMeter;
begin
  result := (Collection as TColorNodeCollection).Owner as TLedMeter;
end;

procedure TColorNode.SetLevel(const Value: single);
begin
  if Value<>FLevel then begin
    FLevel := Value;
    Changed(false);
  end;
end;

{ TLedMeter }

constructor TLedMeter.Create(AOwner: TComponent);
begin
  inherited;
  ParentColor := true;
  Width := 20;
  Height := 100;

  FColorNodes := TColorNodeCollection.Create(self, TColorNode);

  FDarkness     := DEFAULTDARKNESS;
  FMinLevel     := 0.0;
  FSegmentSize  := DEFAULTSEGMENTSIZE;
  FGapSize      := DEFAULTGAPSIZE;
  FOrientation  := loVertical;
  FStyle        := lsNormal;
  FAutoZeroInterval := DEFAULTAUTOZEROINTERVAL;
  FWindowMetricsRequired := true;

  SetColorScheme(lcsSound);
  SetLength(FLevels, 1);
end;

procedure TLedMeter.Paint;
begin
  WindowMetricsRequired;
  DoDraw(FWindowMetrics, Canvas);
end;

destructor TLedMeter.Destroy;
begin
  FColorNodes.Free;
  inherited;
end;

procedure TLedMeter.SetEnabled(Value: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TLedMeter.SetGapSize(const Value: integer);
begin
  if Value<>FGapSize then begin
    FGapSize := Value;
    Changed;
  end;
end;

procedure TLedMeter.SetLevel(AValue: single);
begin
  Levels[0] := AValue;
end;

procedure TLedMeter.WindowMetricsRequired;
begin
  if FWindowMetricsRequired then begin
    CalcMetrics(ClientRect, FWindowMetrics);
    FWindowMetricsRequired := false;
  end;
end;

procedure TLedMeter.SetDarkness(AValue: single);
var
  i :integer;
begin
  if AValue<0.0 then AValue := 0.0
  else if AValue>1.0 then AValue := 1.0;
  if FDarkness=AValue then Exit;
  FDarkness:=AValue;
  for i:=0 to FColorNodes.Count-1 do
    with FColorNodes[i] do
      FOffColor := InterpolateColor(clBlack, ColorToRGB(FOnColor), FDarkness);
  Changed;
end;

function TLedMeter.GetMinLevel: single;
begin
  if Style=lsNormal then
    result := FMinLevel
  else
    result := -Max;
end;

procedure TLedMeter.SetAutoZero(AValue: boolean);
begin
  if AutoZero=AValue then Exit;
  if AValue then begin
    FAutoZeroTimer := TTimer.Create(nil);
    FAutoZeroTimer.OnTimer := @OnAutoZeroTimer;
    FAutoZeroTimer.Interval := 100;
    if (csDesigning in ComponentState) then
      FAutoZeroTimer.Enabled := true;
  end else
    FreeAndNil(FAutoZeroTimer);
  Changed;
end;

procedure TLedMeter.SetAutoZeroInterval(AValue: integer);
begin
  if FAutoZeroInterval=AValue then Exit;
  FAutoZeroInterval:=AValue;
  if AutoZero then
    FAutoZeroTimer.Interval := FAutoZeroInterval;
end;

function TLedMeter.GetMaxLevel: single;
begin
  result := FColorNodes[FColorNodes.Count-1].Level;
end;

function TLedMeter.GetAutoZero: boolean;
begin
  result := Assigned(FAutoZeroTimer);
end;

function TLedMeter.GetLevel0: single;
begin
  result := FLevels[0];
end;

function TLedMeter.GetBarCount: integer;
begin
  result := Length(FLevels)
end;

function TLedMeter.GetLevel: single;
begin
  result := FLevels[0];
end;

procedure TLedMeter.SetBarCount(AValue: integer);
begin
  if AValue<=0 then AValue := 1;
  if AValue<>BarCount then begin
    SetLength(FLevels, AValue);
    Changed;
  end;
end;

procedure TLedMeter.SetSegmentSize(AValue: integer);
begin
  if AValue<0 then AValue := 0;
  if AValue=FSegmentSize then Exit;
  FSegmentSize := AValue;
  Changed;
end;

procedure TLedMeter.SetLevels(Index: integer; const Value: single);
begin
  if Value<>FLevels[Index] then begin
    FLevels[Index] := Value;
    CheckAutoZero;
    Changed;
  end;
end;

function TLedMeter.GetLevels(Index: integer): single;
begin
  result := FLevels[Index];
end;

procedure TLedMeter.SetMinLevel(const Value: single);
begin
  if Value<>FMinLevel then begin
    FMinLevel := Value;
    Changed;
  end;
end;

procedure TLedMeter.SetOrientation(const Value: TOrientation);
var
  x :integer;
begin
  if Value<>FOrientation then begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then begin
      x := Width;
      Width := Height;
      Height := x;
    end;
    Changed;
  end;
end;

procedure TLedMeter.SetColorNodes(const Value: TColorNodeCollection);
begin
  FColorNodes.Assign(Value);
end;

procedure TLedMeter.CalcMetrics(const Rect: TRect; var Layout: TMetrics);
var
  i, ci :integer;
  li :single;
begin
  // v is virtually the vertical axis
  // u is virtually the horizontal axis
  with Layout do begin
    r   := Rect;
    nu  := BarCount;                              // Number of bars
    s   := FGapSize;                              // Slit between segments
    if FOrientation = loVertical then begin
      xu := r.Width;
      xv := r.Height;
    end else begin
      xu := r.Height;
      xv := r.Width;
    end;
    if FSegmentSize=0 then
      ev := (xu-s*(nu-1)) div nu                  // quadratic segments
    else
      ev := FSegmentSize;                         // defined segment height
    cv := ev+s;                                   // cell height
    nv := (xv+s) div cv;                          // number of vertical segments
    if nv<=0 then nv := 1;
    // if Bidirectional then ensure number of segments is odd
    if (FStyle=lsBidirectional) and (nv mod 2 = 0) then dec(nv);
    eu := (xu-s*(nu-1)) div nu;                   // segment width
    cu := eu+s;                                   // cell width
    v0 := (xv - nv*cv) div 2;                     // vertical offset to center column vertically
    xu := nu*cu;                                  // real width
    xv := nv*cv;                                  // real height
    // ColorNodes of levels
    if FStyle=lsNormal then begin
      if nv>1 then begin
        d := Range/(nv-1);
        SetLength(cn, nv);
        ci := 0;
        for i:=0 to nv-1 do begin
          li := FMinLevel + i*d;
          while (ci<FColorNodes.Count-1) and (FColorNodes[ci].Level<li) do
            inc(ci);
          cn[i] := ci;
        end;
      end;
    end else begin
      bic := nv div 2;  // bidirectional center or number of segments half
      if bic>0 then begin
        d := Max/bic;
        SetLength(cn, nv);
        ci := 0;
        for i:=0 to bic do begin // over the half bar
          li := i*d;
          while (ci<FColorNodes.Count-1) and (FColorNodes[ci].Level<li) do
            inc(ci);
          cn[bic+i] := ci;
          cn[bic-i] := ci;
        end;
      end;
    end;
  end;
end;

procedure TLedMeter.DoDraw(const Layout: TMetrics; ACanvas: TCanvas);
var
  u, v :integer;

  procedure Switch(Bright :boolean);
  begin
    if Bright then
      ACanvas.Brush.Color := FColorNodes[Layout.cn[v]].FOnColor
    else
      ACanvas.Brush.Color := FColorNodes[Layout.cn[v]].FOffColor;
  end;

  procedure DrawSegment;
    procedure DrawRect(x, y, w, h :integer);
    begin
      ACanvas.FillRect(x, y, x+w, y+h)
    end;
  begin
    with Layout do begin
      if FOrientation = loVertical then
        DrawRect(r.Left+u*cu, r.Bottom-v*cv-ev-v0, eu, ev)
      else
        DrawRect(r.Left+v*cv+v0, r.Bottom-(u+1)*cu+s, ev, eu);
    end;
  end;

begin
  if FColorNodes.Count=0 then Exit;

  with Layout do begin
{$IFDEF DEBUG}
    with ACanvas, r do begin
      Pen.Color := clSilver;
      MoveTo(Left-1, Top-1); LineTo(Right, Top-1); LineTo(Right, Bottom); LineTo(Left-1, Bottom); LineTo(Left-1, Top-1);
    end;
{$ENDIF}
    if Style=lsNormal then begin
      for u := 0 to nu-1 do begin
        for v := 0 to nv-1 do begin
          Switch(Enabled and ((v=0) or (FLevels[u]>=v*d)));
          DrawSegment;
        end;
      end;
    end else begin
      for u := 0 to nu-1 do begin
        for v := bic to nv-1 do begin
          Switch(Enabled and ((v=bic) or (FLevels[u]>=(v-bic)*d)));
          DrawSegment;
        end;
        for v := bic-1 downto 0 do begin
          Switch(Enabled and ((v=bic) or (FLevels[u]<=(v-bic)*d)));
          DrawSegment;
        end;
      end;
    end;
  end;
end;

procedure TLedMeter.OnAutoZeroTimer(Sender: TObject);
var
  i, n :integer;
  d, l :single;
begin
  WindowMetricsRequired;
  n := BarCount;
  if Style = lsNormal then begin
    d := Range/10;
    for i:=0 to BarCount-1 do begin
      if FLevels[i]=FMinLevel then
        dec(n)
      else begin
        l := FLevels[i] - d;
        if l<=FMinLevel then begin
          l := FMinLevel;
          dec(n);
        end;
        Levels[i] := l;
      end;
    end;
  end else begin
    d := Range/20;
    for i:=0 to BarCount-1 do begin
      if FLevels[i]=0.0 then
        dec(n)
      else begin
        if FLevels[i]>0.0 then begin
          l := FLevels[i] - d;
          if l<=0.0 then begin
            l := 0.0;
            dec(n);
          end;
        end else begin
          l := FLevels[i] + d;
          if l>=0.0 then begin
            l := 0.0;
            dec(n);
          end;
        end;
        Levels[i] := l;
      end;
    end;
  end;
  if n=0 then
    FAutoZeroTimer.Enabled := false;
end;

procedure TLedMeter.CheckAutoZero;
begin
  if AutoZero and not (csDesigning in ComponentState) then
    FAutoZeroTimer.Enabled := true;
end;

function TLedMeter.ItemAt(x, y: integer; out Col, Row: integer; out ALevel :single): boolean;
begin
  WindowMetricsRequired;
  with FWindowMetrics do begin
    if FOrientation = loVertical then begin
      if (x<r.Left) or (x>=r.Left+xu) or (y<r.Top+v0) or (y>=r.Top+v0+xv) then Exit(false);
      Col := (x-r.Left) div cu;
      Row := ((r.Top+v0+xv)-y) div cv
    end else begin
      if (x<r.Left+v0) or (x>=r.Left+xv-v0) or (y<r.Top) or (y>=r.Top+xu) then Exit(false);
      Col := nu-1 - (y-r.Top) div cu;
      Row := (x-r.Left-v0) div cv;
    end;
    if Style=lsNormal then
      ALevel := FMinLevel + Row * d
    else
      ALevel := (Row-bic)*d;
  end;
  result := true;
end;

procedure TLedMeter.SetColorScheme(Value: TColorScheme);
begin
  FColorNodes.Clear;
  case Value of
  lcsSimple:
    begin
      with FColorNodes.Add as TColorNode do begin
        Level := 1.0;
        Color := clLime;
      end;
    end;
  lcsSound:
    begin
      with FColorNodes.Add as TColorNode do begin
        Level := 0.5;
        Color := clLime;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 0.75;
        Color := clYellow;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 1.0;
        Color := clRed;
      end;
    end;
  lcsRainbow:
    begin
      with FColorNodes.Add as TColorNode do begin
        Level := 1/6;
        Color := clAqua;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 2/6;
        Color := clBlue;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 3/6;
        Color := clLime;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 4/6;
        Color := clYellow;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 5/6;
        Color := clRed;
      end;
      with FColorNodes.Add as TColorNode do begin
        Level := 6/6;
        Color := clPurple;
      end;
    end;
  end;
end;

procedure TLedMeter.DoOnResize;
begin
  inherited DoOnResize;
  FWindowMetricsRequired := true;
end;

procedure TLedMeter.Changed;
begin
  FWindowMetricsRequired := true;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TLedMeter.SetStyle(const Value: TStyle);
begin
  if Value<>FStyle then begin
    FStyle := Value;
    Changed;
  end;
end;

function TLedMeter.GetRange: single;
begin
  result := Max - MinLevel;
end;

{ TColorNodeCollection }

function TColorNodeCollection.GetColorNode(Index: integer): TColorNode;
begin
  result := TColorNode(inherited Items[Index]);
end;

procedure TColorNodeCollection.Update(Item: TCollectionItem);
begin
  inherited;
  TLedMeter(GetOwner).Changed;
end;

end.
