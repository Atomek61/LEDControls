unit Controls.SegmentDisplay;

///////////////////////////////////////////////////////////////////////////////
//
//  Controls.SegmentDisplay (c) 2018 Jan Schirrmacher, www.atomek.de
//
//  License: See package LedControls license file LedControlsLicense.txt
//
//  TSegmentDisplay shows a 7-segment LED/LCD display.
//
//  Usage example: Place the control on your form and set these properties:
//    MyDisplay.Modules := '8.8.8.8.'; // This defines the modules
//    MyDisplay.Text := Format('%5.2f', [MyValue]); // Sets the Text to be
//                                                  // displayed
//  Available modules:
//     '8'   7-segment module, Symbols -, 0..9, A..F, SPACE
//     '8.'  7-segments + dot, Symbols as '8' and '.'
//     ':'   Double dot, can display a double dot as a clocks delimiter
//     '_'   Spacer
//
//  The Text must correspond to the defined modules. Use SPACE to fill a
//  dark module. Example: Modules:='88:88'; Text:=' 1 23'; The first module
//  will display an empty module, the doubledot module will display a
//  dark doubledot module.
//
//  Play around with the SegmentDisplayDemo to learn the design features.
//  You can select the colors, the size and many segment proportions.
//
///////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, fgl, ExtCtrls, Types.Led,
  Controls, BGRABitmapTypes, BGRABitmap, BGRAPath, BGRAGradientScanner, BGRATransform,
  Graphics;

type
  TModuleType = class;
  TModule = class;
  TDesign = class;
  TSegmentDisplay = class;

  TModules = specialize TFPGObjectList<TModule>;
  TModuleTypes = specialize TFPGObjectList<TModuleType>;

  TSegmentStyle = (ssClassic, ssBlock, ss80th{, ssOptimal});

  TSegment = (SegA, SegB, SegC, SegD, SegE, SegF, SegG);
  TSegments = set of TSegment;

  TDrawSegmentsEvent = procedure(Display :TSegmentDisplay; SymbolIndex :integer; var Segments :TSegments) of object;

  TPixel = single;

  { TSegmentDisplay }

  TSegmentDisplay = class(TGraphicControl, IFPObserver)
  private
    FAlignment :TAlignment;
    FModuleTypes :TModuleTypes; // Available
    FModules :TModules;         // Defined
    FSymbols :string;           // Text
    FSymbolIndex :integer;      // Valid while OnDrawSymbol
    FPadding :integer;          // Frame around the modules
    FGapRatio :single;          // Gap between modules related to height
    FDesign :TDesign;
    FOnChanged :TNotifyEvent;
    FOnDrawSegments :TDrawSegmentsEvent;
    procedure SetDesign(AValue: TDesign);
    procedure SetGapRatio(AValue: single);
    procedure SetModules(AValue: string);
    function GetModules :string;
    procedure SetPadding(AValue: integer);
    procedure SetText(AValue: string);
    procedure SetAlignment(AValue: TAlignment);
    procedure DoLayout(const ARect :TRect); virtual;
  protected
    procedure Changed; virtual;
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
    procedure Paint; override;
    property ModuleTypes :TModuleTypes read FModuleTypes;
    property ModuleList :TModules read FModules;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas :TCanvas);
  published
    property Alignment :TAlignment read FAlignment write SetAlignment;
    property Text :string read FSymbols write SetText;
    property Modules :string read GetModules write SetModules;
    property Design :TDesign read FDesign write SetDesign;
    property Padding :integer read FPadding write SetPadding;
    property GapRatio :single read FGapRatio write SetGapRatio;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property OnDrawSegments :TDrawSegmentsEvent read FOnDrawSegments write FOnDrawSegments;
    property Anchors;
    property Align;
    property ParentColor;
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

  TModule = class
    ModuleType :TModuleType;
    Rect :TRect;
  end;

const
  DEFAULTSEGMENTSTYLE = ssClassic;
  DEFAULTALIGNMENT    = taRightJustify;
  DEFAULTINNERGLOW    = false;
  DEFAULTDARKVISIBLE  = true;
  DEFAULTWIDTH        = 124;
  DEFAULTHEIGHT       = 48;
  DEFAULTPADDING      = 4;
  DEFAULTMODULERATIO  = 1.800;
  DEFAULTGAPRATIO     = 0;
  DEFAULTSEGMENTRATIO = 0.125;
  DEFAULTTILTRATIO    = 0.150;
  DEFAULTEDGERATIO    = 0.125;
  DEFAULTSLITRATIO    = 0.015;

type
  { TDesign }

  // Important design properties are bundled here
  TDesign = class(TPersistent)
  private
    FModuleRatio :single;
    FSegmentRatio :single;
    FTiltRatio :single;
    FEdgeRatio :single;
    FSlitRatio :single;
    FDarkVisible :boolean;
    FInnerGlow :boolean;
    FColorSet :TColorSet;
    FStyle :TSegmentStyle;
    function IsEqual(Design :TDesign) :boolean;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBrightColor(AValue: TColor);
    procedure SetDarkColor(AValue: TColor);
    procedure SetDarkVisible(AValue: boolean);
    procedure SetEdgeRatio(AValue: single);
    procedure SetInnerGlow(AValue: boolean);
    procedure SetModuleRatio(AValue: single);
    procedure SetSegmentRatio(AValue: single);
    procedure SetSlitRatio(AValue: single);
    procedure SetTiltRatio(AValue: single);
    procedure SetStyle(AValue: TSegmentStyle);
  public
    constructor Create;
    procedure Assign(Source :TPersistent); override;
    procedure Changed; virtual;
  published
    // Height/Width
    property ModuleRatio :single read FModuleRatio write SetModuleRatio;
    // SegmentWidth/Height
    property SegmentRatio :single read FSegmentRatio write SetSegmentRatio;
    // Tilt/Height
    property TiltRatio :single read FTiltRatio write SetTiltRatio;
    // Slit/Height, gap between segments
    property SlitRatio :single read FSlitRatio write SetSlitRatio;
    // Edge/Height
    property EdgeRatio :single read FEdgeRatio write SetEdgeRatio;
    // If dark segments are visible
    property DarkVisible :boolean read FDarkVisible write SetDarkVisible;
    // Inner glow in bright segments
    property InnerGlow :boolean read FInnerGlow write SetInnerGlow;
    property BackgroundColor :TColor read FColorSet.Background write SetBackgroundColor;
    // Bright segments color
    property BrightColor :TColor read FColorSet.Bright write SetBrightColor;
    // Dark segments color
    property DarkColor :TColor read FColorSet.Dark write SetDarkColor;
    // Segment form
    property Style :TSegmentStyle read FStyle write SetStyle;
  end;

  { TModuleType }

  // Base class for modules
  TModuleType = class abstract
  private
    FDesign :TDesign;
    FSize :TSize;
    FCharSet :string;
  protected
    h, w :TPixel;     // Height, Width
    k :TPixel;        // Skew
    kt :single;       // Skew transformation xi := yi*kt
    procedure EraseBackground; virtual;
    procedure DefineCharset(const ACharset :string);
    function GetId :string; virtual; abstract;
    procedure SetSize(const ASize :TSize); virtual; // Called by TSegmentDisplay
    function Layout(AHeight :integer) :integer; virtual; abstract; // Returns width
    function DrawSymbol(ASymbol :char) :boolean; virtual; abstract;
    procedure DrawTo(ACanvas :TCanvas; const ARect :TRect); virtual; abstract;
    property Design :TDesign read FDesign;
  public
    constructor Create(ADesign :TDesign);
    property Size :TSize read FSize;
  end;

implementation

const
  SEG7SEGMENTS_CIPHER :array['0'..'9'] of TSegments = (
    [SegA, SegB, SegC, SegD, SegE, SegF]      , // 0
    [SegB, SegC]                              , // 1
    [SegA, SegB, SegG, SegE, SegD]            , // 2
    [SegA, SegB, SegC, SegD, SegG]            , // 3
    [SegB, SegC, SegF, SegG]                  , // 4
    [SegA, SegC, SegD, SegF, SegG]            , // 5
    [SegA, SegC, SegD, SegE, SegF, SegG]      , // 6
    [SegA, SegB, SegC]                        , // 7
    [SegA, SegB, SegC, SegD, SegE, SegF, SegG], // 8
    [SegA, SegB, SegC, SegD, SegF, SegG]        // 9
  );

  SEG7SEGMENTS_CHAR :array['A'..'F'] of TSegments = (
    [SegA, SegB, SegC, SegE, SegF, SegG], // A
    [SegC, SegD, SegE, SegF, SegG],       // B
    [SegA, SegD, SegE, SegF],             // C
    [SegB, SegC, SegD, SegE, SegG],       // D
    [SegA, SegD, SegE, SegF, SegG],       // E
    [SegA, SegE, SegF, SegG]              // F
  );

type
  { TSpacerModuleType }

  // Empty module as a spacer
  TSpacerModuleType = class(TModuleType)
  protected
    function GetId :string; override;
    function Layout(AHeight :integer) :integer; override; // Returns width
    function DrawSymbol(ASymbol :char) :boolean; override;
    procedure DrawTo(ACanvas :TCanvas; const ARect :TRect); override;
  end;

  { TBitmapModuleType }

  // Base for modules with a bitmap
  TBitmapModuleType = class(TModuleType)
  private
    FBitmap :TBGRABitmap;
  protected
    t :TPixel;        // Segment width
    f :TPixel;        // Frame (=1)
    s :TPixel;        // Slit
    e :TPixel;        // Edge
    ws, hs :TPixel;   // Segment size for InnerGlow
    procedure SetSize(const ASize :TSize); override;
    function Layout(AHeight :integer) :integer; override;
    procedure EraseBackground; override;
    procedure DrawTo(ACanvas :TCanvas; const ARect :TRect); override;
    property Bitmap :TBGRABitmap read FBitmap;
  public
    constructor Create(ADesign :TDesign);
    destructor Destroy; override;
  end;

  { T7SegModuleType }

  // Segments A..G
  T7SegModuleType = class(TBitmapModuleType)
  private const
    SEG7ID :string = '8';
    SEG7CHARSET :string = '0123456789-ABCDEF';
  private
    FDisplay :TSegmentDisplay;
  protected
    ex :TPixel;   // Edge projection
    sx :TPixel;   // Slit projection
    Pts :array[SegA..SegG] of array of TPointF; // Segment polygons
    Cts :array[SegA..SegG] of TPointF; // Segment centers
    function GetId :string; override;
    function Layout(AHeight :integer) :integer; override;
    function DrawSymbol(ASymbol :char) :boolean; override;
  public
    constructor Create(ADesign :TDesign);
  end;

  { T7SegDotModuleType }

  // Segments A..G and dot
  T7SegDotModuleType = class(T7SegModuleType)
  private const
    SEG7DOTID :string = '8.';
    SEG7DOTCHARSET :string = '.,';
  private
    FDot :boolean; // If last char was dot
  protected
    dc :TPointF;   // SegDot
    dd :single;
    function GetId :string; override;
    function Layout(AHeight :integer) :integer; override;
    function DrawSymbol(ASymbol :char) :boolean; override;
  public
    constructor Create(ADesign :TDesign);
  end;

  { TDoubleDotModuleType }

  // : for clock
  TDoubleDotModuleType = class(TBitmapModuleType)
  private const
    DOUBLEDOTID :string = ':';
    DOUBLEDOTCHARSET :string = ':';
  protected
    dc1 :TPointF;  // SegDot1
    dc2 :TPointF;  // SegDot2
    dd :single;
    function GetId :string; override;
    function Layout(AHeight :integer) :integer; override;
    function DrawSymbol(ASymbol :char) :boolean; override;
  public
    constructor Create(ADesign :TDesign);
  end;

{ TSegmentDisplay }

constructor TSegmentDisplay.Create(AOwner: TComponent);
var
  SegModType :T7SegModuleType;
begin
  inherited Create(AOwner);
  FModuleTypes := TModuleTypes.Create;
  FModules    := TModules.Create;
  FDesign     := TDesign.Create;
  FAlignment  := DEFAULTALIGNMENT;
  Height      := DEFAULTHEIGHT;
  Width       := DEFAULTWIDTH;
  FPadding    := DEFAULTPADDING;
  FGapRatio   := DEFAULTGAPRATIO;
  FDesign.FPOAttachObserver(self);

  ModuleTypes.Add(T7SegDotModuleType.Create(Design));
  SegModType := T7SegModuleType.Create(Design);
  SegModType.FDisplay := self;
  ModuleTypes.Add(SegModType);
  ModuleTypes.Add(TDoubleDotModuleType.Create(Design));
  ModuleTypes.Add(TSpacerModuleType.Create(Design));

  Modules := '8.8.8.8.';
  FSymbols := '    ';

end;

destructor TSegmentDisplay.Destroy;
begin
  FModules.Free;
  FModuleTypes.Free;
  FDesign.Free;
  inherited Destroy;
end;

procedure TSegmentDisplay.SetDesign(AValue: TDesign);
begin
  if FDesign.IsEqual(AValue) then Exit;
  FDesign.Assign(AValue);
  Changed;
end;

procedure TSegmentDisplay.SetGapRatio(AValue: single);
begin
  if FGapRatio=AValue then Exit;
  FGapRatio:=AValue;
  Changed;
end;

procedure TSegmentDisplay.SetModules(AValue: string);
var
  Module :TModule;
  i, l :integer;
  Id :string;
  ModuleType :TModuleType;
begin
  if AValue=Modules then Exit;
  FModules.Clear;
  i := 1;
  while i<=Length(AValue) do begin
    Module := nil;
    for ModuleType in FModuleTypes do begin
      Id := ModuleType.GetId;
      l := Length(Id);
      if SameText(Id, Copy(AValue, i, l)) then begin
        inc(i, l);
        Module := TModule.Create;
        Module.ModuleType := ModuleType;
        FModules.Add(Module);
        break;
      end;
    end;
    if Module=nil then inc(i);
  end;
  Changed;
end;

function TSegmentDisplay.GetModules: string;
var
  Module :TModule;
begin
  result := '';
  for Module in FModules do
    result += Module.ModuleType.GetId;
end;

procedure TSegmentDisplay.SetPadding(AValue: integer);
begin
  if FPadding=AValue then Exit;
  FPadding:=AValue;
  Changed;
end;

procedure TSegmentDisplay.SetText(AValue: string);
begin
  if AValue=FSymbols then Exit;
  FSymbols := AValue;
  Changed;
end;

procedure TSegmentDisplay.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
  Changed;
end;

procedure TSegmentDisplay.DoLayout(const ARect :TRect);
var
  x, w, g :integer;
  Module :TModule;
  ModuleType :TModuleType;
  ModuleSize :TSize;
begin

  if FModuleTypes.Count=0 then Exit;

  // Find largest module
  ModuleSize.cy := ARect.Height - 2*FPadding;
  g := round(ModuleSize.cy*FGapRatio);

  // Query all ModuleTypes for their width and store it in Rect.Right
  for ModuleType in FModuleTypes do begin
    ModuleSize.cx := ModuleType.Layout(ModuleSize.cy);
    ModuleType.SetSize(ModuleSize);
  end;

  // Width of all modules
  w := 0;
  for Module in FModules do
    w += Module.ModuleType.Size.cx;
  if FModules.Count>0 then
    w += (FModules.Count-1)*g;

  // Calc x of most left module
  case FAlignment of
  taLeftJustify:  x := ARect.Left + FPadding;
  taCenter:       x := (ARect.Left + ARect.Right - w) div 2;
  taRightJustify: x := ARect.Right - w - FPadding;
  end;

  // Set all modules Rect.Left
  for Module in FModules do begin
    Module.Rect.Left := x;
    Module.Rect.Right := Module.Rect.Left + Module.ModuleType.Size.cx;
    Module.Rect.Top := ARect.Top + FPadding;
    Module.Rect.Bottom := ARect.Bottom - FPadding;
    x += Module.ModuleType.Size.cx + g;
  end;

end;

procedure TSegmentDisplay.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(self);
  Invalidate;
end;

procedure TSegmentDisplay.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation=ooChange then begin
    Changed;
  end;
end;

procedure TSegmentDisplay.Paint;
begin
  inherited Paint;
  DoLayout(ClientRect);
  Draw(Canvas);
end;

procedure TSegmentDisplay.Draw(ACanvas: TCanvas);
var
  ModuleIndex :integer;
  Module :TModule;
begin
  ACanvas.Brush.Color := Design.BackgroundColor;
  ACanvas.FillRect(ClientRect);
  FSymbolIndex := High(FSymbols);
  ModuleIndex := FModules.Count-1;
  // Draw existing symbols
  while (FSymbolIndex>0) and (ModuleIndex>=0) do begin
    Module := FModules[ModuleIndex];
    Module.ModuleType.EraseBackground;
    while Module.ModuleType.DrawSymbol(FSymbols[FSymbolIndex]) do
      dec(FSymbolIndex);
    Module.ModuleType.DrawTo(ACanvas, Module.Rect);
    dec(FSymbolIndex);
    dec(ModuleIndex);
  end;
  // Filling if text is too short
  if ModuleIndex>=0 then begin
    repeat
      Module := FModules[ModuleIndex];
      Module.ModuleType.EraseBackground;
      Module.ModuleType.DrawSymbol(' ');
      Module.ModuleType.DrawTo(ACanvas, Module.Rect);
      dec(ModuleIndex);
    until ModuleIndex<0;
  end;
end;

function TSpacerModuleType.GetId: string;
begin
  result := '_';
end;

function TSpacerModuleType.Layout(AHeight: integer): integer;
begin
  result := round(AHeight/FDesign.ModuleRatio);
end;

function TSpacerModuleType.DrawSymbol(ASymbol: char): boolean;
begin
  result := false;
end;

procedure TSpacerModuleType.DrawTo(ACanvas: TCanvas; const ARect: TRect);
begin

end;

{ TModuleType }

constructor TModuleType.Create(ADesign: TDesign);
begin
  FCharSet := ' ';
  FDesign := ADesign;
end;

procedure TModuleType.SetSize(const ASize :TSize);
begin
  if ASize<>FSize then begin
    FSize := ASize;
  end;
end;

procedure TModuleType.EraseBackground;
begin

{$IFDEF DEBUG}
  with FBitmap.Canvas do begin
    Pen.Color := clLime;
    MoveTo(0, 0);
    LineTo(Size.cx-1, 0);
    LineTo(Size.cx-1, Size.cy-1);
    LineTo(0, Size.cy-1);
    LineTo(0, 0);
  end;
{$ENDIF}

end;

procedure TModuleType.DefineCharset(const ACharset: string);
begin
  FCharSet += ACharset;
end;

{ TDesign }

procedure TDesign.Assign(Source: TPersistent);
var
  ADesign :TDesign;
begin
  if Source is TDesign then begin
    ADesign := TDesign(Source);
    FModuleRatio := ADesign.FModuleRatio;
    FSegmentRatio := ADesign.FSegmentRatio;
    FSlitRatio := ADesign.FSlitRatio;
    FTiltRatio := ADesign.FTiltRatio;
    FEdgeRatio := ADesign.FEdgeRatio;
    FColorSet := ADesign.FColorSet;
    FInnerGlow := ADesign.FInnerGlow;
    FDarkVisible := ADesign.FDarkVisible;
    FStyle := ADesign.FStyle;
    Changed;
  end else
    inherited;
end;

function TDesign.IsEqual(Design: TDesign): boolean;
begin
  result :=
    (ModuleRatio = Design.ModuleRatio)
    and (FSegmentRatio = Design.FSegmentRatio)
    and (FSlitRatio = Design.FSlitRatio)
    and (FTiltRatio = Design.FTiltRatio)
    and (FEdgeRatio = Design.FEdgeRatio)
    and (FColorSet.Background = Design.FColorSet.Background)
    and (FColorSet.Bright = Design.FColorSet.Bright)
    and (FColorSet.Dark = Design.FColorSet.Dark)
    and (FInnerGlow = Design.FInnerGlow)
    and (FDarkVisible = Design.FDarkVisible)
    and (FStyle = Design.FStyle);
end;

procedure TDesign.SetBackgroundColor(AValue: TColor);
begin
  if FColorSet.Background=AValue then Exit;
  FColorSet.Background := AValue;
  Changed;
end;

procedure TDesign.SetBrightColor(AValue: TColor);
begin
  if FColorSet.Bright=AValue then Exit;
  FColorSet.Bright := AValue;
  Changed;
end;

procedure TDesign.SetDarkColor(AValue: TColor);
begin
  if FColorSet.Dark=AValue then Exit;
  FColorSet.Dark := AValue;
  Changed;
end;

procedure TDesign.SetDarkVisible(AValue: boolean);
begin
  if FDarkVisible=AValue then Exit;
  FDarkVisible:=AValue;
  Changed;
end;

procedure TDesign.SetEdgeRatio(AValue: single);
begin
  if FEdgeRatio=AValue then Exit;
  FEdgeRatio:=AValue;
  Changed;
end;

procedure TDesign.SetInnerGlow(AValue: boolean);
begin
  if FInnerGlow=AValue then Exit;
  FInnerGlow:=AValue;
  Changed;
end;

procedure TDesign.Changed;
begin
  FPONotifyObservers(self, ooChange, nil);
end;

procedure TDesign.SetModuleRatio(AValue: single);
begin
  if FModuleRatio=AValue then Exit;
  FModuleRatio:=AValue;
  Changed;
end;

procedure TDesign.SetSegmentRatio(AValue: single);
begin
  if FSegmentRatio=AValue then Exit;
  FSegmentRatio:=AValue;
  Changed;
end;

procedure TDesign.SetSlitRatio(AValue: single);
begin
  if FSlitRatio=AValue then Exit;
  FSlitRatio:=AValue;
  Changed;
end;

procedure TDesign.SetStyle(AValue: TSegmentStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Changed;
end;

procedure TDesign.SetTiltRatio(AValue: single);
begin
  if FTiltRatio=AValue then Exit;
  FTiltRatio:=AValue;
  Changed;
end;

constructor TDesign.Create;
begin
  FModuleRatio    := DEFAULTMODULERATIO;
  FSegmentRatio   := DEFAULTSEGMENTRATIO;
  FTiltRatio      := DEFAULTTILTRATIO;
  FSlitRatio      := DEFAULTSLITRATIO;
  FEdgeRatio      := DEFAULTEDGERATIO;
  FDarkVisible    := DEFAULTDARKVISIBLE;
  FInnerGlow      := DEFAULTINNERGLOW;
  FColorSet       := COLORSETS[0];
  FStyle          := DEFAULTSEGMENTSTYLE;
end;

{ TBitmapModuleType }

constructor TBitmapModuleType.Create(ADesign: TDesign);
begin
  inherited;
  FBitmap := TBGRABitmap.Create;
end;

destructor TBitmapModuleType.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TBitmapModuleType.Layout(AHeight: integer): integer;
begin
  f := 1.0;
  h := AHeight-2*f;
  w := AHeight/Design.ModuleRatio-2*f;
  if h<1.0 then h := 1.0;
  if w<1.0 then w := 1.0;

  // Module skew dx=y*kt
  if Design.TiltRatio>0.0 then
    k := h * Design.TiltRatio
  else
    k := 0.0;
  kt := Design.TiltRatio;

  t := h * Design.SegmentRatio;
  s := h * Design.SlitRatio;
  e := h * Design.FEdgeRatio;

  result := round(AHeight/Design.ModuleRatio);

end;

procedure TBitmapModuleType.SetSize(const ASize :TSize);
begin
  inherited;
  FBitmap.SetSize(FSize.cx, FSize.cy);
end;

procedure TBitmapModuleType.EraseBackground;
begin
  FBitmap.FillRect(0, 0, FSize.cx, FSize.cy, FDesign.BackgroundColor);
end;

procedure TBitmapModuleType.DrawTo(ACanvas: TCanvas; const ARect: TRect);
begin
  FBitmap.Draw(ACanvas, ARect);
end;

{ T7SegModuleType }

constructor T7SegModuleType.Create(ADesign: TDesign);
begin
  inherited;
  DefineCharset(SEG7CHARSET);
end;

function T7SegModuleType.GetId: string;
begin
  result := SEG7ID;
end;

function T7SegModuleType.Layout(AHeight: integer) :integer;
var
  seg :TSegment;
  i :integer;
  p, ps :PPointF;
begin
  result := inherited Layout(AHeight) + round(k);

  ex := e/SQRT(2);
  sx := s/SQRT(2);

  // Prepare styles
  case FDesign.Style of
  ssClassic:
    begin
      for seg:=SegA to SegG do
        SetLength(pts[seg], 6);

      // Points are calculated in the upper left quadrant
      // See drawing
      // Segment a
      p := @pts[SegA,0];
      p[0].x := t+sx;
      p[0].y := t;
      p[1].x := w-t-sx;
      p[1].y := t;
      p[2].x := w-(ex+sx)/2;
      p[2].y := (ex-sx)/2;
      p[3].x := w-ex;
      p[3].y := 0;
      p[5].x := (ex+sx)/2;
      p[5].y := (ex-sx)/2;
      p[4].x := ex;
      p[4].y := 0;

      ws := p[2].x - p[5].x;

      // Segment f: 6 Points in quadrant
      p := @pts[SegF,0];
      p[0].x := t;
      p[0].y := (h-t)/2-sx;
      p[1].x := t;
      p[1].y := t+sx;
      p[2].x := (ex-sx)/2;
      p[2].y := (ex+sx)/2;
      p[3].x := 0;
      p[3].y := ex;
      p[4].x := 0;
      p[4].y := p[0].y;
      p[5].x := t/2;
      p[5].y := h/2-sx;

      hs := p[5].y - p[2].y;

      // Segment g: 2 points in quadrant, rest mirrored
      p := @pts[SegG,0];
      p[0].x := t+sx;
      p[0].y := (h+t)/2;
      p[1].x := w-t-sx;
      p[1].y := p[0].y;
      p[2].x := w-t/2-sx;
      p[2].y := h/2;
      p[3].x := p[1].x;
      p[3].y := (h-t)/2;
      p[4].x := p[0].x;
      p[4].y := p[3].y;
      p[5].x := t/2+sx;
      p[5].y := p[2].y;

      // Segment b: mirrored from f around Y
      p := @pts[SegB,0];
      ps := @pts[SegF,0];
      p[0].x := w-ps[1].x;
      p[0].y := ps[1].y;
      p[1].x := w-ps[0].x;
      p[1].y := ps[0].y;
      p[2].x := w-ps[5].x;
      p[2].y := ps[5].y;
      p[3].x := w-ps[4].x;
      p[3].y := ps[4].y;
      p[4].x := w-ps[3].x;
      p[4].y := ps[3].y;
      p[5].x := w-ps[2].x;
      p[5].y := ps[2].y;

      // Segment c: mirrored from b around X
      p := @pts[SegC,0];
      ps := @pts[SegB,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[2].x;
      p[1].y := h-ps[2].y;
      p[2].x := ps[3].x;
      p[2].y := h-ps[3].y;
      p[3].x := ps[4].x;
      p[3].y := h-ps[4].y;
      p[4].x := ps[5].x;
      p[4].y := h-ps[5].y;
      p[5].x := ps[0].x;
      p[5].y := h-ps[0].y;

      // Segment d: mirrored from a around X
      p := @pts[SegD,0];
      ps := @pts[SegA,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[0].x;
      p[1].y := h-ps[0].y;
      p[2].x := ps[5].x;
      p[2].y := h-ps[5].y;
      p[3].x := ps[4].x;
      p[3].y := h-ps[4].y;
      p[4].x := ps[3].x;
      p[4].y := h-ps[3].y;
      p[5].x := ps[2].x;
      p[5].y := h-ps[2].y;

      // Segment e: mirrored from f around X
      p := @pts[SegE,0];
      ps := @pts[SegF,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[0].x;
      p[1].y := h-ps[0].y;
      p[2].x := ps[5].x;
      p[2].y := h-ps[5].y;
      p[3].x := ps[4].x;
      p[3].y := h-ps[4].y;
      p[4].x := ps[3].x;
      p[4].y := h-ps[3].y;
      p[5].x := ps[2].x;
      p[5].y := h-ps[2].y;
    end;

  ssBlock:
    begin
      for seg:=SegA to SegG do
        SetLength(pts[seg], 4);

      // Points are calculated in the upper left quadrant
      // See drawing
      // Segment a
      p := @pts[SegA,0];
      p[0].x := w-t-sx;
      p[0].y := t;
      p[1].x := p[0].x;
      p[1].y := 0;
      p[2].x := t+sx;
      p[2].y := 0;
      p[3].x := p[2].x;
      p[3].y := t;

      ws := p[0].x - p[3].x;

      // Segment f
      p := @pts[SegF,0];
      p[0].x := t;
      p[0].y := (h-s)/2;
      p[1].x := t;
      p[1].y := e/2;
      p[2].x := 0;
      p[2].y := e/2;
      p[3].x := 0;
      p[3].y := (h-s)/2;

      hs := p[0].y - p[1].y;

      // Segment g
      p := @pts[SegG,0];
      p[0].x := w-t-sx;
      p[0].y := (h+t)/2;
      p[1].x := p[0].x;
      p[1].y := (h-t)/2;
      p[2].x := t+sx;
      p[2].y := p[1].y;
      p[3].x := t+sx;
      p[3].y := p[0].y;

      // Segment b: mirrored from f around Y
      p := @pts[SegB,0];
      ps := @pts[SegF,0];
      p[0].x := w-ps[3].x;
      p[0].y := ps[3].y;
      p[1].x := w-ps[2].x;
      p[1].y := ps[2].y;
      p[2].x := w-ps[1].x;
      p[2].y := ps[1].y;
      p[3].x := w-ps[0].x;
      p[3].y := ps[0].y;

      // Segment c: mirrored from b around X
      p := @pts[SegC,0];
      ps := @pts[SegB,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[0].x;
      p[1].y := h-ps[0].y;
      p[2].x := ps[3].x;
      p[2].y := h-ps[3].y;
      p[3].x := ps[2].x;
      p[3].y := h-ps[2].y;

      // Segment d: mirrored from a around X
      p := @pts[SegD,0];
      ps := @pts[SegA,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[0].x;
      p[1].y := h-ps[0].y;
      p[2].x := ps[3].x;
      p[2].y := h-ps[3].y;
      p[3].x := ps[2].x;
      p[3].y := h-ps[2].y;

      // Segment e: mirrored from f around X
      p := @pts[SegE,0];
      ps := @pts[SegF,0];
      p[0].x := ps[1].x;
      p[0].y := h-ps[1].y;
      p[1].x := ps[0].x;
      p[1].y := h-ps[0].y;
      p[2].x := ps[3].x;
      p[2].y := h-ps[3].y;
      p[3].x := ps[2].x;
      p[3].y := h-ps[2].y;
    end;

{  ssOptimal:
    begin
      for seg in [SegA, SegB, SegC, SegE, SegD, SegF] do
        SetLength(pts[seg], 10);
      SetLength(pts[SeG], 6);

      // Points are calculated in the upper left quadrant
      // See drawing
      // Segment a
      p := @pts[SegA,0];
      p[0].x := t+sx;
      p[0].y := t;
      p[1].x := w-p[0].x;
      p[1].y := t;
      p[2].x := w-3*sx/2;
      p[2].y := sx/2;
      p[3].x := p[2].x;
      p[3].y := p[2].y;
      p[4].x := ;
      p[4].y := ;
      p[5].x := w-3*sx;
      p[5].y := ;
      p[6].x := ;
      p[6].y := ;
    end;}
  ss80th:
    begin
      for seg in [SegA, SegG, SegD] do
        SetLength(pts[seg], 4);
      for seg in [SegB, SegC, SegE, SegF] do
        SetLength(pts[seg], 7);

      // Points are calculated in the upper left quadrant
      // See drawing
      // Segment a
      p := @pts[SegA,0];
      p[0].x := w-t-sx;
      p[0].y := t;
      p[1].x := p[0].x;
      p[1].y := 0;
      p[2].x := t+sx;
      p[2].y := 0;
      p[3].x := p[2].x;
      p[3].y := t;

      ws := p[0].x - p[3].x;

      // Segment d
      p := @pts[SegD,0];
      ps := @pts[SegA,0];
      p[0].x := ps[0].x;
      p[0].y := h-t;
      p[1].x := ps[1].x;
      p[1].y := h;
      p[2].x := ps[2].x;
      p[2].y := h;
      p[3].x := ps[3].x;
      p[3].y := p[0].y;

      // Segment g
      p := @pts[SegG,0];
      p[0].x := ps[0].x;
      p[0].y := (h+t)/2;
      p[1].x := ps[1].x;
      p[1].y := (h-t)/2;
      p[2].x := ps[3].x;
      p[2].y := p[1].y;
      p[3].x := ps[2].x;
      p[3].y := p[0].y;

      // Segment F with points 3 and 4 as bezier control points
      p := @pts[SegF,0];
      p[0].x := t;
      p[0].y := (h-sx)/2;
      p[1].x := p[0].x;
      p[1].y := 0;
      p[2].x := ex/2;
      p[2].y := 0;
      p[3].x := ex/4;
      p[3].y := 0;
      p[4].x := 0;
      p[4].y := ex/4;
      p[5].x := 0;
      p[5].y := ex/2;
      p[6].x := 0;
      p[6].y := p[0].y;

      hs := p[0].y - p[1].y;

      // Segment B with points 3 and 4 as bezier control points
      p := @pts[SegB,0];
      ps := @pts[SegF,0];
      p[0].x := w-ps[0].x;
      p[0].y := ps[0].y;
      p[1].x := w-ps[1].x;
      p[1].y := ps[1].y;
      p[2].x := w-ps[2].x;
      p[2].y := ps[2].y;
      p[3].x := w-ps[3].x;
      p[3].y := ps[3].y;
      p[4].x := w-ps[4].x;
      p[4].y := ps[4].y;
      p[5].x := w-ps[5].x;
      p[5].y := ps[5].y;
      p[6].x := w-ps[6].x;
      p[6].y := ps[6].y;

      // Segment C with points 3 and 4 as bezier control points
      p := @pts[SegC,0];
      ps := @pts[SegB,0];
      p[0].x := ps[0].x;
      p[0].y := h-ps[0].y;
      p[1].x := ps[1].x;
      p[1].y := h-ps[1].y;
      p[2].x := ps[2].x;
      p[2].y := h-ps[2].y;
      p[3].x := ps[3].x;
      p[3].y := h-ps[3].y;
      p[4].x := ps[4].x;
      p[4].y := h-ps[4].y;
      p[5].x := ps[5].x;
      p[5].y := h-ps[5].y;
      p[6].x := ps[6].x;
      p[6].y := h-ps[6].y;

      // Segment E with points 3 and 4 as bezier control points
      p := @pts[SegE,0];
      ps := @pts[SegF,0];
      p[0].x := ps[0].x;
      p[0].y := h-ps[0].y;
      p[1].x := ps[1].x;
      p[1].y := h-ps[1].y;
      p[2].x := ps[2].x;
      p[2].y := h-ps[2].y;
      p[3].x := ps[3].x;
      p[3].y := h-ps[3].y;
      p[4].x := ps[4].x;
      p[4].y := h-ps[4].y;
      p[5].x := ps[5].x;
      p[5].y := h-ps[5].y;
      p[6].x := ps[6].x;
      p[6].y := h-ps[6].y;
    end;

  end;

  // Segment centers
  Cts[SegA].x := w/2;
  Cts[SegA].y := t/2;
  Cts[SegB].x := w-t/2;
  Cts[SegB].y := h/4;
  Cts[SegC].x := w-t/2;
  Cts[SegC].y := 3*h/4;
  Cts[SegD].x := w/2;
  Cts[SegD].y := h-t/2;
  Cts[SegE].x := t/2;
  Cts[SegE].y := 3*h/4;
  Cts[SegF].x := t/2;
  Cts[SegF].y := h/4;
  Cts[SegG].x := w/2;
  Cts[SegG].y := h/2;

  // Tilt
  if kt>0.0 then begin

    for seg:=SegA to SegG do
      for i:=0 to high(pts[seg]) do
        pts[seg,i].x += (h-pts[seg,i].y)*kt;

    for seg:=SegA to SegG do
      Cts[seg].x += (h-Cts[seg].y)*kt;

  end;

end;

function T7SegModuleType.DrawSymbol(ASymbol :char) :boolean;
var
  Segment :TSegment;
  UserSegments :TSegments;
  Gradient: TBGRAGradientScanner;
  Affine: TBGRAAffineScannerTransform;

  procedure DrawSegment(Segment :TSegment; Color :TColor);
  var
    i :integer;
    Path :TBGRAPath;
  begin
    Path := TBGRAPath.Create;
    try
      Path.beginPath;
      Path.moveTo(Pts[Segment][0]);
      case FDesign.Style of
      ssClassic, ssBlock:
        for i:=1 to High(Pts[Segment]) do
          Path.lineTo(Pts[Segment][i]);
      ss80th:
        begin
          if Segment in [SegA, SegD, SegG] then
            for i:=1 to High(Pts[Segment]) do
              Path.lineTo(Pts[Segment][i])
          else begin
            Path.lineTo(Pts[Segment][1]);
            Path.lineTo(Pts[Segment][2]);
            Path.bezierCurveTo(Pts[Segment,3], Pts[Segment,4], Pts[Segment,5]);
            Path.lineTo(Pts[Segment][6]);
          end;
        end;
      end;
      Path.closePath;
      if FDesign.InnerGlow and (Color <> FDesign.DarkColor) then begin
        Affine := TBGRAAffineScannerTransform.Create(Gradient);
        if Segment in [SegA, SegD, SegG] then begin // horizontal
          Affine.Scale(ws*2, t*2);
          affine.Translate(Cts[Segment].x, Cts[Segment].y);
        end else begin
          Affine.Scale(t*2, hs*2);
          Affine.RotateRad(-arctan(k/h));
          Affine.Translate(Cts[Segment].x, Cts[Segment].y);
        end;
        Path.fill(Bitmap, f, f, Affine);
        Affine.Free;
      end else
        Path.fill(Bitmap, f, f, Color);
    finally
      Path.Free;
    end;
  end;

begin
  if FDesign.InnerGlow then begin
    Gradient:= TBGRAGradientScanner.Create(FDesign.BrightColor, FDesign.BackgroundColor, gtRadial, PointF(0, 0), PointF(1.2,1.2), false, false);
  end;
  case ASymbol of
  ' ':
    begin
      if FDesign.DarkVisible then for Segment := SegA to SegG do
        DrawSegment(Segment, FDesign.DarkColor);
    end;
  '0'..'9':
    begin
      for Segment in SEG7SEGMENTS_CIPHER[ASymbol] do
        DrawSegment(Segment, FDesign.BrightColor);
      if FDesign.DarkVisible then
        for Segment := SegA to SegG do
          if not (Segment in SEG7SEGMENTS_CIPHER[ASymbol]) then
            DrawSegment(Segment, FDesign.DarkColor);
    end;
  'A'..'F':
    begin
      for Segment in SEG7SEGMENTS_CHAR[ASymbol] do
        DrawSegment(Segment, FDesign.BrightColor);
      if FDesign.DarkVisible then
        for Segment := SegA to SegG do
          if not (Segment in SEG7SEGMENTS_CHAR[ASymbol]) then
            DrawSegment(Segment, FDesign.DarkColor);
    end;
  '-':
    begin
      DrawSegment(SegG, FDesign.BrightColor);
      if FDesign.DarkVisible then
        for Segment := SegA to SegF do
          DrawSegment(Segment, FDesign.DarkColor);
    end;
{  'E':
    begin
      for Segment in SEG7SEGMENTS_E do
        DrawSegment(Segment, FDesign.BrightColor);
      if FDesign.DarkVisible then begin
        DrawSegment(SegB, FDesign.DarkColor);
        DrawSegment(SegC, FDesign.DarkColor);
      end;
    end;}
  '?':
    if Assigned(FDisplay.FOnDrawSegments) then begin
      UserSegments := [];
      FDisplay.FOnDrawSegments(FDisplay, FDisplay.FSymbolIndex, UserSegments);
      for Segment in UserSegments do
        DrawSegment(Segment, FDesign.BrightColor);
      if FDesign.DarkVisible then
        for Segment:=low(TSegment) to high(TSegment) do
          if not (Segment in UserSegments) then
            DrawSegment(Segment, FDesign.DarkColor);
    end;
  end;
  if FDesign.InnerGlow then begin
    Gradient.Free;
  end;
  result := false;
end;

{ T7SegDotModuleType }

constructor T7SegDotModuleType.Create(ADesign: TDesign);
begin
  inherited;
  DefineCharset(SEG7DOTCHARSET);
end;

function T7SegDotModuleType.GetId: string;
begin
  result := SEG7DOTID;
end;

function T7SegDotModuleType.Layout(AHeight: integer): integer;
var
  wd :integer;
begin
  Result := inherited Layout(AHeight);
  dc.y := h - t/2;
  dc.x := w + t/4 + t/2 + (h-dc.y)*kt;
  dd := t;

  // Check, if tilt covers dot
  wd := round(2*f + w + t/4 + t + (h-dc.y)*kt);
  if wd>result then
    result := wd;

end;

function T7SegDotModuleType.DrawSymbol(ASymbol: char) :boolean;
begin
  if FDot then begin
    result := inherited DrawSymbol(ASymbol);
    FDot := false;
  end else begin
    result := (ASymbol='.') or (ASymbol=',');
    if result then begin
      Bitmap.FillEllipseAntialias(dc.x, dc.y, dd/2, dd/2, FDesign.BrightColor);
      FDot := true;
    end else begin
      result := inherited DrawSymbol(ASymbol);
      Bitmap.FillEllipseAntialias(dc.x, dc.y, dd/2, dd/2, FDesign.DarkColor);
    end;
  end;
end;

{ TDoubleDotModuleType }

function TDoubleDotModuleType.GetId: string;
begin
  result := DOUBLEDOTID;
end;

function TDoubleDotModuleType.Layout(AHeight: integer): integer;
begin
  inherited Layout(AHeight);
  dc1.y := h/2 + 3*t/2;
  dc1.x := t/2 + (h-dc1.y)*kt;

  dc2.y := h/2 - 3*t/2;
  dc2.x := t/2 + (h-dc2.y)*kt;

  dd := t;
  Result := round(2*f + dc2.x + t/2);
end;

function TDoubleDotModuleType.DrawSymbol(ASymbol :char) :boolean;
begin
  if ASymbol=':' then begin
    Bitmap.FillEllipseAntialias(dc1.x, dc1.y, dd/2, dd/2, FDesign.BrightColor);
    Bitmap.FillEllipseAntialias(dc2.x, dc2.y, dd/2, dd/2, FDesign.BrightColor);
  end else if ASymbol=' ' then begin
    if FDesign.DarkVisible then begin
      Bitmap.FillEllipseAntialias(dc1.x, dc1.y, dd/2, dd/2, FDesign.DarkColor);
      Bitmap.FillEllipseAntialias(dc2.x, dc2.y, dd/2, dd/2, FDesign.DarkColor);
    end;
  end;
  result := false;
end;

constructor TDoubleDotModuleType.Create(ADesign: TDesign);
begin
  inherited;
  DefineCharset(DOUBLEDOTCHARSET);
end;

end.
