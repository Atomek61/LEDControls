unit Controls.Blinker;

///////////////////////////////////////////////////////////////////////////////
//
//  Controls.Blinker (c) 2018 Jan Schirrmacher, www.atomek.de
//
//  License: See package LedControls license file LedControlsLicense.txt
//
//  TBlinker is a simple control, wich hosts two ImageList-Images, usually
//  images of an Led (bright and dark).
//
//  Simple example:
//
//  Blinker := TBlinker.Create(self);
//  Blinker.Parent := self;
//  Blinker.ImageList := LedImageList;
//  Blinker.Visible := true;
//  Blinker.Mode := bmContinuously;
//  Blinker.Active := true;
//
//  The Mode bmKeepAlive requires an occasionally Active := true to keep
//  the control blinking. This is usefull to visualize backround processes.
//  With KeepAliveBlink := n you can control the intervals of required
//  activations to keep the blinker active.
//
///////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, Controls, ExtCtrls, SysUtils, Graphics, ImgList;

type
  TBlinkerMode = (bmContinuously, bmKeepAlive);

  { TBlinker }

  TBlinker = class(TGraphicControl)
  private
    FImageList :TImageList;
    FImageIndexOn: integer;
    FImageIndexOff: integer;
    FIsOn :boolean;
    FTimer :TTimer;
    FActive :boolean;
    FInterval: integer;
    FBlinkerMode: TBlinkerMode;
    FKeepAliveCounter :integer;
    FKeepAliveBlink: integer;
    FChangeLink :TChangeLink;
    procedure SetImageIndexOff(const Value: integer);
    procedure SetImageIndexOn(const Value: integer);
    procedure SetImageList(const Value: TImageList);
    procedure OnImageListChanged(Sender :TObject);
    procedure SetIsOn(const Value: boolean);
    procedure SetActive(const Value: boolean);
    procedure SetInterval(const Value: integer);
    procedure OnTimer(Sender :TObject);
    procedure SetMode(const Value: TBlinkerMode);
    procedure SetKeepAliveBlink(const Value: integer);
    procedure StartTimer;
    procedure StopTimer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
  published
    property ImageList :TImageList read FImageList write SetImageList;
    property ImageIndexOn :integer read FImageIndexOn write SetImageIndexOn;
    property ImageIndexOff :integer read FImageIndexOff write SetImageIndexOff;
    property IsOn :boolean read FIsOn write SetIsOn;
    property Active :boolean read FActive write SetActive;
    property Interval :integer read FInterval write SetInterval;
    property Mode :TBlinkerMode read FBlinkerMode write SetMode;
    property KeepAliveBlink :integer read FKeepAliveBlink write SetKeepAliveBlink;
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

{ TBlinker }

constructor TBlinker.Create(AOwner: TComponent);
begin
  inherited;
  FImageIndexOff := 0;
  FImageIndexOn := 1;
  FInterval := 500;
  FIsOn := false;
  FActive := false;
  FBlinkerMode := bmContinuously;
  FKeepAliveBlink := 1;
  Width := 20;
  Height := 20;
end;

destructor TBlinker.Destroy;
begin
  StopTimer;
  FChangeLink.Free;
  inherited;
end;

procedure TBlinker.Paint;
var
  Index :integer;
  r :TRect;
begin
  inherited;
  if Assigned(FImageList) then begin
    if FIsOn then
      Index := FImageIndexOn
    else
      Index := FImageIndexOff;
    with ImageList do
      if (Index>=0) and (Index<Count) then
        ImageList.Draw(Canvas,
          (ClientWidth-Width-1) div 2,
          (ClientHeight-Height-1) div 2,
          Index);
  end else if csDesigning in ComponentState then begin
    r := ClientRect;
    with Canvas do begin
      Pen.Color := clNavy;
      MoveTo(r.Left, r.Top); LineTo(r.Right+1, r.Bottom+1);
      MoveTo(r.Left, r.Bottom); LineTo(r.Right+1, r.Top-1);
    end;
  end;
end;

procedure TBlinker.SetMode(const Value: TBlinkerMode);
begin
  if Value<>FBlinkerMode then begin
    FBlinkerMode := Value;
  end;
end;

procedure TBlinker.StartTimer;
begin
  FTimer := TTimer.Create(self);
  FTimer.Interval := FInterval;
  FTimer.OnTimer := @OnTimer;
  FTimer.Enabled := true;
end;

procedure TBlinker.StopTimer;
begin
    FreeAndNil(FTimer);
end;

procedure TBlinker.SetActive(const Value: boolean);
begin
  case FBlinkerMode of
  bmContinuously:
    if Value<>FActive then begin
      FActive := Value;
      IsOn := Value;
      if FActive then
        startTimer
      else
        stopTimer;
    end;
  bmKeepAlive:
    begin
      if FActive then begin
        if Value then begin
          FKeepAliveCounter := FKeepAliveBlink;
        end else begin
          FKeepAliveCounter := 0;
        end;
      end else begin
        if Value then begin
          FKeepAliveCounter := FKeepAliveBlink;
          IsOn := true;
          startTimer;
        end;
      end;
      FActive := Value;
    end;
  end;
end;

procedure TBlinker.OnTimer(Sender: TObject);
begin
  case FBlinkerMode of
  bmContinuously:
    begin
      IsOn := not IsOn;
    end;
  bmKeepAlive:
    begin
      if FKeepAliveCounter=0 then begin
        IsOn := false;
        StopTimer;
        FActive := false;
      end else begin
        if IsOn then
          dec(FKeepAliveCounter);
        IsOn := not IsOn;
      end;
    end;
  end;
end;

procedure TBlinker.SetImageIndexOff(const Value: integer);
begin
  if (Value<>FImageIndexOff) and (Value>=0) then begin
    FImageIndexOff := Value;
    Invalidate;
  end;
end;

procedure TBlinker.SetImageIndexOn(const Value: integer);
begin
  if (Value<>FImageIndexOn) and (Value>=0) then begin
    FImageIndexOn := Value;
    Invalidate;
  end;
end;

procedure TBlinker.SetImageList(const Value: TImageList);
begin
  if Value=FImageList then Exit;
  if Assigned(FImageList) then
    FImageList.UnRegisterChanges(FChangeLink);
  if Assigned(Value) then begin
    if not Assigned(FChangeLink) then begin
      FChangeLink := TChangeLink.Create;
      FChangeLink.OnChange := @OnImagelistChanged;
    end;
    Value.RegisterChanges(FChangeLink);
    if FImageIndexOff>Value.Count-1 then
      FImageIndexOff := Value.Count-1;
    if FImageIndexOn>Value.Count-1 then
      FImageIndexOn := Value.Count-1;
  end;
  FImageList := Value;
  OnImagelistChanged(self);
  Invalidate;
end;

procedure TBlinker.OnImageListChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TBlinker.SetInterval(const Value: integer);
begin
  if (Value<>FInterval) and (Value>=0) then begin
    FInterval := Value;
    if Assigned(FTimer) then
      FTimer.Interval := FInterval;
  end;
end;

procedure TBlinker.SetIsOn(const Value: boolean);
begin
  if Value<>FIsOn then begin
    FIsOn := Value;
    Invalidate;
  end;
end;

procedure TBlinker.SetKeepAliveBlink(const Value: integer);
begin
  if (Value<>FKeepAliveBlink) and (Value>=1) then begin
    FKeepAliveBlink := Value;
  end;
end;

end.
