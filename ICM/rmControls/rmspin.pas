{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmSpin
Purpose  : Contains Multiple "Spin" edit controls.
Date     : 09-03-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmSpin;

interface

{$I CompilerDefines.INC}

uses Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Menus, Buttons, rmBaseEdit, rmSpeedBtns;

type

{ TrmCustomSpinEdit }

  TrmCustomSpinEdit = class(TrmCustomEdit)
  private
    FButton: TrmSpinButton;
    FEditorEnabled: Boolean;
    fUseRanges: boolean;
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;

    {$ifdef D4_OR_HIGHER}
    procedure SetEnabled(value:Boolean); reintroduce; (* reintroduce is D4 Modification *)
    function GetEnabled:Boolean; reintroduce; (* reintroduce is D4 Modification *)
    {$else}
    procedure SetEnabled(value:Boolean);
    function GetEnabled:Boolean;
    {$endif}

    procedure SetUseRanges(const Value: boolean);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure UpClick (Sender: TObject);
    procedure DownClick (Sender: TObject);

    function IsValidChar(Key: Char): Boolean; virtual;
    procedure DecValue; virtual; Abstract;
    procedure IncValue; virtual; Abstract;
    procedure ExitCheck; virtual; Abstract;
    procedure InternalUpdate; virtual; Abstract;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    property Button: TrmSpinButton read FButton;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property UseRanges: boolean read fUseRanges write SetUseRanges default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TrmSpinEdit }

  TrmSpinEdit = class(TrmCustomSpinEdit)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FIncrement: Integer;
    function GetValue: Integer;
    function CheckValue (NewValue: Integer): Integer;
    procedure SetValue (NewValue: Integer);
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure IncValue; override;
    procedure DecValue; override;
    procedure ExitCheck; override;
    procedure InternalUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Increment: Integer read FIncrement write FIncrement default 1;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property MinValue: Integer read FMinValue write SetMinValue;
    property Value: Integer read GetValue write SetValue;

    property UseRanges;

    {$ifdef D4_OR_HIGHER}
    property Anchors;
    property Constraints;
    {$endif}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TrmFloatSpinEdit }

  TrmFloatSpinEdit = class(TrmCustomSpinEdit)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FIncrement: Double;
    function GetValue: Double;
    function CheckValue (NewValue: Double): Double;
    procedure SetValue (NewValue: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinValue(const Value: Double);
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure IncValue; override;
    procedure DecValue; override;
    procedure ExitCheck; override;
    procedure InternalUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Increment: Double read FIncrement write FIncrement;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property Value: Double read GetValue write SetValue;

    property UseRanges;

    {$ifdef D4_OR_HIGHER}
    property Anchors;
    property Constraints;
    {$endif}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TrmTimeSpinEdit }

  TrmTimeDisplay = (td24Hour, td12Hour);

  TrmTimeSpinEdit = class(TrmCustomSpinEdit)
  private
    FMinValue: TTime;
    FMaxValue: TTime;
    fTimeDisplay: TrmTimeDisplay;
    fHour: shortint;
    fMinute: shortint;
    fSecond: shortint;
    fFormat: string;
    fShowSeconds: boolean;
    function CheckValue (NewValue: TTime): TTime;
    function GetValue: TTime;
    function GetMaxValue:TTime;
    function GetMinValue:TTime;
    procedure SetValue(NewValue: TTime);
    procedure SetMaxValue(const Value: TTime);
    procedure SetMinValue(const Value: TTime);
    procedure SetTimeDisplay(const Value: TrmTimeDisplay);
    procedure SetShowSeconds(const Value: boolean);
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure IncValue; override;
    procedure DecValue; override;
    procedure ExitCheck; override;
    procedure InternalUpdate; override;
    procedure UpdateText;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: TTime read GetMaxValue write SetMaxValue;
    property MinValue: TTime read GetMinValue write SetMinValue;
    property TimeDisplay:TrmTimeDisplay read fTimeDisplay write SetTimeDisplay default td24hour;
    property ShowSeconds:boolean read fShowSeconds write SetShowSeconds default false;

    property Value: TTime read GetValue write SetValue;
    property UseRanges;

    {$ifdef D4_OR_HIGHER}
    property Anchors;
    property Constraints;
    {$endif}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{ TrmCustomSpinEdit }

constructor TrmCustomSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TrmSpinButton.Create (Self);
  with FButton Do
  begin
     align := alRight;
     Visible := True;
     Parent := Self;
     FocusControl := Self;
     OnUpClick := UpClick;
     OnDownClick := DownClick;
  end;
  fUseRanges := false;
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
end;

destructor TrmCustomSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TrmCustomSpinEdit.DownClick(Sender: TObject);
begin
  if ReadOnly then
     MessageBeep(0)
  else
  begin
     if FButton.DownEnabled then
        DecValue;
  end;
end;

procedure TrmCustomSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then
  begin
     UpClick (Self);
     key := 0;
  end
  else if Key = VK_DOWN then
  begin
     DownClick (Self);
     key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TrmCustomSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TrmCustomSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TrmCustomSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TrmCustomSpinEdit.SetEditRect;
var
  R: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));
  R.Right := ClientWidth - FButton.Width - 1;
  R.Top := 0;
  R.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));  {debug}
end;

procedure TrmCustomSpinEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  if NewStyleControls and Ctl3D then
     FButton.SetBounds((width - FButton.Width) - 4, 0, FButton.Width, Height - 4)
  else
     FButton.SetBounds (width - FButton.Width, 1, FButton.Width, Height - 2);
  SetEditRect;
end;

procedure TrmCustomSpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then
     MessageBeep(0)
  else
  begin
     if FButton.UpEnabled then
        IncValue;
  end;
end;

procedure TrmCustomSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomSpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TrmCustomSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  ExitCheck;
end;

procedure TrmCustomSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//Do nothing;
end;

function TrmCustomSpinEdit.GetEnabled: Boolean;
begin
   result := inherited Enabled;
end;

procedure TrmCustomSpinEdit.SetEnabled(value: Boolean);
begin
   inherited enabled := value;
   fButton.enabled := value;
end;

function TrmCustomSpinEdit.IsValidChar(Key: Char): Boolean;
begin
   result := false;
end;

procedure TrmCustomSpinEdit.SetUseRanges(const Value: boolean);
begin
  fUseRanges := Value;
  InternalUpdate;
end;

{ TrmSpinEdit }

constructor TrmSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  FIncrement := 1;
end;

function TrmSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

function TrmSpinEdit.GetValue: Integer;
begin
  try
    Result := StrToInt (Text);
  except
    Result := FMinValue;
  end;
end;

procedure TrmSpinEdit.SetValue (NewValue: Integer);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

function TrmSpinEdit.CheckValue (NewValue: Integer): Integer;
begin
  Result := NewValue;
  if UseRanges then
  begin
     if NewValue < FMinValue then
       Result := FMinValue
     else if NewValue > FMaxValue then
       Result := FMaxValue;

     Button.UpEnabled := (NewValue < FMaxValue);
     Button.DownEnabled := (NewValue > FMinValue);
  end
  else
  begin
     Button.UpEnabled := true;
     Button.DownEnabled := true;
  end;
end;

procedure TrmSpinEdit.DecValue;
begin
  Value := Value - FIncrement;
  selectall;
end;

procedure TrmSpinEdit.IncValue;
begin
  Value := Value + FIncrement;
  selectall;
end;

procedure TrmSpinEdit.ExitCheck;
begin
  if CheckValue (Value) <> Value then
     SetValue (Value);
end;

procedure TrmSpinEdit.SetMaxValue(const Value: Integer);
begin
  if value >= fMinValue then
  begin
     FMaxValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmSpinEdit.SetMinValue(const Value: Integer);
begin
  if Value <= fMaxValue then
  begin
     FMinValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmSpinEdit.InternalUpdate;
begin
  value := CheckValue(GetValue);
end;

{ TrmFloatSpinEdit }

function TrmFloatSpinEdit.CheckValue(NewValue: Double): Double;
begin
  Result := NewValue;
  if UseRanges then
  begin
     if NewValue < FMinValue then
       Result := FMinValue
     else if NewValue > FMaxValue then
       Result := FMaxValue;

     Button.UpEnabled := (NewValue < FMaxValue);
     Button.DownEnabled := (NewValue > FMinValue);
  end
  else
  begin
     Button.UpEnabled := true;
     Button.DownEnabled := true;
  end;
end;

constructor TrmFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  FIncrement := 1;
end;

procedure TrmFloatSpinEdit.DecValue;
begin
  Value := Value - FIncrement;
  selectall;
end;

procedure TrmFloatSpinEdit.ExitCheck;
begin
  if CheckValue (Value) <> Value then
     SetValue (Value);
end;

function TrmFloatSpinEdit.GetValue: Double;
begin
  try
    Result := StrToFloat(Text);
  except
    Result := FMinValue;
  end;
end;

procedure TrmFloatSpinEdit.IncValue;
begin
  Value := Value + FIncrement;
  selectall;
end;

procedure TrmFloatSpinEdit.InternalUpdate;
begin
   Value := CheckValue(GetValue);  
end;

function TrmFloatSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TrmFloatSpinEdit.SetMaxValue(const Value: Double);
begin
  if value >= fMinValue then
  begin
     FMaxValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmFloatSpinEdit.SetMinValue(const Value: Double);
begin
  if value <= fMaxValue then
  begin
     FMinValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmFloatSpinEdit.SetValue(NewValue: Double);
begin
  Text := FormatFloat('#######0.00', CheckValue (NewValue));
end;

{ TrmTimeSpinEdit }

function TrmTimeSpinEdit.CheckValue(NewValue: TTime): TTime;
begin
  Result := NewValue;
  if UseRanges then
  begin
     if NewValue < FMinValue then
       Result := FMinValue
     else if NewValue > FMaxValue then
       Result := FMaxValue;

     Button.UpEnabled := (NewValue < FMaxValue);
     Button.DownEnabled := (NewValue > FMinValue);
  end
  else
  begin
     Button.UpEnabled := true;
     Button.DownEnabled := true;
  end;
end;

constructor TrmTimeSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  fTimeDisplay := td24Hour;
  fShowSeconds := false;
  fFormat := 'hh:mm:ss';
  fMinValue := EncodeTime(12,0,0,0);
  fMaxValue := EncodeTime(12,0,0,0);
  Value := EncodeTime(12,0,0,0);
end;

procedure TrmTimeSpinEdit.DecValue;
var
   wss : integer;
   wsl : integer;
   wNewTime : TTime;
begin
   wss := 0;
   wsl := 0;
   case fTimeDisplay of
   td24Hour:
     begin
        case SelStart of
          0, 1, 2:begin
                 dec(fHour);
                 if fhour < 0 then
                    fhour := 23;
                 wss := 0;
                 wsl := 2;
               end;
          3, 4, 5:begin
                 dec(fMinute);
                 if fMinute < 0 then
                    fMinute := 59;
                 wss := 3;
                 wsl := 2;
               end;
          6, 7, 8:begin
                 dec(fSecond);
                 if fSecond < 0 then
                    fSecond := 59;
                 wss := 6;
                 wsl := 2;
               end;
        end;
     end;
   td12Hour:
     begin
        case SelStart of
          0, 1, 2:begin
                    if fHour > 11 then
                    begin
                       dec(fHour);
                       if fhour < 12 then
                          fhour := 23;
                    end
                    else
                    begin
                       dec(fHour);
                       if fhour < 0 then
                          fhour := 11;
                    end;
                    wss := 0;
                    wsl := 2;
                  end;
          3, 4, 5:begin
                    dec(fMinute);
                    if fMinute < 0 then
                       fMinute := 59;
                    wss := 3;
                    wsl := 2;
                  end;
          6, 7, 8:begin
                     if fShowSeconds then
                     begin
                        dec(fSecond);
                        if fSecond < 0 then
                           fSecond := 59;
                        wss := 6;
                        wsl := 2;
                     end
                     else
                     begin
                        inc(fHour, 12);
                        if fhour > 23 then
                           dec(fhour, 24);
                        wss := 6;
                        wsl := 3;
                     end;
                  end;
          9, 10, 11:begin
                        inc(fHour, 12);
                        if fhour > 23 then
                           dec(fhour, 24);
                        wss := 9;
                        wsl := 3;
                    end;
        end;
     end;
   end;
   wNewTime := CheckValue(value);
   Value := wNewTime;
   UpdateText;
   SelStart := wss;
   SelLength := wsl;
end;

procedure TrmTimeSpinEdit.ExitCheck;
begin
  if CheckValue (Value) <> Value then
     SetValue (Value);
end;

function TrmTimeSpinEdit.GetMaxValue: TTime;
var
   wh, wm, ws, wms : word;
begin
   if csdesigning in componentstate then
   begin
      decodetime(FMaxValue, wh, wm, ws, wms);
      result := encodetime(wh, wm, ws, 1);
   end
   else
   result := fMaxValue;
end;

function TrmTimeSpinEdit.GetMinValue: TTime;
var
   wh, wm, ws, wms : word;
begin
   if csdesigning in componentstate then
   begin
      decodetime(FMinValue, wh, wm, ws, wms);
      result := encodetime(wh, wm, ws, 1);
   end
   else
   result:= fMinValue;
end;

function TrmTimeSpinEdit.GetValue: TTime;
begin
  try
     if csdesigning in componentstate then
        Result := EncodeTime(fHour, fminute, fsecond, 1)
     else
        result := EncodeTime(fHour, fminute, fsecond, 0);
  except
    Result := FMinValue;
  end;
end;

procedure TrmTimeSpinEdit.IncValue;
var
   wss : integer;
   wsl : integer;
   wNewTime : TTime;
begin
   wss := 0;
   wsl := 0;
   case fTimeDisplay of
   td24Hour:
     begin
        case SelStart of
          0, 1, 2:begin
                 inc(fHour);
                 if fhour > 23 then
                    fhour := 0;
                 wss := 0;
                 wsl := 2;
               end;
          3, 4, 5:begin
                 inc(fMinute);
                 if fMinute > 59 then
                    fMinute := 0;
                 wss := 3;
                 wsl := 2;
               end;
          6, 7, 8:begin
                 inc(fSecond);
                 if fSecond > 59 then
                    fSecond := 0;
                 wss := 6;
                 wsl := 2;
               end;
        end;
     end;
   td12Hour:
     begin
        case SelStart of
          0, 1, 2:begin
                 if fHour > 11 then
                 begin
                    inc(fHour);
                    if fhour > 23 then
                       fhour := 12;
                 end
                 else
                 begin
                    inc(fHour);
                    if fhour > 11 then
                       fhour := 0;
                 end;
                 wss := 0;
                 wsl := 2;
               end;
          3, 4, 5:begin
                 inc(fMinute);
                 if fMinute > 59 then
                    fMinute := 0;
                 wss := 3;
                 wsl := 2;
               end;
          6, 7, 8:begin
                     if fShowSeconds then
                     begin
                        inc(fSecond);
                        if fSecond > 59 then
                           fSecond := 0;
                        wss := 6;
                        wsl := 2;
                     end
                     else
                     begin
                        inc(fHour, 12);
                        if fhour > 23 then
                           dec(fhour, 24);
                        wss := 6;
                        wsl := 3;
                     end;
               end;
          9, 10, 11:begin
                        inc(fHour, 12);
                        if fhour > 23 then
                           dec(fhour, 24);
                        wss := 9;
                        wsl := 3;
                    end;
        end;
     end;
   end;
   wNewTime := CheckValue(value);
   Value := wNewTime;
   UpdateText;
   SelStart := wss;
   SelLength := wsl;
end;

procedure TrmTimeSpinEdit.InternalUpdate;
begin
   value := CheckValue(Value);
end;

function TrmTimeSpinEdit.IsValidChar(Key: Char): Boolean;
var
   wNewTime : TTime;
   wStr : string;
   wMs, wh, wm, ws : word;
begin
   result := false;
   wNewTime := 0.0; 
   wstr := Text;
   case fTimeDisplay of
     td24Hour:
       begin
          case SelStart of
            0, 1, 3, 4, 6, 7:
               begin
                   wstr[SelStart+1] := key;
                   try
                      wNewTime := CheckValue(StrToTime(wstr));
                      result := true;
                   except
                      result := false;
                   end;
               end;
          else
             result := false;
          end;
       end;
     td12Hour:
       begin
          case SelStart of
            0, 1, 3, 4:
            begin
               wstr[SelStart+1] := key;
               try
                  wNewTime := CheckValue(StrToTime(wstr));
                  result := true;
               except
                  result := false;
               end;
            end;
            6, 7, 8, 9, 10, 11 :
            begin
               if fShowSeconds then
               begin
                  if (selStart = 6) or (selStart = 7) then
                  begin
                     wstr[SelStart+1] := key;
                     try
                        wNewTime := CheckValue(StrToTime(wstr));
                        result := true;
                     except
                        result := false;
                     end;
                  end
{
//This has been commented out until I can figure out a nice way of updating the
//text with out screwing up the values...
                  else if (selstart = 9) then
                  begin
                     result := (key in ['a','A','p','P']);
                     wstr[SelStart+1] := key;
                     try
                        wNewTime := CheckValue(StrToTime(wstr));
                        result := true;
                     except
                        result := false;
                     end;
                  end    }
                  else
                  result := false;
               end
               else
               begin
{
//This has been commented out until I can figure out a nice way of updating the
//text with out screwing up the values...
                  if (selstart = 6) then
                  begin
                     result := (key in ['a','A','p','P']);
                     wstr[SelStart+1] := key;
                     try
                        wNewTime := CheckValue(StrToTime(wstr));
                        result := true;
                     except
                        result := false;
                     end;
                  end
                  else   }
                  result := false;
               end;
            end;
          else
             result := false;
          end;
       end;
   end;
   if result then
   begin
      sellength := 1;
      DecodeTime(wNewTime, wh, wm, ws, wms);
      fhour := wh;
      fMinute := wm;
      fSecond := ws;
   end;
end;

procedure TrmTimeSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if key = vk_Delete then
     key := 0;
  inherited;
end;

procedure TrmTimeSpinEdit.SetMaxValue(const Value: TTime);
begin
  if value >= fMinValue then
  begin
     FMaxValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmTimeSpinEdit.SetMinValue(const Value: TTime);
begin
  if value <= fMaxValue then
  begin
     FMinValue := Value;
     CheckValue(GetValue);
  end;
end;

procedure TrmTimeSpinEdit.SetShowSeconds(const Value: boolean);
begin
  fShowSeconds := Value;
  InternalUpdate;
end;

procedure TrmTimeSpinEdit.SetTimeDisplay(const Value: TrmTimeDisplay);
begin
  if value <> fTimedisplay then
  begin
     fTimeDisplay := Value;
     InternalUpdate;
  end;
end;

procedure TrmTimeSpinEdit.SetValue(NewValue: TTime);
var
   wms : word;
   wh, wm, ws : word;
begin
   decodeTime(NewValue, wh, wm, ws, wms);
   fHour := wh; 
   fMinute := wm;
   fSecond := ws;
   UpdateText;
end;

procedure TrmTimeSpinEdit.UpdateText;
   function LeftPad(st:string; ch:char; len:integer):string;
   begin
      while length(st) < len do
            st := ch+st;
      result := st;
   end;
var
   wStr : string;
   wAM : boolean;
begin
   case fTimeDisplay of
      td24Hour :
        begin
           wstr := leftpad(inttostr(fHour),'0',2)+TimeSeparator+leftpad(inttostr(fMinute),'0', 2);
           if fShowSeconds then
              wstr := wstr+TimeSeparator+leftpad(inttostr(fsecond),'0', 2);
        end;
      td12Hour :
        begin
           wAM := (fHour-12 < 0);
           if wAm then
           begin
              if fhour > 0 then
                 wstr := leftpad(inttostr(fHour),' ',2)
              else
                 wStr := '12';
           end
           else
           begin
              if fhour-12 > 0 then
                 wStr := leftpad(inttostr(fHour-12),' ',2)
              else
                 wStr := '12';
           end;
           wstr := wstr+TimeSeparator+leftpad(inttostr(fMinute),'0', 2);
           if fShowSeconds then
              wstr := wstr+TimeSeparator+leftpad(inttostr(fsecond),'0', 2);

           if wAm then
              wstr := wstr + ' ' + TimeAMString
           else
              wstr := wstr + ' ' + TimePMString;
        end;
   end;
   text := wstr;
end;

end.
