unit EnhEdit;

{ TEnhancedEdit (C)opyright 2000 Version 1.50
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponente ist eine Ableitung von TEdit, welche eine Ausrichtung
  des Textes erlaubt und neben anderen zusätzlichen Funktionen auch
  Value-Eigenschaften für Zahlenwerte bietet. Sie ist Public Domain,
  das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Messages, Classes,
  Controls, StdCtrls, Clipbrd, SysUtils, Forms, Graphics;

type
  TEnhancedEdit = class(TEdit)
  private
    FAcceptChars         : boolean;
    FAlignment           : TAlignment;
    FCanvas              : TControlCanvas;
    FDigits              : byte;
    FExitOnEnterKey      : boolean;
    FFormat              : TFloatFormat;
    {$IFDEF SR_Delphi2_Up}
    FGrayDisabled         : boolean;
    {$ENDIF}
    FPrecision            : byte;
    FOldText              : string;
    FUndoOnEscKey         : boolean;
    FValue                : extended;
    FValueInt             : integer;

    function FormatEditString(AValue:extended):string;
    function GetParentForm:TControl;

  protected
    procedure Change; override;
    procedure CreateParams(var Params:TCreateParams); override;
    procedure KeyPress(var Key:Char); override;

    procedure SetAcceptChars(NewValue:boolean);
    procedure SetAlignment(NewValue:TAlignment);
    procedure SetDigits(NewValue:byte);
    procedure SetFormat(NewValue:TFloatFormat);
    {$IFDEF SR_Delphi2_Up}
    procedure SetGrayDisabled(NewValue:boolean);
    {$ENDIF}
    procedure SetPrecision(NewValue:byte);
    procedure SetValue(NewValue:extended);
    procedure SetValueInt(NewValue:integer);

    procedure CMEnter(var Msg: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    {$IFDEF SR_Delphi2_Up}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetTextMargins: TPoint;
    {$ENDIF}

  public
    constructor Create(AOwner:TComponent); override;
    procedure Clear; {$IFDEF SR_Delphi3_Up} override;{$ENDIF}
    destructor Destroy; override;

  published
    property AcceptChars: boolean read FAcceptChars write SetAcceptChars default true;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Digits: byte read FDigits write SetDigits default 2;
    property ExitOnEnterKey: boolean read FExitOnEnterKey write FExitOnEnterKey default false;
    property Format: TFloatFormat read FFormat write SetFormat;
    {$IFDEF SR_Delphi2_Up}
    property GrayDisabled: boolean read FGrayDisabled write SetGrayDisabled default true;
    {$ENDIF}
    property Precision: byte read FPrecision write SetPrecision default 8;
    property UndoOnEscKey: boolean read FUndoOnEscKey write FUndoOnEscKey default false;
    property Value: extended read FValue write SetValue;
    property ValueInt: integer read FValueInt write SetValueInt;

  end;

procedure Register;

implementation

uses SRUtils;

{$IFDEF SR_Delphi1}
{$R *.D16}
{$ELSE}
{$R *.D32}
{$ENDIF}

{$IFNDEF SR_Delphi3_Up}
function LastDelimiter(AChar:char;AText:string):integer;
var i : integer;
begin
  Result:=0;
  if length(AText)=0 then
    Exit;
  for i:=length(AText) downto 1 do begin
    if AText[i]=AChar then begin
      Result:=i;
      Exit;
    end;
  end;
end;
{$ENDIF}

{ Komponente TEnhancedEdit }
Constructor TEnhancedEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FAcceptChars:=true;
  FAlignment:=taLeftJustify;
  FDigits:=2;
  FExitOnEnterKey:=false;
  FFormat:=ffNumber;
  {$IFDEF SR_Delphi2_Up}
  FGrayDisabled:=true;
  {$ENDIF}
  FPrecision:=8;
  FUndoOnEscKey:=false;
end;

destructor TEnhancedEdit.Destroy;
begin
  inherited destroy;
end;

procedure TEnhancedEdit.Change;
var Temp  : string;
    i,L,P : integer;
begin
  try
    Temp:=Text;
    L:=length(Temp);
    if L>0 then begin
      if not FAcceptChars then begin
        P:=LastDelimiter(DecimalSeparator, Temp);
        if FDigits>0 then begin
          { Längenbegrenzung auf FDigits Nachkomma-Stellen }
          if (P>0) and (P<(L-FDigits)) then
            delete(Temp, P+FDigits+1, L-P+FDigits);
          L:=length(Temp);
        end;
        { Ungültige Zeichen entfernen }
        for i:=L downto 1 do begin
          if (i<P) and (Temp[i]=DecimalSeparator) then
            delete(Temp, i, 1);
          if (Temp[i]<>'-') and (Temp[i]<>DecimalSeparator) and
           (Temp[i]<>ThousandSeparator) and not (Temp[i] in ['0'..'9']) then
            delete(Temp, i, 1);
        end;
      end;
      if Temp<>Text then begin
        i:=SelStart;
        Text:=Temp;
        SelStart:=i;
      end;
      L:=length(Temp);
      for i:=L downto 1 do
        if Temp[i]=ThousandSeparator then
          delete(Temp, i, 1);
      FValue:=StrToFloatDef(Temp, 0);
      FValueInt:=trunc(FValue);
    end
    else begin
      FValue:=0;
      FValueInt:=0;
    end;
  except
    FValue:=0;
    FValueInt:=0;
  end;
  inherited Change;
end;

procedure TEnhancedEdit.Clear;
begin
  SetWindowText(Handle, '');
  Change;
end;

procedure TEnhancedEdit.CreateParams(var Params: TCreateParams);
{$IFDEF SR_Delphi1}
const
   Alignments: array[TAlignment] of longint =
    (ES_LEFT,ES_RIGHT,ES_CENTER);
{$ELSE}
const
   Alignments: array[TAlignment] of DWord =
    (ES_LEFT,ES_RIGHT,ES_CENTER);
{$ENDIF}
begin
   inherited CreateParams(Params);
   Params.Style:=Params.Style or
                 ES_MULTILINE or
                 Alignments[FAlignment];
end;

function TEnhancedEdit.FormatEditString(AValue:extended):string;
begin
  Result:=FloatToStrF(AValue, FFormat, FPrecision, FDigits);
end;

function TEnhancedEdit.GetParentForm:TControl;
var AParent : TControl;
begin
  Result:=Self;
  repeat
    AParent:=Result.Parent;
    if assigned(AParent) then
      Result:=AParent;
  until not assigned(AParent);
end;

procedure TEnhancedEdit.KeyPress(var Key:Char);
var AParentForm : TControl;
begin
  inherited KeyPress(Key);
  if (Key=#13) or (Key=#10) then begin
    Key:=#0;
    if ExitOnEnterKey then begin
      AParentForm:=GetParentForm;
      (AParentForm as TControl).Perform(WM_NextDlgCtl, 0, 0);
    end;
  end;
  if (Key=#27) and UndoOnEscKey then
    Text:=FOldText;
end;

procedure TEnhancedEdit.SetAcceptChars(NewValue:boolean);
begin
  if FAcceptChars<>NewValue then begin
    FAcceptChars:=NewValue;
    Change;
  end;
end;

procedure TEnhancedEdit.SetAlignment(NewValue:TAlignment);
begin
  if FAlignment<>NewValue then begin
    FAlignment:=NewValue;
    RecreateWnd;
  end;
end;

procedure TEnhancedEdit.SetDigits(NewValue:byte);
begin
  if FDigits<>NewValue then begin
    FDigits:=NewValue;
    try
      Text:=FormatEditString(FValue);
    except
      Text:='';
    end;
    Invalidate;
  end;
end;

procedure TEnhancedEdit.SetFormat(NewValue:TFloatFormat);
begin
  if FFormat<>NewValue then begin
    FFormat:=NewValue;
    try
      Text:=FormatEditString(FValue);
    except
      Text:='';
    end;
    Invalidate;
  end;
end;

{$IFDEF SR_Delphi2_Up}
procedure TEnhancedEdit.SetGrayDisabled(NewValue:boolean);
begin
  if FGrayDisabled<>NewValue then begin
    FGrayDisabled:=NewValue;
    if not Enabled then
      Invalidate;
  end;
end;
{$ENDIF}

procedure TEnhancedEdit.SetPrecision(NewValue:byte);
begin
  if FPrecision<>NewValue then begin
    FPrecision:=NewValue;
    try
      Text:=FormatEditString(FValue);
    except
      Text:='';
    end;
    Invalidate;
  end;
end;

procedure TEnhancedEdit.SetValue(NewValue:extended);
begin
  if FValue<>NewValue then begin
    FValue:=NewValue;
    try
      Text:=FormatEditString(FValue);
    except
      Text:='';
    end;
    Invalidate;
  end;
end;

procedure TEnhancedEdit.SetValueInt(NewValue:integer);
begin
  if FValueInt<>NewValue then begin
    FValueInt:=NewValue;
    try
      Text:=FormatEditString(FValueInt);
    except
      Text:='';
    end;
    Invalidate;
  end;
end;

procedure TEnhancedEdit.CMEnter(var Msg:TCMGotFocus);
begin
  FOldText:=Text;
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TEnhancedEdit.CMExit(var Message: TWMNoParams);
begin
  inherited;
end;

procedure TEnhancedEdit.WMPaste(var Message: TWMPaste);
var
  SGlobalHnd : THandle;
  Ptr        : PChar;
  Size,i     : Longint;
  s          : string;
begin
  s:='';
  if OpenClipboard(Handle) then begin
    try
      if Clipboard.HasFormat(CF_TEXT) then begin
        SGlobalHnd:=GetClipboardData(CF_TEXT);
        if SGlobalHnd<>0 then begin
          Size:=GlobalSize(SGlobalHnd);
          Ptr:=GlobalLock(SGlobalHnd);
          if Ptr<>nil then begin
            i:=0;
            while (i<size) and (Ptr[i]>=#32) do begin
              s:=s+Ptr[i];
              inc(i);
            end;
          end;
          GlobalUnlock(SGlobalHnd);
        end;
      end;
    finally
      CloseClipboard;
    end;
  end;
  SelText:=s;
end;

{$IFDEF SR_Delphi2_Up}
function TEnhancedEdit.GetTextMargins: TPoint;
var DC         : HDC;
    SaveFont   : HFont;
    i          : Integer;
    SysMetrics : TTextMetric;
    Metrics    : TTextMetric;
begin
  if NewStyleControls then begin
    if BorderStyle=bsNone then
      i:=0
    else
      if Ctl3D then
        i:=1
      else
        i:=2;
    Result.X:=SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF+i;
    Result.Y:=i;
  end
  else begin
    if BorderStyle = bsNone then
      i:=0
    else begin
      DC:=GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont:=SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      i:=SysMetrics.tmHeight;
      if i>Metrics.tmHeight then
        i:=Metrics.tmHeight;
      i:=i div 4;
    end;
    Result.X:=i;
    Result.Y:=i;
  end;
end;

procedure TEnhancedEdit.WMPaint(var Message: TWMPaint);
const AlignStyle : array[Boolean, TAlignment] of DWORD =
	 ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
          (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var Left       : Integer;
    Margins    : TPoint;
    R          : TRect;
    PS         : TPaintStruct;
    AAlignment : TAlignment;
    DC         : HDC;
    S          : string;
begin
  AAlignment := FAlignment;
  If FGrayDisabled then begin
    inherited;
    Exit;
  end;
  if FCanvas = nil then begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC:=Message.DC;
  if DC=0 then
    DC:=BeginPaint(Handle, PS);
  FCanvas.Handle:=DC;
  try
    FCanvas.Font:=Font;
    with FCanvas do begin
      R:=ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle=bsSingle) then begin
        Brush.Color:=clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color:=Color;
      if not Enabled then
        Font.Color:=clWindowText;
      S:=Text;
      if (csPaintCopy in ControlState) then begin
      case CharCase of
        ecUpperCase: S:=AnsiUpperCase(S);
        ecLowerCase: S:=AnsiLowerCase(S);
      end;
    end;
    if PasswordChar<>#0 then
      FillChar(S[1], Length(S), PasswordChar);
    Margins:=GetTextMargins;
    case AAlignment of
      taLeftJustify  : Left:=Margins.X;
      taRightJustify : Left:=ClientWidth-TextWidth(S)-Margins.X-1;
      else             Left:=(ClientWidth-TextWidth(S)) div 2;
    end;
    TextRect(R, Left, Margins.Y, S);
  end;
  finally
    FCanvas.Handle:=0;
    if Message.DC=0 then
      EndPaint(Handle, PS);
  end;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Simon', [TEnhancedEdit]);
end;

end.
