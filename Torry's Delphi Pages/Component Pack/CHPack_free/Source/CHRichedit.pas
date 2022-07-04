unit CHRichedit;

{ ##############################################################################
  TCHRichedit

  Version   		:   1.1.2
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.1.0 - 18.10.2002    - ADD: Linespacing and SelectColor
  1.1.1 - 15.12.2002    - BUG: repair memory leak
  1.1.2 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }


interface

uses
  Windows, Messages, Classes, Controls, Graphics, ComCtrls, Richedit, CommCtrl,
  StdCtrls, ShellAPI, ActiveX, ComObj, Registry, _CHClassProperty;

type
  TCHRichEdit = class;


  TCHRichedit = class(TRichEdit)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnScrollH: TScrollEvent;
    FOnScrollV: TScrollEvent;

    FMargin : TCHMargin;
    FMarginLeft : TControlCanvas;
    FMarginRight : TControlCanvas;
    FMarginTop : TControlCanvas;
    FMarginBottom : TControlCanvas;
    FMaxLines: Integer;
    FIndent: Boolean;
    FClickSelectLine: Boolean;
    FURLDetect: Boolean;
    FUndoLevel: Word;


    procedure SetMargin(Left, Right, Top, Bottom : Word);
    procedure UpdateChanges(Sender: TObject);
    procedure SetMaxLines(const Value: Integer);
    procedure SetIndent(const Value: Boolean);
    procedure SetURLDetect(const Value: Boolean);
    procedure SetUndoLevel(const Value: Word);
  protected
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMPaint(var Message : TWMPaint); message WM_PAINT;
    procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;

    procedure KeyPress(var Key: Char); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    //procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DeleteLine(LineNr : Integer);
    function VisibleLineCount : Integer;
    procedure ScrollToLastLine;
    procedure ScrollToFirstLine;
    procedure ScrollLineUp;
    procedure ScrollLineDown;
    procedure ScrollPageUp;
    procedure ScrollPageDown;
    procedure SearchAndReplace(Find, Replace : string);
    procedure SearchAndSelect(Find : string);
    procedure OpenURL(Url : string);
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnScrollH: TScrollEvent read FOnScrollH write FOnScrollH;
    property OnScrollV: TScrollEvent read FOnScrollV write FOnScrollV;

    property Margin : TCHMargin read FMargin Write FMargin;
    property MaxLines : Integer read FMaxLines Write SetMaxLines;
    property Indent : Boolean read FIndent Write SetIndent;
    property ClickSelectLine : Boolean read FClickSelectLine Write FClickSelectLine;
    property URLDetect : Boolean read FURLDetect Write SetURLDetect;
    property UndoLevel : Word read FUndoLevel Write SetUndoLevel;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('CH Pack', [TCHRichedit]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.CMMouseEnter(var Message: TMessage);
begin
    if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHRichedit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FWnd := AllocateHWnd(WndProc);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FMargin := TCHMargin.Create;
  FMargin.OnChange := UpdateChanges;

  FMarginLeft := TControlCanvas.Create;
  FMarginLeft.Control := Self;
  FMarginRight := TControlCanvas.Create;
  FMarginRight.Control := Self;
  FMarginTop := TControlCanvas.Create;
  FMarginTop.Control := Self;
  FMarginBottom := TControlCanvas.Create;
  FMarginBottom.Control := Self;
  FUndoLevel := 50;
  FURLDetect := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.CreateWnd;
var
  Mask: Longint;
begin
  inherited CreateWnd;
  Mask := ENM_CHANGE or ENM_SELCHANGE or ENM_REQUESTRESIZE or ENM_PROTECTED or ENM_LINK;
  SendMessage(Handle, EM_SETEVENTMASK, 0, Mask);
  SendMessage(Handle, EM_EXLIMITTEXT, 0, $FFFFFF);
  SendMessage(Handle, EM_AUTOURLDETECT, Longint(FURLDetect), 0);
  FUndoLevel := SendMessage(Handle, EM_SETUNDOLIMIT, FUndoLevel, 0);

  SetMargin(FMargin.MLeft, FMargin.MRight, FMargin.MTop, FMargin.MBottom);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHRichedit.Destroy;
begin
   FMarginLeft.Free;
   FMarginRight.Free;
   FMarginTop.Free;
   FMarginBottom.Free;
   FMargin.Free;
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.WMPaint(var Message: TWMPaint);
var
  LeftRect, TopRect, RightRect, BottomRect : TRect;
  LineCount : Integer;
begin
  inherited;
  // Left
  if FMargin.MLeft > 0 then
  begin
    LeftRect := Rect(-1, -1, FMargin.MLeft, Self.Height);
    with FMarginLeft do
    begin
      Brush.Color := FMargin.Color;
      FrameRect(LeftRect);
      InflateRect(LeftRect, -1, -1);
      FrameRect(LeftRect);
      FillRect(LeftRect);
    end;
  end;

  // Top
  if FMargin.MTop > 0 then
  begin
    TopRect := Rect(-1, -1, Self.Width, FMargin.MTop);
    with FMarginTop do
    begin
      Brush.Color := FMargin.Color;
      FrameRect(TopRect);
      InflateRect(TopRect, -1, -1);
      FrameRect(TopRect);
      FillRect(TopRect);
    end;
  end;

  // Right
  if FMargin.MRight > 0 then
  begin
    RightRect := Rect(Self.Width - FMargin.MRight - 4, -1, Self.Width, Self.Height);
    with FMarginRight do
    begin
      Brush.Color := FMargin.Color;
      FrameRect(RightRect);
      InflateRect(RightRect, -1, -1);
      FrameRect(RightRect);
      FillRect(RightRect);
    end;
  end;

  // Bottom
  if FMargin.MBottom > 0 then
  begin
    BottomRect := Rect(-1, Self.Height - FMargin.MBottom - 4, Self.Width, Self.Height);
    with FMarginBottom do
    begin
      Brush.Color := FMargin.Color;
      FrameRect(BottomRect);
      InflateRect(BottomRect, -1, -1);
      FrameRect(BottomRect);
      FillRect(BottomRect);
    end;
  end;

  // Line Margin
  if FMargin.LineWidth > 0 then
  begin
    for LineCount := 0 to FMargin.LineWidth  - 1 do
    begin
      if FMargin.MLeft > 0 then
      begin
        FMarginLeft.Pen.Color := FMargin.LineColor;
        FMarginLeft.MoveTo(LeftRect.Right - LineCount, LeftRect.Top);
        FMarginLeft.LineTo(LeftRect.Right - LineCount, LeftRect.Bottom);
      end;

      if FMargin.MTop > 0 then
      begin
        FMarginTop.Pen.Color := FMargin.LineColor;
        FMarginTop.MoveTo(TopRect.Left, TopRect.Bottom - LineCount);
        FMarginTop.LineTo(TopRect.Right, TopRect.Bottom - LineCount);
      end;

      if FMargin.MRight > 0 then
      begin
        FMarginRight.Pen.Color := FMargin.LineColor;
        FMarginRight.MoveTo(RightRect.Left - LineCount, RightRect.Top);
        FMarginRight.LineTo(RightRect.Left - LineCount, RightRect.Bottom);
      end;

      if FMargin.MBottom > 0 then
      begin
        FMarginBottom.Pen.Color := FMargin.LineColor;
        FMarginBottom.MoveTo(BottomRect.Left, BottomRect.Top - LineCount);
        FMarginBottom.LineTo(BottomRect.Right, BottomRect.Top - LineCount);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.SetMargin(Left, Right, Top, Bottom: Word);
var
  TextRect : TRect;
begin
  // Pos
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@TextRect));
  TextRect.Left := Left + 1;
  TextRect.Right := TextRect.Right - Right;
  TextRect.Top := Top;
  TextRect.Bottom := TextRect.Bottom - Bottom;
  SendMessage(Handle, EM_SETRECT, 0, LongInt(@TextRect));
  Refresh;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  RecreateWnd;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRichedit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  SetMargin(FMargin.MLeft, FMargin.MRight, FMargin.MTop, FMargin.MBottom);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

procedure TCHRichedit.DeleteLine(LineNr: Integer);
begin
  Selstart := Perform(EM_LineIndex,LineNr,0);
  SelLength := Length(Lines[LineNr]) +2;
  SelText := '';
end;


procedure TCHRichedit.SetMaxLines(const Value: Integer);
begin
  if FMaxLines <> Value then
  begin
    FMaxLines := Value;
  end;
end;

procedure TCHRichedit.KeyPress(var Key: Char);
var
  Row, Col, Indent: integer;
  S: string;
begin
  // MaxLines
  if FMaxLines > 0 then
  begin
    if (Lines.Count = FMaxLines) and (Key = #13) then
      Key := #0;
  end;

  // Indent
  if (FIndent) and (Key = #13) then
  begin
    Key := #0;
    Row := PerForm( EM_EXLINEFROMCHAR, 0, SelStart );
    Col := SelStart - Perform( EM_LINEINDEX, Row, 0 );
    S := Copy( lines[ Row ], 1, col );
    indent := 0;
    while (indent < length( S )) and (S[indent + 1] in  [' ', #9]) do
      Inc( indent );
    SelText := #13#10 + Copy(S, 1, indent);
  end;

  inherited;
end;

procedure TCHRichedit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  // MaxLines (support WordWrap)
  if FMaxLines > 0 then
  begin
    if Lines.Count > FMaxLines then
    begin
      Keybd_Event(VK_BACK, 0, 0, 0);
      Keybd_Event(VK_BACK, 0, KEYEVENTF_KEYUP, 0);
    end;
  end;
end;

procedure TCHRichedit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Line : Integer;
begin
  inherited;
  if FClickSelectLine then
  begin
    Line := Perform(EM_LINEFROMCHAR,SelStart,0);
    SelStart := Perform(EM_LINEINDEX, Line, 0);
    SelLength := Length(Lines[Line]);
  end;
end;

procedure TCHRichedit.SetIndent(const Value: Boolean);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
  end;
end;

function TCHRichedit.VisibleLineCount: Integer;
var
  OldFont: HFont;
  DC: THandle;
  TM: TTextMetric;
  Rect: TRect;
  nCount : integer;
begin
  DC := GetDC(Handle);
  try
    OldFont := SelectObject(DC, Font.Handle);
    try
      GetTextMetrics(DC, TM);
      Perform(EM_GETRECT, 0, longint(@Rect));
      nCount := (Rect.Bottom - Rect.Top) div (TM.tmHeight + TM.tmExternalLeading);
    finally
      SelectObject(DC, OldFont);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
  Result := nCount;
end;

procedure TCHRichedit.ScrollToFirstLine;
begin
  Selstart := Perform(EM_LineIndex,0,0);
  PostMessage(Handle,WM_VSCROLL,SB_TOP,0);
  SetFocus;
end;

procedure TCHRichedit.ScrollToLastLine;
begin
  SelStart := Length(Text);
  PostMessage(Handle,WM_VSCROLL,SB_BOTTOM,0);
  SetFocus;
end;

procedure TCHRichedit.ScrollLineDown;
begin
  Selstart := Perform(EM_LineIndex,CaretPos.Y +1,0);
  PostMessage(Handle,WM_VSCROLL,SB_LINEDOWN,0);
  SetFocus;
end;

procedure TCHRichedit.ScrollLineUp;
begin
  Selstart := Perform(EM_LineIndex,CaretPos.Y -1,0);
  PostMessage(Handle,WM_VSCROLL,SB_LINEUP,0);
  SetFocus;
end;

procedure TCHRichedit.ScrollPageDown;
begin
  Selstart := Perform(EM_LineIndex,CaretPos.Y + VisibleLineCount,0);
  PostMessage(Handle,WM_VSCROLL,SB_PAGEDOWN,0);
  SetFocus;
end;

procedure TCHRichedit.ScrollPageUp;
begin
  Selstart := Perform(EM_LineIndex,CaretPos.Y - VisibleLineCount,0);
  PostMessage(Handle,WM_VSCROLL,SB_PAGEUP,0);
  SetFocus;
end;

procedure TCHRichedit.SearchAndReplace(Find, Replace: string);
var  
  startpos, position, endpos: integer;  
begin  
  startpos := 0;

  endpos := Length(Text);
  Lines.BeginUpdate;
  while FindText(Find, startpos, endpos, [stMatchCase]) <> -1 do
  begin
    endpos  := Length(Text) - startpos;
    position := FindText(Find, startpos, endpos, [stMatchCase]);
    Inc(startpos, Length(Find));
    SetFocus;
    SelStart  := position;
    SelLength := Length(Find);
    clearselection;
    SelText := Replace;
  end;
  Lines.EndUpdate;
end;

procedure TCHRichedit.SearchAndSelect(Find: string);
var 
  startpos, position, endpos: integer; 
begin 
  startpos := 0;
  endpos := Length(Text);
  Lines.BeginUpdate;
  while FindText(Find, startpos, endpos, [stMatchCase]) <> -1 do
  begin
    endpos   := Length(Text) - startpos;
    position := FindText(Find, startpos, endpos, [stMatchCase]);
    Inc(startpos, Length(Find));
    SetFocus;
    SelStart  := position;
    SelLength := Length(Find);
  end;
  Lines.EndUpdate;
end;




procedure TCHRichedit.WMHScroll(var Message: TWMScroll);
VAR
  Pos: Integer;
begin
  inherited;
  {TScrollEvent}
  Pos := Message.Pos;
  IF Pos = 0 THEN
    Pos := GetScrollPos(Handle, SB_HORZ);
  IF assigned(FOnScrollH) THEN
    FOnScrollH(Self, TScrollCode(Message.ScrollCode), Pos);
end;

procedure TCHRichedit.WMVScroll(var Message: TWMScroll);
VAR
  Pos: Integer;
begin
  inherited;
  {TScrollEvent}
  Pos := Message.Pos;
  IF Pos = 0 THEN
    Pos := GetScrollPos(Handle, SB_VERT);
  IF assigned(FOnScrollV) THEN
    FOnScrollV(Self, TScrollCode(Message.ScrollCode), Pos);
end;


procedure TCHRichedit.SetURLDetect(const Value: Boolean);
begin
  FURLDetect := Value;
end;

procedure TCHRichedit.SetUndoLevel(const Value: Word);
begin
  FUndoLevel := Value;
  SendMessage(Handle, EM_SETUNDOLIMIT, FUndoLevel, 0);
end;





procedure TCHRichedit.CNNotify(var Msg: TWMNotify);
type
  PENLink = ^TENLink;

  TTextRange = record
    chrg: TCharRange;
    lpstrText: PAnsiChar;
  end;

var
  TextRange: TTextRange;
  sUrl : string;
begin
  with Msg do
    case NMHdr^.code of
      EN_LINK:
        with PENLink(NMHdr)^ do
        begin
          case Msg of
            WM_RBUTTONUP:
              begin
                SetLength(sUrl, chrg.cpMax - chrg.cpMin + 1);
                TextRange.chrg.cpMin := chrg.cpMin;
                TextRange.chrg.cpMax := chrg.cpMax;
                TextRange.lpstrText := PAnsiChar(sUrl);
                SetLength(sUrl, SendMessage(Handle, EM_GETTEXTRANGE, 0, Longint(@TextRange)));
                OpenUrl(sUrl);
              end;
          end;
        end;
    end;
end;

procedure TCHRichedit.OpenURL(Url : string);
var 
  ts :string; 
begin 
  with TRegistry.Create do try 
    rootkey:= HKEY_CLASSES_ROOT; 
    OpenKey('\htmlfile\shell\open\command',false); 
    try 
      ts:= ReadString(''); 
    except 
      ts:= ''; 
    end; 
    CloseKey; 
  finally 
    Free; 
  end; 
  if ts = '' then
    Exit;

  ts:= Copy(ts,Pos('"',ts)+1,Length(ts));
  ts:= Copy(ts,1,Pos('"',ts)-1);
  ShellExecute(0,'open',pchar(ts),pchar(url),nil,sw_show); 
end;



end.
