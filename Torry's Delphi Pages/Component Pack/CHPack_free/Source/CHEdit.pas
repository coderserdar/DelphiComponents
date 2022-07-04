unit CHEdit;

{ ##############################################################################
  TCHEdit

  Version       :   1.3.0
  Delphi        :   5, 6, 7
  Author        :   Christian Hämmerle
  eMail         :   chaemmerle@Blue-Xplosion.de
  Internet      :   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.1.0 - 14.11.2002		- NEW: MouseWheel, MouseWheelDown, MouseWheelUp
  1.1.1 - 15.12.2002    - BUG: repair some memory leaks
  1.2.0 - 20.08.2003    - NEW: ReturnAsTab change the focus to next control
                        - CHANGE: TextTyp
                        - DELETE: Showcursor (was useless)
  1.3.0 - 04.04.2004    - NEW: Class Edittyp, Class AutoComplete,
                          Class ClipboardAction, Class Selection
  1.4.0 - 31.12.2004    - BUG: Fix some Error in Class Edittyp
                        - NEW: ttTime in TextTyp; Errormessage
                        - NEW: DisableColor

  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, Types,
  ClipBrd, StrUtils, Dialogs, _CHClassProperty;


type
  TTextTyp = (ttString, ttInteger, ttFloat, ttDate, ttTime);
  TSelectMode = (smAll, smFirst, smLast, smCustom);
  TCaseMode = (cmNormal, cmUppercase, cmLowerCase, cmFirstUpper);
  TEditBeep = (ebStandard, ebNoEnterBeep, ebAlwaysBeep);
  TDateBasic = (ba1900, ba2000);
  
  TCHCustomEdit = class;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHAutoComplete = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FEnabled: Boolean;
    FCount: Integer;
    FItems: TStringList;
    procedure SetCount(const Value: Integer);
  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
    property Items: TStringList read FItems Write FItems;
  published
    property Enabled : Boolean read FEnabled Write FEnabled;
    property Count : Integer read FCount Write SetCount;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TClipboardNotifyEvent = procedure (Sender: TObject; Text: String; var Accept: Boolean) of object;
  TCHClipboard = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FClear: Boolean;
    FCopy: Boolean;
    FPaste: Boolean;
    FCut: Boolean;
  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property CanCut: Boolean read FCut write FCut;
    property CanCopy: Boolean read FCopy write FCopy;
    property CanPaste: Boolean read FPaste write FPaste;
    property CanClear: Boolean read FClear write FClear;

  end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHTypInt = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FMinInt: Integer;
    FMaxInt: Integer;
  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property MaxInt : Integer read FMaxInt Write FMaxInt;
    property MinInt : Integer read FMinInt Write FMinInt;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

  TCHTypFloat = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FMasking: Boolean;
    FMaxFloat: Double;
    FMinFloat: Double;
    FMask: string;
    procedure SetMasking(const Value: Boolean);
  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property MaxFloat : Double read FMaxFloat Write FMaxFloat;
    property MinFloat : Double read FMinFloat Write FMinFloat;
    property Mask : string read FMask Write FMask;
    property Masking : Boolean read FMasking Write SetMasking;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHTypString = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FNotAllowedChar: string;

  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property NotAllowedChars : string read FNotAllowedChar Write FNotAllowedChar;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHTypDate = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FDateBasic: TDateBasic;
    //FPastDate: Boolean;
    //FFutureDate: Boolean;
    FSeperator: Char;
    FSpaceIsDate: Boolean;

  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property DateBasic : TDateBasic read FDateBasic write FDateBasic;
    property Seperator : Char read FSeperator write FSeperator;
    //property DateCanFuture : Boolean read FFutureDate Write FFutureDate;
    //property DateCanPast : Boolean read FPastDate Write FPastDate;
    property SpaceIsDate : Boolean read FSpaceIsDate write FSpaceIsDate;
  end;

  { ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHTypTime = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FSeperator: Char;

  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property Seperator : Char read FSeperator write FSeperator;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHEditTyp = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FTypFloat: TCHTypFloat;
    FTypInt: TCHTypInt;
    FTypString: TCHTypString;
    FTextTyp: TTextTyp;
    FTypDate: TCHTypDate;
    FTypTime: TCHTypTime;
    FErrorText: string;
    procedure SetTextTyp(const Value: TTextTyp);

  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property Typ : TTextTyp read FTextTyp Write SetTextTyp;
    property TypInt: TCHTypInt read FTypInt write FTypInt;
    property TypFloat: TCHTypFloat read FTypFloat write FTypFloat;
    property TypString: TCHTypString read FTypString write FTypString;
    property TypDate: TCHTypDate read FTypDate write FTypDate;
    property TypTime: TCHTypTime read FTypTime write FTypTime;
    property ErrorText : string read FErrorText Write FErrorText;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHSelect = class(TPersistent)
  private
    FOwner : TCHCustomEdit;
    FSelectMode: TSelectMode;
    FSelectLength: Word;
    FSelectPos: Word;
    procedure SetSelectLength(const Value: Word);
    procedure SetSelectMode(const Value: TSelectMode);
    procedure SetSelectPos(const Value: Word);

  protected

  public
    constructor Create(AOwner: TCHCustomEdit); virtual;
  published
    property SelectMode : TSelectMode read FSelectMode Write SetSelectMode;
    property SelectLength : Word read FSelectLength Write SetSelectLength;
    property SelectPos : Word read FSelectPos Write SetSelectPos;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHCustomEdit = class(TCustomEdit)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnEnterPress: TKeyEvent;
    FOnESCPress: TKeyEvent;
    FOnAltPress: TKeyEvent;
    FOnStrgPress: TKeyEvent;
    FOnShiftPress: TKeyEvent;

    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FWheelAccumulator: Integer;

    FFocuscolor : TColor;
    FOrigColor : TColor;
    FAlignment : TAlignment;
    FUserKeyEvent : Boolean;
    FCaseMode : TCaseMode;
    FBeep : TEditBeep;
    FFocus: Boolean;
    FReturnAsTab: Boolean;
    FAutoComplete: TCHAutoComplete;
    FEditTyp: TCHEditTyp;
    FClipboard: TCHClipboard;
    FOnPaste: TClipboardNotifyEvent;
    FOnCopy: TClipboardNotifyEvent;
    FOnClear: TClipboardNotifyEvent;
    FOnCut: TClipboardNotifyEvent;
    FSelection: TCHSelect;
    FOnMinFloat: TNotifyEvent;
    FOnMinInt: TNotifyEvent;
    FOnMaxFloat: TNotifyEvent;
    FOnMaxInt: TNotifyEvent;
    FDisabledTextColor: TColor;

    procedure SetFocusColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaseMode(const Value: TCaseMode);
    function GetDouble(const Value : string) : Double;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message : TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure SetDisabledTextColor(const Value: TColor);

  protected
    procedure CreateParams(var params : TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure KeyPress(var Key : Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnKeyEnterPress : TKeyEvent read FOnEnterPress write FOnEnterPress;
    property OnKeyESCPress : TKeyEvent read FOnESCPress write FOnESCPress;
    property OnKeyAltPress : TKeyEvent read FOnAltPress write FOnAltPress;
    property OnKeyStrgPress : TKeyEvent read FOnStrgPress write FOnStrgPress;
    property OnKeyShiftPress : TKeyEvent read FOnShiftPress write FOnShiftPress;
    property OnCut: TClipboardNotifyEvent read FOnCut write FOnCut;
    property OnCopy: TClipboardNotifyEvent read FOnCopy write FOnCopy;
    property OnPaste: TClipboardNotifyEvent read FOnPaste write FOnPaste;
    property OnClear: TClipboardNotifyEvent read FOnClear write FOnClear;
    property OnMaxFloat : TNotifyEvent read FOnMaxFloat write FOnMaxFloat;
    property OnMinFloat : TNotifyEvent read FOnMinFloat write FOnMinFloat;
    property OnMaxInt : TNotifyEvent read FOnMaxInt write FOnMaxInt;
    property OnMinInt : TNotifyEvent read FOnMinInt write FOnMinInt;

    property Alignment : TAlignment read FAlignment Write SetAlignment;
    property AutoComplete : TCHAutoComplete read FAutoComplete Write FAutoComplete;
    property BeepOption : TEditBeep read FBeep Write FBeep;
    property CaseMode : TCaseMode read FCaseMode Write SetCaseMode;
    property Focus : Boolean read FFocus Write FFocus;
    property FocusColor : TColor read FFocuscolor Write SetFocusColor;
    property ReturnAsTab : Boolean read FReturnAsTab Write FReturnAsTab;
    property Selection : TCHSelect read FSelection Write FSelection;
    property EditTyp : TCHEditTyp read FEditTyp Write FEditTyp;
    property ClipboardAction : TCHClipboard read FClipboard Write FClipboard;
    property DisabledTextColor: TColor read FDisabledTextColor Write SetDisabledTextColor default clGrayText;

  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHEdit = class(TCHCustomEdit)
  protected
    property CharCase;
  published
    property Alignment;
    property AutoComplete;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOption;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CaseMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Focus;
    property Focuscolor;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property ImeMode;
    property ImeName;
    property Left;
    property MaxLength;
    property Name;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;

    property Top;
    property Visible;
    property Width;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyEnterPress;
    property OnKeyESCPress;
    property OnKeyAltPress;
    property OnKeyStrgPress;
    property OnKeyShiftPress;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnCut;
    property OnCopy;
    property OnPaste;
    property OnClear;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHEdit]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoComplete := TCHAutoComplete.Create(Self);
  FEditTyp := TCHEditTyp.Create(Self);
  FClipboard := TCHClipboard.Create(Self);
  FSelection := TCHSelect.Create(Self);

  FFocusColor := clInfoBk;
  FFocus := False;
  FBeep := ebStandard;
  FAlignment := taLeftJustify;
  FDisabledTextColor := clGrayText;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomEdit.Destroy;
begin
  FAutoComplete.Free;
  FClipboard.Free;
  FSelection.Free;
  FEditTyp.Free;

  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CreateParams(var params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT,ES_RIGHT,ES_CENTER);
begin
  inherited CreateParams(Params);
    Params.Style := Params.Style or Alignments[FAlignment];
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FOrigColor := Color;
  if FFocus then
  begin
    Color := FFocuscolor;
  end;

  if FSelection.FSelectMode = smAll then
    SelectAll
  else if FSelection.FSelectMode = smFirst then
  begin
    SelStart := 0;
    SelLength := 0;
  end
  else if FSelection.FSelectMode = smLast then
  begin
    SelStart := Length(Text);
    SelLength := 0;
  end
  else if FSelection.FSelectMode = smCustom then
  begin
    SelStart := FSelection.FSelectPos;
    SelLength := FSelection.FSelectLength;
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CMExit(var Message: TCMExit);
var
  nPos1, nPos2, nInt, nY : Integer;
  nDouble : Double;
  sFormatDate : string;
  dCheck : TDate;
begin
  Color := FOrigColor;

  if FEditTyp.FTextTyp = ttInteger then
  begin
    nInt := StrToInt(Text);
    // check min integer
    if FEditTyp.FTypInt.FMinInt <> 0 then
      if nInt < FEditTyp.FTypInt.FMinInt then
      begin
        if Assigned(FOnMinInt) then
          FOnMinInt(self);
      end;
    // check max integer
    if FEditTyp.FTypInt.FMaxInt <> 0 then
      if nInt > FEditTyp.FTypInt.FMaxInt then
      begin
        if Assigned(FOnMaxInt) then
          FOnMaxInt(self);
      end;
  end
  else if FEditTyp.FTextTyp = ttFloat then
  begin
    nDouble := GetDouble(Text);
    // check min float
    if FEditTyp.FTypFloat.FMinFloat <> 0 then
      if nDouble < FEditTyp.FTypFloat.FMinFloat then
      begin
        if Assigned(FOnMinFloat) then
          FOnMinFloat(self);
      end;
    // check max float
    if FEditTyp.FTypFloat.FMaxFloat <> 0 then
      if nDouble > FEditTyp.FTypFloat.FMaxFloat then
      begin
        if Assigned(FOnMaxFloat) then
          FOnMaxFloat(self);
      end;

    // masking
    if FEditTyp.FTypFloat.FMasking then
      Text := FormatFloat(FEditTyp.FTypFloat.Mask, nDouble)
    else
      Text := FloatToStr(nDouble);
  end
  // format date
  else if FEditTyp.FTextTyp = ttDate then
  begin
    // sobald Datumsfeld ein Zeichen enthält, kann es nur verlassen werden
    // wenn ein gültiges Datum eingegeben wurde
    if Text <> '' then
    begin
      // auf min. Date Eingabe -> 1.1.01 prüfen
      if Length(Text) >= 5 then
      begin
        // auf max. Date Eingabe -> 01.01.2003 prüfen
        if Length(Text) <= 10 then
        begin
          sFormatDate := Text;
          // Tag prüfen
          nPos1 := PosEx('.', Text, 1);

          // ohne Punkte
          if (nPos1 = 0) then
          begin
            if (Length(sFormatDate) = 6) then
            begin
              sFormatDate := sFormatDate[1]+sFormatDate[2]+'.'+sFormatDate[3]+sFormatDate[4];
              nY := strtoint(Copy(Text, 5,2));
              if FEditTyp.FTypDate.FDateBasic = ba2000 then
              begin
                if nY < 10 then
                  sFormatDate := sFormatDate+'.200'+IntToStr(nY)
                else
                  sFormatDate := sFormatDate+'.20'+IntToStr(nY);
              end
              else
              begin
                if nY < 10 then
                  sFormatDate := sFormatDate+'.199'+IntToStr(nY)
                else
                  sFormatDate := sFormatDate+'.19'+IntToStr(nY)
              end;
            end
            else if (Length(sFormatDate) = 8) then
              sFormatDate := sFormatDate[1]+sFormatDate[2]+'.'+sFormatDate[3]+sFormatDate[4]+'.'+
                sFormatDate[5]+sFormatDate[6]+sFormatDate[7]+sFormatDate[8];
          end
          else
          begin
            //nY := strtoint(Copy(Text, Length(Text)-1,2));
            if (nPos1 = 2) then
              sFormatDate := '0' + Text;

            // Monat prüfen
            nPos2 := PosEx('.', sFormatDate, 4);
            if (nPos2 = 5) or (nPos2 = 6) then
            begin
              if nPos2 = 5 then
                sFormatDate := Copy(sFormatDate, 1, 3) + '0' + Copy(sFormatDate, 4, 7);
            end;

            // Jahr prüfen
            if FEditTyp.FTypDate.FDateBasic = ba2000 then
            begin
              if Length(sFormatDate) = 7 then
                sFormatDate := Copy(sFormatDate, 1, 6) + '200' + Copy(sFormatDate, 7, 2)
              else if Length(sFormatDate) = 8 then
                sFormatDate := Copy(sFormatDate, 1, 6) + '20' + Copy(sFormatDate, 7, 2);
            end
            else
            begin
              if Length(sFormatDate) = 7 then
                sFormatDate := Copy(sFormatDate, 1, 6) + '199' + Copy(sFormatDate, 7, 2)
              else if Length(sFormatDate) = 8 then
                sFormatDate := Copy(sFormatDate, 1, 6) + '19' + Copy(sFormatDate, 7, 2);
            end;
          end;

          Text := sFormatDate;

          try
            dCheck := StrToDate(Text);
            if dCheck = 0 then
              Clear;
          except
            ShowMessage('Invalid date!');
            SelectAll;
            SetFocus;
          end;

        end
        else
        begin
          SelectAll;
          SetFocus;
        end;
      end
      else
      begin
        SelectAll;
        SetFocus;
      end;
    end;
  end;

  if FAutoComplete.FEnabled then
  begin
    if FAutoComplete.FItems.Count >= FAutoComplete.FCount then
      FAutoComplete.FItems.Delete(0);
    FAutoComplete.FItems.Add(Text);
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.SetFocusColor(const Value: TColor);
begin
  if FFocuscolor <> Value then
  begin
    FFocuscolor := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.SetCaseMode(const Value: TCaseMode);
begin
  if FCaseMode <> Value then
  begin
    FCaseMode := Value;

    if FCaseMode = cmNormal then
    begin
      Self.CharCase := ecNormal;
    end
    else if FCaseMode = cmUppercase then
    begin
      Self.CharCase := ecUpperCase;
    end
    else if FCaseMode = cmLowercase then
    begin
      Self.CharCase := ecLowerCase;
    end;
    Invalidate;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomEdit.GetDouble(const Value: string): Double;
var
  nChar : Integer;
  sDouble : string;
begin
  Result := 0;
  sDouble := '';
  for nChar := 1 to Length(Value) do
  begin
    case Value[nChar] of
      '0'..'9' : sDouble := sDouble + Value[nChar];
      '+','-'  : if nChar = 1 then
                   sDouble := sDouble + Value[nChar];
    end;
    if Value[nChar] = DecimalSeparator then
      sDouble := sDouble + Value[nChar];
  end;

  if sDouble <> '' then
    Result := StrToFloat(sDouble);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.KeyPress(var Key: Char);
begin
  // handle ENTER Key
  if Key = #13 then
  begin
    if FReturnAsTab then
    begin
      Key := #0;
      PostMessage(self.Handle,WM_KEYDOWN,VK_TAB,0);
    end;

    if FBeep = ebNoEnterBeep then
      Key := #0
    else if FBeep = ebAlwaysBeep then
      Beep;

    if FEditTyp.FTextTyp = ttFloat then
      if Key = #46 then
        Key := #44;

    if PasswordChar = #0 then
      Invalidate;
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  s1 : string;
  I : Integer;
begin
  inherited;
  // AutoComplete
  if not (Char(Key) in [#8, #9, #13, #20, #27, #16..#18, #37..#40]) then
  begin
    if FAutoComplete.FEnabled then
    begin
      for I := 0 to FAutoComplete.FItems.Count -1 do
      begin
        s1 := Copy(FAutoComplete.FItems[I], 1, Length(Text));
        if s1 = Text then
        begin
          Text := FAutoComplete.FItems[I];
          SelStart := Length(s1);
          SelLength := Length(FAutoComplete.FItems[I]) - SelStart;
          Break;
        end;
      end;
    end;
  end;

  // String
  //if FTextTyp = ttString then
//  begin
//    if FEditTyp.FTypString.FNotAllowedChar <> '' then
//    begin
//      sText := Text;
//      for I := 1 to Length(FEditTyp.FTypString.FNotAllowedChar) do
//      begin
//        if Key = FEditTyp.FTypString.FNotAllowedChar[I] then
//          Delete(sText, Length(Text)-1, 1);
//      end;
//      Text := sText;
//    end;
//  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Mgs: TMsg;
  IntSet, FloatSet, DateSet, TimeSet, NotAllowSet: set of char;
  I : Integer;
  nCaretPos : Word;
  sFirst, sText : String;


  procedure ClearKey;
  begin
    PeekMessage(Mgs, 0, WM_CHAR, WM_CHAR, PM_REMOVE);
    if Trim(FEditTyp.FErrorText) <> '' then
      MessageDlg(FEditTyp.FErrorText, mtError, [mbOK], 0);
  end; 

begin
  IntSet := ['0','1','2','3','4','5','6','7','8','9','-'];
  FloatSet := ['0','1','2','3','4','5','6','7','8','9',',','.','-'];
  DateSet := ['0','1','2','3','4','5','6','7','8','9',FEditTyp.FTypDate.FSeperator];
  TimeSet := ['0','1','2','3','4','5','6','7','8','9',FEditTyp.FTypTime.FSeperator];
  NotAllowSet := [];
  for I := 1 to Length(FEditTyp.FTypString.FNotAllowedChar) do
  begin
    NotAllowSet := NotAllowSet + [FEditTyp.FTypString.FNotAllowedChar[I]];
  end;


  if Key <> VK_BACK then
  begin
    // Strings
    if FEditTyp.FTextTyp = ttString then
    begin
      if (ssCtrl in Shift) then
      begin
        case key of
          { Ctrl + V }
          86: begin
                if Clipboard.HasFormat(CF_TEXT) then
                begin
                  for I := 1 to Length(Clipboard.AsText) do
                  begin
                    if (Clipboard.AsText[I] in NotAllowSet) then
                      ClearKey;
                  end;
                end;
              end;
        end;
      end
      else
        if (Char(Key) in NotAllowSet) then
          ClearKey;

      // Case
      if FCaseMode = cmFirstUpper then
      begin
        if Text <> '' then
        begin
          nCaretPos := Self.SelStart;
          sText := Text;
          sFirst := AnsiUpperCase(Text[1]);
          Delete(sText, 1, 1);
          Text := sFirst + sText;
          Self.SelStart := nCaretPos;
        end;
      end;
    end
    // Integer
    else if FEditTyp.FTextTyp = ttInteger then
    begin
      if (ssCtrl in Shift) then
      begin
        case key of
          { Ctrl + V }
          86: begin
                if Clipboard.HasFormat(CF_TEXT) then
                begin
                  for I := 1 to Length(Clipboard.AsText) do
                  begin
                    if not (Clipboard.AsText[I] in IntSet) then
                      ClearKey;
                  end;
                end;
              end;
        end;
      end
      else
        if not (Char(Key) in IntSet) then
          ClearKey;
    end
    // Float
    else if FEditTyp.FTextTyp = ttFloat then
    begin
      if (ssCtrl in Shift) then
      begin
        case key of
          { Ctrl + V }
          86: begin
                if Clipboard.HasFormat(CF_TEXT) then
                begin
                  for I := 1 to Length(Clipboard.AsText) do
                  begin
                    if not (Clipboard.AsText[I] in FloatSet) then
                      ClearKey;
                  end;
                end;
              end;
        end;
      end
      else
        if not (Char(Key) in FloatSet) then
          ClearKey;
    end
    // Date
    else if FEditTyp.FTextTyp = ttDate then
    begin
      if (ssCtrl in Shift) then
      begin
        case key of
          { Ctrl + V }
          86: begin
                if Clipboard.HasFormat(CF_TEXT) then
                begin
                  for I := 1 to Length(Clipboard.AsText) do
                  begin
                    if not (Clipboard.AsText[I] in DateSet) then
                      ClearKey;
                  end;
                end;
              end;
        end;
      end
      else
      begin
        if FEditTyp.FTypDate.FSpaceIsDate then
        begin
          ClearKey;
          Text := DateToStr(Date);
        end
        else if not (Char(Key) in DateSet) then
          ClearKey;
      end;
    end
    // Time
    else if FEditTyp.FTextTyp = ttTime then
    begin
      if (ssCtrl in Shift) then
      begin
        case key of
          { Ctrl + V }
          86: begin
                if Clipboard.HasFormat(CF_TEXT) then
                begin
                  for I := 1 to Length(Clipboard.AsText) do
                  begin
                    if not (Clipboard.AsText[I] in TimeSet) then
                      ClearKey;
                  end;
                end;
              end;
        end;
      end
      else
        if not (Char(Key) in TimeSet) then
          ClearKey;
    end;
  end;

  // Alt Key
  FUserKeyEvent := False;
  if (Key = VK_MENU	) then
  begin
    FUserKeyEvent := True;
    if Assigned(FOnAltPress) then
      FOnAltPress(Self, Key, Shift)
    else
      Key := vk_Clear;
    Exit;
  end;

  // Enter Key
  if (Key = vk_Return) then
  begin
    FUserKeyEvent := True;
    if Assigned(FOnEnterPress) then
      FOnEnterPress(Self, Key, Shift)
    else
      Key := vk_Clear;
    Exit;
  end;

  // ESC Key
  if (Key = vk_Escape) then
  begin
    FUserKeyEvent := True;
    if Assigned(FOnESCPress) then
      FOnESCPress(Self, Key, Shift)
    else
      Key := vk_Clear;
    Exit;
  end;

  // Strg Key
  if (Key = VK_CONTROL) then
  begin
    FUserKeyEvent := True;
    if Assigned(FOnStrgPress) then
      FOnStrgPress(Self, Key, Shift)
    else
      Key := vk_Clear;
    Exit;
  end;

  // Shift Key
  if (Key = VK_SHIFT) then
  begin
    FUserKeyEvent := True;
    if Assigned(FOnShiftPress) then
      FOnShiftPress(Self, Key, Shift)
    else
      Key := vk_Clear;
    Exit;
  end;


  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomEdit.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomEdit.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;


{ TCHAutoComplete }

constructor TCHAutoComplete.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FItems := TStringList.Create;
  FItems.Sorted := True;
  FCount := 15;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAutoComplete.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.CreateWnd;
begin
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TCanvas;
  PS: TPaintStruct;
  callEndPaint: Boolean;
  FontHeight, H, HF : Integer;
  Rec: TRect;
  //Txt: array[0..1000] of Char;
begin
  if Enabled then
    inherited
  else
  begin
    callEndPaint := False;
    Canvas := TCanvas.Create;
    try
      if Msg.DC <> 0 then
      begin
        Canvas.Handle := Msg.DC;
        PS.fErase := True;
      end
      else
      begin
        BeginPaint(Handle, PS);
        callEndPaint := True;
        Canvas.Handle := PS.HDC;
      end;

      if (PS.fErase) then
        Perform(WM_ERASEBKGND, Canvas.Handle, 0);

      SaveDC(Canvas.Handle);
      try
        FontHeight := Canvas.TextHeight(Text);
        Canvas.Brush.Style := bsClear;
        Canvas.Font := Font;
        Canvas.Font.Color := FDisabledTextColor;
        Canvas.Brush.Color := Color;
        Canvas.FillRect(Bounds(0,0,Width,Height));

        if FontHeight > 0 then
        begin
          Rec.Left := +1;
          Rec.Top := -2;
          Rec.Bottom := ClientHeight;
          Rec.Right := ClientWidth;

          H := Rec.Bottom - Rec.Top;
          HF := (H DIV FontHeight) * FontHeight;
          Rec.Top := (Rec.Top + (H - HF) DIV 2);
          Rec.Bottom := (Rec.Top + HF);
          //StrPCopy(Txt, Text);

          case Alignment of
            taLeftJustify : DrawText(Canvas.Handle, PAnsiChar(Text), -1, Rec, DT_Left+DT_NOPREFIX	);
            taRightJustify: DrawText(Canvas.Handle, PAnsiChar(Text), -1, Rec, DT_Right+DT_NOPREFIX	);
            taCenter      : DrawText(Canvas.Handle, PAnsiChar(Text), -1, Rec, DT_Center+DT_NOPREFIX	);
          end;
        end;

      finally
        RestoreDC(Canvas.Handle, -1);
      end;
    finally
      if callEndPaint then
        EndPaint(Handle, PS);
      Canvas.Free;
    end;
  end;
end;


{ TCHTypString }

constructor TCHTypString.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TCHTypFloat }

constructor TCHTypFloat.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FMask := '#,##0.00';
  FMasking := True;
  FMinFloat := 0;
  FMaxFloat := 0;
end;

procedure TCHTypFloat.SetMasking(const Value: Boolean);
begin
  FMasking := Value;
end;


{ TCHTypInt }

constructor TCHTypInt.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FMinInt := 0;
  FMaxInt := 0;
end;

{ TCHEditTyp }

constructor TCHEditTyp.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FTypInt := TCHTypInt.Create(FOwner);
  FTypFloat := TCHTypFloat.Create(FOwner);
  FTypString := TCHTypString.Create(FOwner);
  FTypDate := TCHTypDate.Create(FOwner);
  FTypTime := TCHTypTime.Create(FOwner);
end;

procedure TCHEditTyp.SetTextTyp(const Value: TTextTyp);
begin
  FTextTyp := Value;
end;


{ TCHClipboard }

constructor TCHClipboard.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FClear := True;
  FCopy := True;
  FPaste := True;
  FCut := True;
end;

procedure TCHCustomEdit.WMClear(var Message: TMessage);
var
  Accept: Boolean;
  CText: String;
begin
  if not FClipboard.FClear then
    Exit;
  if SelStart = 0 then
    Exit;
  CText := Copy(Text, SelStart + 1, SelLength);
  Accept := True;
  if Assigned(FOnClear) then
    FOnClear(Self, CText, Accept);
  if not Accept then
    Exit;
  CText := Text;
  Delete(CText, SelStart + 1, SelLength);
  Text := CText;
end;

procedure TCHCustomEdit.WMCopy(var Message: TMessage);
var
  Accept: Boolean;
  Handle: THandle;
  HandlePtr: Pointer;
  CText: String;
begin
  if not FClipboard.FCopy then
    Exit;
  if SelLength = 0 then
    Exit;
  CText := Copy(Text, SelStart + 1, SelLength);
  try
    OpenClipBoard(Self.Handle);
    Accept := True;
    if Assigned(FOnCopy) then
      FOnCopy(Self, CText, Accept);
    if not Accept then
      Exit;
    Handle := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Length(CText) + 1);
    if Handle = 0 then
      Exit;
    HandlePtr := GlobalLock(Handle);
    Move((PChar(CText))^, HandlePtr^, Length(CText));
    SetClipboardData(CF_TEXT, Handle);
    GlobalUnlock(Handle);
  finally
    CloseClipBoard;
  end;
end;

procedure TCHCustomEdit.WMCut(var Message: TMessage);
var
  Accept: Boolean;
  Handle: THandle;
  HandlePtr: Pointer;
  CText: String;
begin
  if not FClipboard.FCut then
    Exit;
  if SelLength = 0 then
    Exit;
  CText := Copy(Text, SelStart + 1, SelLength);
  try
    OpenClipBoard(Self.Handle);
    Accept := True;
    if Assigned(FOnCut) then
      FOnCut(Self, CText, Accept);
    if not Accept then
      Exit;
    Handle := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Length( CText ) + 1);
    if Handle = 0 then
      Exit;
    HandlePtr := GlobalLock(Handle);
    Move((PChar(CText))^, HandlePtr^, Length(CText));
    SetClipboardData(CF_TEXT, Handle);
    GlobalUnlock(Handle);
    CText := Text;
    Delete(CText, SelStart + 1, SelLength);
    Text := CText;
  finally
    CloseClipBoard;
  end;
end;

procedure TCHCustomEdit.WMPaste(var Message: TMessage);
var
  Accept: Boolean;
  Handle: THandle;
  CText: String;
  LText: String;
  AText: String;
begin
  if not FClipboard.FPaste then
    Exit;
  if IsClipboardFormatAvailable(CF_TEXT) then
  begin
    try
      OpenClipBoard(Self.Handle);
      Handle := GetClipboardData(CF_TEXT);
      if Handle = 0 then
        Exit;
      CText := StrPas(GlobalLock(Handle));
      GlobalUnlock(Handle);
      Accept := True;
      if Assigned(FOnPaste) then
        FOnPaste(Self, CText, Accept);
      if not Accept then
        Exit;
      LText := '';
      if SelStart > 0 then
        LText := Copy(Text, 1, SelStart);
      LText := LText + CText;
      AText := '';
      if (SelStart + 1) < Length(Text) then
         AText := Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart + SelLength + 1);
      Text := LText + AText;
    finally
      CloseClipBoard;
    end;
  end;
end;

{ TCHSelect }

constructor TCHSelect.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TCHSelect.SetSelectLength(const Value: Word);
begin
  if FSelectLength <> Value then
  begin
    FSelectLength := Value;
    FOwner.Invalidate;
  end;
end;

procedure TCHSelect.SetSelectMode(const Value: TSelectMode);
begin
  if FSelectMode <> Value then
  begin
    FSelectMode := Value;
    FOwner.Invalidate;
  end;
end;

procedure TCHSelect.SetSelectPos(const Value: Word);
begin
  if FSelectPos <> Value then
  begin
    FSelectPos := Value;
    FOwner.Invalidate;
  end;
end;

{ TCHTypDate }

constructor TCHTypDate.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FSeperator := '.';
  FSpaceIsDate := True;
  FDateBasic := ba2000;
end;

{ TCHTypTime }

constructor TCHTypTime.Create(AOwner: TCHCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;

  FSeperator := ':';
end;



end.
