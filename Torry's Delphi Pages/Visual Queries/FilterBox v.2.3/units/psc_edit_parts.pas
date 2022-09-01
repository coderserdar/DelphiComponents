{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_edit_parts;

interface
{$I psc_defines.inc}
{$IFDEF D2009}
  {$M+}
{$ENDIF}

Uses
  winapi.Windows,
  winapi.messages,
  forms,
  controls,
  classes,

  myla_system,
  myla_interfaces,

  psc_theme,
  psc_wrapper,
  psc_edit,
  psc_procs,
  psc_const;

type
  IPSCTextPart = Interface(IPSCInterface)
    ['{BE3D5580-1B22-49CF-ACDC-F81046796F63}']
    Function GetValue: Integer;
    Function GetMinValue: Integer;
    Function GetMaxValue: Integer;
    Function GetDisplayStr(AValue : integer): String;
    Function GetEditStr(AValue : integer): String;
    Function CanEdit: Boolean;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;

    Procedure SetValue(AValue: Integer);

    property Value:Integer Read GetValue Write SetValue;
  End;

  TPSCTextPart = Class(TPSCNamedItem)
  private
    FExpandWidth: boolean;
    FPartData: IPSCTextPart;
    FAlignment: TPSCHorzAlign;
    FCheckEnabled: boolean;
    FCheckVisible : boolean;
    FChecked: boolean;
    FHandler: IPSCEventHandler;
    Procedure SetCheckEnabled(AValue: boolean);
    Procedure SetCheckVisible(AValue : boolean);
    Procedure SetChecked(AValue: boolean);
    Procedure SetExpandWidth(AValue: boolean);
    Procedure SetPartData(Const AValue: IPSCTextPart);
    Procedure SetAlignment(AValue: TPSCHorzAlign);
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);override;
  public
    Destructor Destroy; override;
    constructor Create(Collection: TCollection); override;

    Property PartData: IPSCTextPart read FPartData write SetPartData;
  published
    Property Alignment: TPSCHorzAlign read FAlignment write SetAlignment;
    Property ExpandWidth: boolean read FExpandWidth write SetExpandWidth;
    Property CheckEnabled: boolean read FCheckEnabled write SetCheckEnabled;
    Property CheckVisible : boolean read FCheckVisible write SetCheckVisible;
    Property Checked: boolean read FChecked write SetChecked;
  End;

  TPSCTextPartClass = Class Of TPSCTextPart;

  TPSCTextParts = Class(TPSCNamedItems)
  private
    Function GetPart(Index: integer): TPSCTextPart;
    Procedure SetPart(Index: integer; PartEdit: TPSCTextPart);
  protected
  public
    Function Add: TPSCTextPart;
    Property Items[Index: Integer]: TPSCTextPart read GetPart write SetPart;
  End;

  TPSCTextPartsClass = Class Of TPSCTextParts;

  TPSCCustomPartsEdit = Class(TPSCCustomPopupEdit)
  private
    FValue : integer;
    FFlatCheckBoxes: boolean;
    FShowCheckBoxes: boolean;
    FTextParts: TPSCTextParts;
    FEditingStarted: boolean;
    FSelectedPart: integer;
    FOnChange: TPSCNotifyEvent;
    Function GetFirstEditPart : integer;
    Function GetLastEditPart : integer;    
    Function GetCheckedPartCount: integer;
    Function GetTextPartClass: TPSCTextPartClass;
    Function GetTextPartsClass: TPSCTextPartsClass;
    Function GetHorzTextSlip(ASelectedPart: integer): integer;
    Function GetCaption(APlainText: boolean): String;
    Function GetSelectedPart(X: integer; Var AStartItemX: integer): integer;
    Function FormatTextParts(AItemNumber: integer;APlainText: boolean): String;

    Procedure SetSelectedPart(AValue: integer);
    Procedure PaintCheck;
    Procedure SetFlatCheckBoxes(AValue: boolean);
    Procedure SetShowCheckBoxes(AValue: boolean);
    procedure SetTextParts(const Value: TPSCTextParts);
    Procedure ChangeTextPart(ASelected,ADelta,ANumber: integer);
    Procedure ChangeTextPartValue(Var ATextBit: integer; ADelta,AMaxValue,
      AMinValue,ANumber: integer; ASelected: integer);
    Procedure UpdateCaption;
  protected
    Procedure TextPartsChanged(Sender: TObject; AItem: TPSCNamedItem);virtual;
    Function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
      override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure DoButtonClick(BtnIndex: Integer); override;
    Procedure DoExit; override;
    Procedure WMGetDlgCode(Var Msg: TWMGetDlgCode); message WM_GetDlgCode;
    
  public
    Function GetMaxTextWidth(AWithBorder:Boolean): integer; virtual;
    Function GetTextSize: TSize;

    Procedure Paint; override;
    Procedure SetFocus; override;
    Procedure IncSelectedPart;
    Procedure DecSelectedPart;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property TextParts: TPSCTextParts read FTextParts write SetTextParts;
    Property EditingStarted: boolean read FEditingStarted write FEditingStarted;
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
    Property FlatCheckBoxes: boolean read FFlatCheckBoxes write SetFlatCheckBoxes
      default true;
    Property ShowCheckBoxes: boolean read FShowCheckBoxes write SetShowCheckBoxes
      default false;
    Property SelectedPart: integer read FSelectedPart write SetSelectedPart Stored False;
    Property TabStop Default True;
  End;

  TPSCAbstractTextPart = class(TPSCEvents)
  private
  protected
    procedure Changed;virtual;
  end;

  IPSCTextPartInteger=interface(IPSCTextPart)
    ['{C5EB1FEF-A514-4B8B-B8EB-BF301F58A7AA}']
    procedure SetLeadingZero(V:Boolean);
  end;

  TPSCTextPartInteger = Class(TPSCAbstractTextPart,IPSCTextPart,IPSCTextPartInteger)
  private
    FMinValue: integer;
    FMaxValue: integer;
    FValue: integer;
    FLeadingZero: Boolean;
  protected
    Procedure SetLeadingZero(AValue: Boolean);
    Function CanEdit: Boolean;
    Function GetDisplayStr(AValue : integer): String;virtual;
    Function GetEditStr(AValue : integer): String;virtual;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;virtual;
    Function GetMinValue: Integer;virtual;
    Function GetMaxValue: Integer;virtual;
    Function GetValue: integer;virtual;

    Procedure SetMinValue(AValue: Integer);virtual;
    Procedure SetMaxValue(AValue: Integer);virtual;
    Procedure SetValue(AValue: Integer);virtual;
  public
  published
    Property Value: Integer read GetValue write SetValue;
    Property LeadingZero: Boolean read FLeadingZero write SetLeadingZero;
    Property MinValue: Integer read GetMinValue write SetMinValue;
    Property MaxValue: Integer read GetMaxValue write SetMaxValue;
  End;

  TPSCTextPartDelimiter = Class(TPSCAbstractTextPart,IPSCTextPart)
  private
    FStrValue: String;
    Function GetMinValue: Integer;
    Function GetMaxValue: Integer;
    Function GetValue: integer;
    Function GetDisplayStr(AValue : integer): String;
    Function GetEditStr(AValue : integer): String;
    Function CanEdit: Boolean;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;

    Procedure SetValue(AValue: Integer);
    procedure SetStrValue(const V:String);
  public
    constructor Create(const AValue:String);
  published
    Property StrValue: String read FStrValue write SetStrValue;
  End;

  TPSCTextPartPickList = Class(TPSCAbstractTextPart,IPSCTextPart)
  private
    FValue: integer;
    FPickList: IPSCStrings;
    Function GetMinValue: Integer;
    Function GetMaxValue: Integer;
    Function GetValue: integer;
    Function GetDisplayStr(AValue : integer): String;
    Function GetEditStr(AValue : integer): String;
    Function CanEdit: Boolean;
    Function GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
    Procedure SetValue(AValue: Integer);
  public
    Constructor Create;
    Destructor Destroy; override;
    Property PickList: IPSCStrings read FPickList;
    Property Value: Integer read GetValue write SetValue;
  published
  End;


implementation

{-------------------------------------}

Procedure TPSCTextPart.SetChecked(AValue: boolean);
Begin
  If FChecked <> AValue Then
    Begin
      FChecked := AValue;
      Changed(false);
    End;
End;

{-------------------------------------}

Procedure TPSCTextPart.SetCheckEnabled(AValue: boolean);
Begin
  If FCheckEnabled <> AValue Then
    Begin
      FCheckEnabled := AValue;
      Changed(false);
    End;
End;

{-------------------------------------}

Function TPSCTextParts.Add: TPSCTextPart;
Begin
  result := TPSCTextPart(Inherited Add);
  result.CheckVisible := true;
End;

{-------------------------------------}

Function TPSCTextParts.GetPart(Index: integer): TPSCTextPart;
Begin
  Result := TPSCTextPart(Inherited Items[Index]);
End;

{-------------------------------------}

Procedure TPSCTextParts.SetPart(Index: integer; PartEdit: TPSCTextPart);
Begin
  Items[Index].Assign(PartEdit);
End;

{-------------------------------------}

Procedure TPSCTextPartInteger.SetLeadingZero(AValue: Boolean);
Begin
  If FLeadingZero <> AValue Then
  begin
    FLeadingZero := AValue;
    Changed;
  end;
End;

{-------------------------------------}

Function TPSCTextPartInteger.CanEdit: Boolean;
Begin
  result := true;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.CanEdit: Boolean;
Begin
  result := false;
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetDisplayStr(AValue : integer): String;
Var
  MaxLength: integer;
  i: integer;
Begin
  result := PSCIntToStr(AValue);
  MaxLength := Length(PSCIntToStr(GetMaxValue)) - Length(result);
  If (MaxLength > 0) And LeadingZero Then
    For i := 1 To MaxLength Do
      result := '0' + result;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetDisplayStr(AValue : integer): String;
Begin
  result := FStrValue;
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetValue: integer;
Begin
  result := FValue;
End;

{-------------------------------------}

Procedure TPSCTextPartInteger.SetValue(AValue: Integer);
Begin
  If FValue <> AValue Then
    Begin
      FValue := AValue;
      Changed;
    End;
End;

{-------------------------------------}

Procedure TPSCTextPart.SetAlignment(AValue: TPSCHorzAlign);
Begin
  If FAlignment <> AValue Then
    Begin
      FAlignment := AValue;
      Changed(false);
    End;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetEditStr(AValue : integer): String;
Begin
  result := FStrValue;
End;

{-------------------------------------}

Procedure TPSCTextPart.SetExpandWidth(AValue: boolean);
Begin
  If FExpandWidth <> AValue Then
    Begin
      FExpandWidth := AValue;
      Changed(false);
    End;
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetMaxValue: Integer;
Begin
  result := FMinValue;
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetMinValue: Integer;
Begin
  result := FMaxValue;
End;

{-------------------------------------}

Procedure TPSCTextPartInteger.SetMaxValue(AValue: Integer);
Begin
  If FMinValue <> AValue Then
  begin
    FMinValue := AValue;
    Changed;
  end;
End;

{-------------------------------------}

Procedure TPSCTextPartInteger.SetMinValue(AValue: Integer);
Begin
  If FMaxValue <> AValue Then
  begin
    FMaxValue := AValue;
    Changed;
  end;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetMaxValue: Integer;
Begin
  result := 0;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
Begin
  result := ACanvas.TextWidth(FStrValue);
End;

{-------------------------------------}

constructor TPSCTextPartDelimiter.Create(const AValue:String);
begin
  inherited Create;
  FStrValue:=AValue;
end;

{-------------------------------------}

procedure TPSCTextPartDelimiter.SetStrValue(const V:String);
begin
  If FStrValue<>V then
  begin
    FStrValue:=V;
    Changed;
  end;
end;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetMinValue: Integer;
Begin
  result := 0;
End;

{-------------------------------------}

Function TPSCTextPartDelimiter.GetValue: integer;
Begin
  result := 0;
End;

{-------------------------------------}

Procedure TPSCTextPartDelimiter.SetValue(AValue: Integer);
Begin
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetEditStr(AValue : integer): String;
Begin
  result := PSCIntToStr(AValue);
End;

{-------------------------------------}

Function TPSCTextPartInteger.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
Begin
  If MaxValue <> 0 Then
    result := ACanvas.TextWidth(PSCIntToStr(MaxValue))
  Else
    result := ACanvas.TextWidth(PSCIntToStr(Value));
End;

{-------------------------------------}

Procedure TPSCTextPart.SetPartData(Const AValue: IPSCTextPart);
Var
  MyEvents: IPSCEvents;
Begin
  If FPartData <> AValue Then
    Begin
      If FPartData <> Nil Then
        If PSCSupports(FPartData,IPSCEvents,MyEvents) Then
          MyEvents.UnregisterHandler(FHandler);
      FPartData := AValue;
      If FPartData <> Nil Then
        Begin
          If PSCSupports(FPartData,IPSCEvents,MyEvents) Then
            MyEvents.RegisterHandler(FHandler);
        End;
      Changed(False);
    End;
End;

{-------------------------------------}

constructor TPSCTextPart.Create(Collection: TCollection);
begin
  inherited;
  FHandler:=PSCCreateEventHandler(HandleEvent);
end;

{-------------------------------------}

Destructor TPSCTextPart.Destroy;
Begin
  PartData := Nil;
  Inherited;
End;

{-------------------------------------}

Function TPSCTextPartPickList.CanEdit: Boolean;
Begin
  result := true;
End;

{-------------------------------------}

Constructor TPSCTextPartPickList.Create;
begin
  inherited;
  FPickList:=PSCCreateStringList;
end;

{-------------------------------------}

Destructor TPSCTextPartPickList.Destroy;
Begin
  Inherited;
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetDisplayStr(AValue : integer): String;
Begin
  result := PickList[AValue - 1];
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetEditStr(AValue : integer): String;
Begin
  result := PickList[AValue - 1];
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetMaxValue: Integer;
Begin
  result := FPickList.Count;
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetMaxWidth(const ACanvas:IPSCCanvas): Integer;
var
  i:Integer;
Begin
  result := 0;
  For i := 0 To FPickList.Count - 1 Do
    Result:=PSCMax(Result,ACanvas.TextWidth(FPickList[i]));
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetMinValue: Integer;
Begin
  result := 1;
End;

{-------------------------------------}

Function TPSCTextPartPickList.GetValue: integer;
Begin
  result := FValue;
End;

{-------------------------------------}

Procedure TPSCTextPartPickList.SetValue(AValue: Integer);
Begin
  If FValue <> AValue Then
  Begin
    FValue := AValue;
    Changed;
  End;
End;

{-------------------------------------}

procedure TPSCAbstractTextPart.Changed;
var
  MyParams:TPSCEventParams;
begin
  inherited;
  MyParams.ASender:=Self;
  MyParams.AEventType:=EVENT_AFTER_CHANGE;
  HandleEvent(MyParams);
end;

{-------------------------------------}

Procedure TPSCTextPart.HandleEvent(const AParams:TPSCEventParams);
Begin
  Inherited;
  Changed(False);
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.ChangeTextPart(ASelected, ADelta,
  ANumber: integer);
Var
  MyBit: integer;
Begin
  If ASelected = -1 Then
    exit;
  With TextParts.Items[ASelected],PartData Do
    Begin
      If Not CanEdit Then
        exit;
      MyBit := FValue;
      ChangeTextPartValue(MyBit,ADelta,GetMaxValue,GetMinValue,ANumber,ASelected);
      FValue := MyBit;
      If (GetMaxValue <> 0) Then
        If Not PSCApplyLimits(GetMinValue,GetMaxValue,MyBit) Then
          SetValue(MyBit);
    End;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.ChangeTextPartValue(var ATextBit: integer;
  ADelta, AMaxValue, AMinValue, ANumber, ASelected: integer);
Begin
  ATextBit := ATextBit + ADelta;
  If FEditingStarted Then
    exit;
  If (AMaxValue <> 0) Then
    If PSCApplyLimits(AMinValue,AMaxValue,ATextBit) Then
      FEditingStarted := false;
End;

{-------------------------------------}

Constructor TPSCCustomPartsEdit.Create(AOwner: TComponent);
Begin
  Inherited;
  FShowCheckBoxes := false;
  FFlatCheckBoxes := true;
  FTextParts := GetTextPartsClass.Create(Self,GetTextPartClass);
  FTextParts.OnUpdate := TextPartsChanged;
  BtnKind := bkUpDown;
  ButtonsVisible := true;
  FSelectedPart := -1;
  TabStop := True;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.DecSelectedPart;
Begin
  FEditingStarted := false;
  ChangeTextPart(SelectedPart, -1, 0);
End;

{-------------------------------------}

Destructor TPSCCustomPartsEdit.Destroy;
Begin
  Inherited;
  FTextParts.Free;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.DoButtonClick(BtnIndex: Integer);
Begin
  Inherited;
  If (Not ButtonsVisible) Or (TextParts.Count = 0) Or (BtnKind<>bkUpDown) Then
    exit;
  If SelectedPart = -1 Then
    SelectedPart := 0;
  FEditingStarted := false;
  If BtnKind = bkUpDown Then
    If BtnIndex = 0 Then
      IncSelectedPart
    Else
      DecSelectedPart;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.DoExit;
Begin
  Inherited;
  UpdateCaption;
  EditingStarted := false;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
Begin
  Inherited DoMouseWheel(Shift,WheelDelta,MousePos);
  If SelectedPart = -1 Then
    SelectedPart := 0;
  FEditingStarted := false;
  If WheelDelta > 0 Then
    ChangeTextPart(SelectedPart,1,0)
  Else
    ChangeTextPart(SelectedPart, -1,0);
  Result:=True;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.FormatTextParts(AItemNumber: integer;
  APlainText: boolean): String;
Var
  VisibleColor: String;
  MyRect: TRect;
  MyOuterRect,MyInnerRect: TRect;
  Delta: integer;
  MyTextColor,MyColor : TPSCColor;
  MySelColor,MySelTextColor : TPSCColor;

  Function _FormatPart: String;
  Var
    S: String;
    Delta,Delta1: integer;
    MyValue : integer;
  Begin
    If not HandleAllocated Then
      exit;
    With TextParts.Items[AItemNumber],PartData,Canvas Do
      Begin
        if EditingStarted and (SelectedPart = AItemNumber) then
        Begin
          MyValue := FValue;
          S := GetEditStr(MyValue);
        End
        else
        Begin
          MyValue := GetValue;
          S := GetDisplayStr(MyValue);
        End;
        MyOuterRect := Rect(0,0,GetMaxWidth(MylaCanvas),Height);
        MyInnerRect := Rect(0,0,TextWidth(S),Height);
        MyRect :=
          PSCAlignRectInRect(MyInnerRect,MyOuterRect,Alignment,vaCenter);
        Delta := 0;
        Delta1 := 0;
        If ExpandWidth And (Not APlainText) Then
          Begin
            Delta := MyRect.Left - MyOuterRect.Left;
            Delta1 := MyOuterRect.Right - MyRect.Right;
          End;
        Begin
          If (Delta > 0) Then
            S := '<skip:' + PSCIntToStr(Delta) + '>' + S;
          If (Delta1 > 0) Then
            S := S + '<skip:' + PSCIntToStr(Delta1) + '>';
        End;
        S := VisibleColor + S;
        result := S;
      End;
  End;

Begin
  MyTextColor := GetDefaultColor(GetEditState,TMT_TEXTCOLOR);
  MyColor := GetDefaultColor(GetEditState,TMT_FILLCOLOR);
  MySelColor := clPSCHighlight;
  MySelTextColor := clPSCHighlightText;
  VisibleColor := '';
  If Not APlainText Then
    Begin
      If (AItemNumber = SelectedPart) And Focused Then
        Begin
          VisibleColor := PSCColorToHTString(MySelTextColor);
          If Enabled then
            VisibleColor := VisibleColor + PSCBkColorToHTString(MySelColor);
        End
      Else
        Begin
          VisibleColor := PSCColorToHTString(MyTextColor);
          If Enabled then
            VisibleColor := VisibleColor + PSCBkColorToHTString(MyColor);
        End;
    End;
  result := _FormatPart;
  If APlainText Or (Not ShowCheckBoxes) Then
    exit;
  If FlatCheckBoxes Then
    Delta := 2
  Else
    Delta := 3;
  With TextParts.Items[AItemNumber] , PartData Do
    If CanEdit And CheckVisible Then
      If HandleAllocated Then
        result := '<skip:' + PSCIntToStr(PSCCheckWidth + Delta) + '>' + result
      Else
        result := '<skip:' + PSCIntToStr(13 + Delta) + '>' + result;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetCaption(APlainText:Boolean): String;
Var
  i: integer;
Begin
  Result := '';
  For i := 0 To TextParts.Count - 1 Do
    With TextParts.Items[i],PartData Do
      Result := Result + FormatTextParts(i,APlainText);
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetCheckedPartCount: integer;
Var
  i: integer;
Begin
  result := 0;
  For i := 0 To TextParts.Count - 1 Do
    With TextParts.Items[i],PartData Do
      If CanEdit Then
        result := result + 1;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetFirstEditPart: integer;
Var
  i : integer;
Begin
  Result := -1;
  For i := 0 To TextParts.Count - 1 Do
    With TextParts.Items[i].PartData Do
      If CanEdit Then
      Begin
        Result := i;
        Exit;
      End;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetHorzTextSlip(
  ASelectedPart: integer): integer;
Var
  i: integer;
  Delta: integer;
  MyPaintTextWidth: integer;
  MyRect: TRect;
  MyEditWidth: integer;
Begin
  If FlatCheckBoxes Then
    Delta := 2
  Else
    Delta := 3;
  If ShowCheckBoxes Then
    Delta := PSCCheckWidth + Delta
  else
    Delta := 0;
  MyPaintTextWidth := 0;
  For i := 0 To ASelectedPart Do
    With TextParts.Items[i],PartData,Canvas Do
      If CanEdit Then
        MyPaintTextWidth := MyPaintTextWidth + GetMaxWidth(MylaCanvas) + Delta
      Else
        MyPaintTextWidth := MyPaintTextWidth + GetMaxWidth(MylaCanvas);
  MyRect := GetInplaceEditRect;
  With MyRect Do
    MyEditWidth := Right - Left;
  If FlatCheckBoxes Then
    result := MyEditWidth - MyPaintTextWidth
  Else
    result := MyEditWidth - MyPaintTextWidth - 1;
  If result > 0 Then
    result := 0;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetLastEditPart: integer;
Var
  i : integer;
Begin
  Result := -1;
  For i := TextParts.Count - 1 DownTo 0 Do
    With TextParts.Items[i].PartData Do
      If CanEdit Then
      Begin
        Result := i;
        Exit;
      End;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetMaxTextWidth(AWithBorder:Boolean): integer;
Var
  i: integer;
  Delta: integer;
  R:TRect;
Begin
  result := 0;

  If not HandleAllocated Then
    exit;

  Canvas.Font:=Font;

  For i := 0 To TextParts.Count - 1 Do
    With TextParts.Items[i],PartData,Canvas Do
      result := result + GetMaxWidth(MylaCanvas);

  If ShowCheckBoxes Then
  begin
    If FlatCheckBoxes Then
      Delta := 2
    Else
      Delta := 3;
    result := result + (PSCCheckWidth + Delta) * GetCheckedPartCount;
  end;  
  If AWithBorder then
  begin
    R:=GetInplaceEditRect;
    Result:=Result+R.Left+(Width-ClientWidth)+(ClientWidth-R.Right+3);
  end;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetSelectedPart(X: integer;
  Var AStartItemX: integer): integer;
Var
  i,j: integer;
  Size: TSize;
  AX,OldAX: integer;
  S: String;
  Delta : integer;
  
  Procedure _SetSize(AItemNumber: integer);
  Var
    Delta: integer;
    PartWidth: integer;
  Begin
    If FlatCheckBoxes Then
      Delta := 2
    Else
      Delta := 3;
    If ShowCheckBoxes Then
      Delta := PSCCheckWidth + Delta
    Else
      Delta := 0;
    With TextParts.Items[AItemNumber], PartData ,Canvas Do
      Begin
        Canvas.Font := Self.Font;
        If ExpandWidth Then
          PartWidth := GetMaxWidth(MylaCanvas)
        Else
          PartWidth := TextWidth(GetDisplayStr(GetValue));
        If CanEdit And CheckVisible Then
          Size.cx := PartWidth + Delta
        Else
          Size.cx := PartWidth;
      End;
  End;

Begin
  If FlatCheckBoxes Then
    Delta := 0
  Else
    Delta := 1;  
  S := GetCaption(true);
  AX := GetHorzTextSlip(SelectedPart) + GetInplaceEditRect.Left;
  result := -1;
  For i := 0 To TextParts.Count - 1 Do
    Begin
      _SetSize(i);
      OldAX := AX;
      AX := AX + Size.cx;
      If AX >= X - 2 Then
        Begin
          j := i;
          While Not TextParts.Items[j].PartData.CanEdit Do
            Begin
              j := j + 1;
              If j > TextParts.Count - 1 Then
                j := 0;
              If j = i Then
                Begin
                  j := -1;
                  break;
                End;
            End;
          result := j;
          AStartItemX := OldAX - (GetHorzTextSlip(SelectedPart) + GetInplaceEditRect.Left);
          If ((OldAX + PSCCheckWidth + Delta) >= X) And ShowCheckBoxes Then
            With TextParts.Items[j] Do
              If CheckEnabled then
                Checked := Not Checked;
          exit;
        End;
    End;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetTextPartClass: TPSCTextPartClass;
Begin
  Result := TPSCTextPart;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetTextPartsClass: TPSCTextPartsClass;
Begin
  Result := TPSCTextParts;
End;

{-------------------------------------}

Function TPSCCustomPartsEdit.GetTextSize: TSize;
Var
  S: String;
Begin
  With result Do
    Begin
      cx := 0;
      cy := 0;
    End;
  S := GetCaption(true);
  If HandleAllocated Then
    With Canvas Do
      Begin
        Font := Self.Font;
        result := TextExtent(S);
      End;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.IncSelectedPart;
Begin
  FEditingStarted := false;
  ChangeTextPart(SelectedPart, 1, 0);
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.KeyDown(Var Key: Word; Shift: TShiftState);
Const
  MyDelta: Array[VK_LEFT..VK_DOWN] Of integer = (-1,1,1, -1);
  MyBigDelta: Array[48..57] Of integer = (0,1,2,3,4,5,6,7,8,9);
Var
  MyOldSelected: integer;
  MyKeyProcessed: boolean;
  MyChecked: boolean;

  Procedure _LeftRightKey;
  Begin
    FEditingStarted := false;
    MyOldSelected := SelectedPart;
    SelectedPart := SelectedPart + MyDelta[Key];
    While Not TextParts.Items[SelectedPart].PartData.CanEdit Do
      Begin
        SelectedPart := SelectedPart + MyDelta[Key];
        If SelectedPart = MyOldSelected Then
          Begin
            SelectedPart := -1;
            break;
          End;
      End;
  End;

  Procedure _NumbersKey;
  Begin
    If SelectedPart < 0 Then
      SelectedPart := 0;
    With TextParts.Items[SelectedPart],PartData Do
      Begin
        If (Not FEditingStarted) Then
          Begin
            If (Key = Ord('0')) And (MyBigDelta[Key] < GetMinValue) then
              exit;
            FEditingStarted := true;
            ChangeTextPart(SelectedPart,MyBigDelta[Key] -
              FValue,MyBigDelta[Key]);
          End
        Else
          Begin
            With TextParts.Items[SelectedPart],PartData Do
              Begin
                If CanEdit And (GetMaxValue <> 0) Then
                  If Length(PSCIntToStr(FValue)) >= (Length(PSCIntToStr(GetMaxValue))
                    - 1) Then
                    FEditingStarted := false;
                If Length(PSCIntToStr(FValue)) >= (Length(PSCIntToStr(MaxInt)) - 1)
                  Then
                  FEditingStarted := false;
              End;
            ChangeTextPart(SelectedPart,MyBigDelta[Key] + 9 *
              FValue,MyBigDelta[Key]);
          End;
      End;
  End;

  Procedure _BackKey;
  Begin
    FEditingStarted := true;  
    With TextParts.Items[SelectedPart],PartData Do
      Begin
        If Length(PSCIntToStr(FValue)) <= 1 Then
          FEditingStarted := false;
        ChangeTextPart(SelectedPart,(FValue Div 10) - FValue,0);
      End;
  End;

  Procedure _SetTextPartsChecked;
  var
    i : integer;
  Begin
    If (ssCtrl In Shift) Then
      Begin
        MyChecked := Not TextParts.Items[SelectedPart].Checked;
        TextParts.BeginUpdate;
        For i := 0 To TextParts.Count - 1 Do
          With TextParts.Items[i] Do
            If CheckEnabled then
              Checked := MyChecked;
        TextParts.EndUpdate;
      End
    Else
      Begin
        If SelectedPart < 0 Then
          SelectedPart := 0;
        With TextParts.Items[SelectedPart] Do
          If CheckEnabled then
            Checked := Not Checked;
      End;
  End;

  Procedure _EndKey;
  Begin
    If (ssCtrl In Shift) Then
      SelectedPart := GetLastEditPart
    Else
      With TextParts.Items[SelectedPart],PartData Do
        Begin
          If GetMaxValue <> 0 Then
            ChangeTextPart(SelectedPart,GetMaxValue - FValue,0);
        End;
  End;

  Procedure _HomeKey;
  Begin
    If (ssCtrl In Shift) Then
      SelectedPart := GetFirstEditPart
    Else
      With TextParts.Items[SelectedPart],PartData Do
        ChangeTextPart(SelectedPart,GetMinValue - FValue,0);
  End;

Begin
  Inherited;
  MyKeyProcessed := true;
  If Key In [VK_NUMPAD0..VK_NUMPAD9] Then
    Key := Key - 48;
  Case Key Of
    VK_UP,VK_ADD: IncSelectedPart;
    VK_DOWN,VK_SUBTRACT: DecSelectedPart;
    VK_LEFT,VK_RIGHT: _LeftRightKey;
    VK_BACK: _BackKey;
    Ord('0')..Ord('9'): _NumbersKey;
    VK_END: _EndKey;
    VK_HOME: _HomeKey;
    VK_SPACE: _SetTextPartsChecked;
  Else
    MyKeyProcessed := false;
  End;
  If MyKeyProcessed Then
    Key := 0;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  MyStartItemX: integer;
Begin
  Inherited;
  If (ssRight In Shift) Then
    exit;
  FEditingStarted := false;
  If X < GetInplaceEditRect.Right Then
    SelectedPart := GetSelectedPart(X,MyStartItemX);
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.Paint;
Var
  S,SS: String;
  AWidth: integer;
  MyRect: TRect;
  SlipX,SlipY: integer;
  Size: TSize;
  Rgn: TPSCRegion;
Begin
  Inherited;
  Canvas.Font := Self.Font;
  Canvas.Font.Color:=GetDefaultColor(GetEditState,TMT_TEXTCOLOR);
  Caption := GetCaption(false);
  With Canvas Do
    Begin
      S := GetCaption(true);
      Size := TextExtent(S);
      If ShowCheckBoxes And (Not FlatCheckBoxes) Then
        SlipX := GetHorzTextSlip(SelectedPart) + 1
      Else
        SlipX := GetHorzTextSlip(SelectedPart);
      MyRect := GetInplaceEditRect;

      Brush.Color:=GetDefaultColor(GetEditState,TMT_FILLCOLOR);
      FillRect(MyRect);

      SlipY := ((MyRect.Bottom - MyRect.Top) - Size.cy) Div 2;
      With MyRect Do
        Begin
          Left := Left + SlipX;
          Top := Top + SlipY;
          Right := Left+Size.cx;
          Bottom := Top + Size.cy;
        End;
      Rgn := PSCGetClipRgn(Self,Canvas);
      Try
        With GetInplaceEditRect do
          IntersectClipRect(Canvas.Handle,Left,Top,Right,Bottom);

        Canvas.Brush.Style:=BrushStyle_Clear;// otherwise will be bug in painting when disabled state 
        PSCItemHtDrawEx(Canvas,MyRect,Caption,SS,AWidth,false);
      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn)
      End;
    End;
  PaintCheck;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.PaintCheck;
Var
  S: String;
  Delta: integer;
  SizeX:Integer;
  SizeCheck: TSize;
  SlipX,SlipY: integer;
  MyRect: TRect;
  MyCheckedRect: TRect;
  i: integer;
  MyState: TPSCCheckBoxState;
  Rgn: TPSCRegion;

  Procedure DrawCheck(ATextPart: TPSCTextPart);
  var
    Delta1 : integer;
  Begin
    With ATextPart, PartData Do
      Begin
        if Sizex > 0 then
          Delta1 := 2
        else
          Delta1 := 0;
        If CanEdit Then
          Begin
            If Checked Then
              MyState := CheckBox_Checked
            Else
              MyState := CheckBox_Unchecked;
            With MyRect Do
              Begin
                MyCheckedRect := Rect(Left,MyCheckedRect.Top,Right +
                  1,MyCheckedRect.Bottom);
                If (Sizex > 0) And (MyRect.Left > 0) And CheckVisible Then
                  PSCDrawCheck(Canvas,MyRect,MyState,FlatCheckBoxes,
                    GetDefaultColor(GetEditState,TMT_FILLCOLOR),
                    CheckEnabled And Enabled);
                If CheckVisible Then
                begin
                  Left := Left + SizeCheck.cx + Delta1;
                  Right := Right + SizeCheck.cx + Delta1;
                end;
              End;
          End;
        With MyRect Do
          Begin
            Left := Left + Sizex + Delta;
            Right := Right + Sizex + Delta;
          End;
      End;
  End;

Begin
  If Not ShowCheckBoxes Then
    exit;
  SizeCheck.cx := PSCCheckWidth;
  SizeCheck.cy := PSCCheckHeight;
  Canvas.Font := Font;
  S := GetCaption(true);
  If FlatCheckBoxes Then
    Delta := 0
  Else
    Delta := 1;
  With GetInplaceEditRect, Canvas Do
    Begin
      SlipX := GetHorzTextSlip(SelectedPart);
      SlipY := ((Bottom + Top) - SizeCheck.cy) Div 2 + 1;
      MyCheckedRect := GetInplaceEditRect;
      MyRect := Rect(Left + Delta + SlipX, SlipY, Left + SizeCheck.cx + Delta +
        SlipX, SizeCheck.cy + SlipY - 2);
      Rgn := PSCGetClipRgn(Self,Canvas);
      Try
        IntersectClipRect(Canvas.Handle,Left,Top,Right,Bottom);
        For i := 0 To TextParts.Count - 1 Do
          With TextParts.Items[i],PartData,Canvas Do
            Begin
              If ExpandWidth Then
                SizeX := GetMaxWidth(MylaCanvas)
              Else
                If EditingStarted And (i = SelectedPart) Then
                  SizeX := TextWidth(GetEditStr(GetValue))
                Else
                  SizeX := TextWidth(GetDisplayStr(GetValue));
              If MyRect.Right <= GetInplaceEditRect.Right Then
                DrawCheck(TextParts.Items[i]);
            End;
      Finally
        PSCSelectClipRgn(Canvas,Rgn);
        PSCDeleteClipRgn(Rgn);
      End;
    End;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.SetFlatCheckBoxes(AValue: boolean);
Begin
  If FFlatCheckBoxes <> AValue Then
    Begin
      FFlatCheckBoxes := AValue;
      UpdateCaption;
      Invalidate;
    End;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.SetFocus;
Begin
  Inherited;
  If SelectedPart < 0 Then
    SelectedPart := 0;
  UpdateCaption;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.SetSelectedPart(AValue: integer);
Begin
  FSelectedPart := AValue;
  PSCApplyLimits(0,TextParts.Count - 1,FSelectedPart);
  FValue := TextParts.Items[SelectedPart].PartData.GetValue;  
  UpdateCaption;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.SetShowCheckBoxes(AValue: boolean);
Begin
  If FShowCheckBoxes <> AValue Then
    Begin
      FShowCheckBoxes := AValue;
      UpdateCaption;
      Invalidate;
    End;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.SetTextParts(const Value: TPSCTextParts);
Begin
  FTextParts := Value;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.TextPartsChanged(Sender: TObject;
  AItem: TPSCNamedItem);
Begin
  SelectedPart:=PSCMin(PSCMax(0,SelectedPart),TextParts.Count-1);

  UpdateCaption;
  If Assigned(FOnChange) Then
    FOnChange(Self);
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.UpdateCaption;
Begin
  Caption := GetCaption(false);
  Invalidate;
End;

{-------------------------------------}

Procedure TPSCCustomPartsEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
Begin
  With Msg Do
    Begin
      Result := DLGC_WANTMESSAGE Or
        DLGC_WANTALLKEYS Or
        DLGC_WANTARROWS Or
        DLGC_WANTCHARS;
    End;
End;

{-------------------------------------}
procedure TPSCTextPart.SetCheckVisible(AValue: boolean);
begin
  If FCheckVisible <> AValue Then
    Begin
      FCheckVisible := AValue;
      Changed(false);
    End;
end;

end.
