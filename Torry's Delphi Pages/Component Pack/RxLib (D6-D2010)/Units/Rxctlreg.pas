{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

{ Note:
  - in Delphi 4.0 you must add DCLSTD40 and DCLSMP40 to the requires
    page of the package you install this components into.
  - in Delphi 3.0 you must add DCLSTD30 and DCLSMP30 to the requires
    page of the package you install this components into.
  - in C++Builder 3.0 you must add DCLSTD35 to the requires page of the
    package you install this components into. }

unit RxCtlReg;

{$I RX.INC}
{$D-,L-,S-}

interface

{ Register custom useful controls }

procedure Register;

implementation

{$R *.D32}

uses
  Windows, Classes, SysUtils,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  {$IFDEF RX_D3} DsnConst, ExtDlgs, {$ELSE} LibConst, {$ENDIF} 
{$IFDEF DCS}
  {$IFDEF RX_D4} ImgEdit, {$ENDIF} ImgList,
{$ENDIF DCS}
  RxRichEd, Menus, FiltEdit, StdCtrls, Buttons,
  RxLConst, RxCtrls, RxGrids, rxCurrEdit, rxToolEdit, rxHintProp, rxDateUtil,
  rxPickDate, RxSplit, RxSlider, RxClock, rxAnimate, RxCombos, RxSpin, Consts,
  RxDice, RxSwitch, rxCheckItm, rxVCLUtils, RxColors, rxAniFile, RxGraph,
  {$IFDEF USE_RX_GIF} RxGIF, rxGIFCtrl, {$ENDIF} RxHints, rxExcptDlg, RxCConst,
  rxFileUtil, RxDsgn;

{$IFNDEF RX_D3}

{ TDateProperty }

type
  TDateProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TDateProperty.GetValue: string;
begin
  if GetFloatValue = NullDate then Result := ''
  else Result := FormatDateTime(ShortDateFormat, GetFloatValue);
end;

procedure TDateProperty.SetValue(const Value: string);
begin
  if Value = '' then SetFloatValue(NullDate)
  else SetFloatValue(StrToDateFmt(ShortDateFormat, Value));
end;

{ TRxModalResultProperty }

type
  TRxModalResultProperty = class(TModalResultProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

const
  ModalResults: array[mrAll..mrYesToAll] of string = (
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

function TRxModalResultProperty.GetValue: string;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
    else
      Result := inherited GetValue;
  end;
end;

procedure TRxModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := Low(ModalResults) to High(ModalResults) do
    Proc(ModalResults[I]);
end;

procedure TRxModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if (Value <> '') then
    for I := Low(ModalResults) to High(ModalResults) do
      if CompareText(ModalResults[I], Value) = 0 then
      begin
        SetOrdValue(I);
        Exit;
      end;
  inherited SetValue(Value);
end;

{$ENDIF RX_D3}

function ValueName(E: Extended): string;
begin
  if E = High(Integer) then Result := 'MaxInt'
  else if E = Low(Integer) then Result := 'MinInt'
  else if E = High(Longint) then Result := 'MaxLong'
  else if E = Low(Longint) then Result := 'MinLong'
  else if E = High(ShortInt) then Result := 'MaxShort'
  else if E = Low(ShortInt) then Result := 'MinShort'
  else if E = High(Word) then Result := 'MaxWord'
  else Result := '';
end;

function StrToValue(const S: string): Longint;
begin
  if CompareText(S, 'MaxLong') = 0 then Result := High(Longint)
  else if CompareText(S, 'MinLong') = 0 then Result := Low(Longint)
  else if CompareText(S, 'MaxInt') = 0 then Result := High(Integer)
  else if CompareText(S, 'MinInt') = 0 then Result := Low(Integer)
  else if CompareText(S, 'MaxShort') = 0 then Result := High(ShortInt)
  else if CompareText(S, 'MinShort') = 0 then Result := Low(ShortInt)
  else if CompareText(S, 'MaxWord') = 0 then Result := High(Word)
  else Result := 0;
end;

{ TRxIntegerProperty }

type
  TRxIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TRxIntegerProperty.GetValue: string;
begin
  Result := ValueName(GetOrdValue);
  if Result = '' then
    Result := IntToStr(GetOrdValue);
end;

procedure TRxIntegerProperty.SetValue(const Value: String);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L = 0 then
    L := StrToInt(Value);
  inherited SetValue(IntToStr(L));
end;

{ TRxFloatProperty }

type
  TRxFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TRxFloatProperty.GetValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
  Result := ValueName(GetFloatValue);
  if Result = '' then
    Result := FloatToStrF(GetFloatValue, ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TRxFloatProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L <> 0 then
    SetFloatValue(L)
  else
    SetFloatValue(StrToFloat(Value));
end;

{ TPaintBoxEditor }

type
  TPaintBoxEditor = class(TDefaultEditor)
  public
{$IFDEF RX_D6}   // Polaris
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
{$ENDIF}
  end;

{$IFDEF RX_D6}   // Polaris
procedure TPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then begin
    PropertyEditor.Edit;
    Continue := False;
  end
{$IFDEF RX_D6}   // Polaris
  else inherited EditProperty(PropertyEditor, Continue);
{$ELSE}
  else inherited EditProperty(PropertyEditor, Continue, FreeEditor);
{$ENDIF}
end;

{ TAnimatedEditor }

type
  TAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
{$IFDEF RX_D6}  // Polaris
    procedure CheckEdit(const PropertyEditor: IProperty);
{$ELSE}
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
{$ENDIF}
    procedure EditImage(Image: TAnimatedImage);
    procedure LoadAniFile(Image: TAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$IFDEF RX_D6}  // Polaris
procedure TAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
{$ELSE}
procedure TAnimatedEditor.CheckEdit(PropertyEditor: TPropertyEditor);
{$ENDIF}
begin
  try
    if FContinue and (CompareText(PropertyEditor.GetName, 'GLYPH') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  finally
{$IFNDEF RX_D6}  // Polaris
    PropertyEditor.Free;
{$ENDIF}
  end;
end;

procedure TAnimatedEditor.EditImage(Image: TAnimatedImage);
var
{$IFDEF RX_D6}  // Polaris
  Components: IDesignerSelections;
{$ELSE}
  Components: TDesignerSelectionList;
{$ENDIF}
begin
{$IFDEF RX_D6}  // Polaris
  Components := CreateSelectionlist;
{$ELSE}
  Components := TDesignerSelectionList.Create;
{$ENDIF}
  try
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  finally
{$IFNDEF RX_D6}  // Polaris
    Components.Free;
{$ENDIF}
  end;
end;

procedure TAnimatedEditor.LoadAniFile(Image: TAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TAnimatedCursorImage;
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do
    begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := LoadStr(srAniCurFilter);
      if Execute then
      begin
        AniCursor := TAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
            Image.Orientation = goVertical);
          Image.Interval := AniCursor.DefaultRate;
          Image.TransparentColor := clFuchsia;
          Designer.Modified;
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
    SetCurrentDir(CurDir);
  end;
end;

procedure TAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 1) then
    LoadAniFile(TAnimatedImage(Component))
  else
    if (Index = GetVerbCount - 2) then
      EditImage(TAnimatedImage(Component))
    else
      inherited ExecuteVerb(Index);
end;

function TAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 1) then
    Result := LoadStr(srLoadAniCursor)
  else
    if (Index = GetVerbCount - 2) then
      Result := LoadStr(srEditPicture)
    else
      Result := inherited GetVerb(Index);
end;

function TAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{$IFDEF DCS}

type
  TRxImageListEditor = class(TComponentEditor)
  private
    procedure SaveAsBitmap(ImageList: TImageList);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TRxImageListEditor.SaveAsBitmap(ImageList: TImageList);
var
  Bitmap: TBitmap;
  SaveDlg: TOpenDialog;
  I: Integer;
begin
  if ImageList.Count > 0 then
  begin
    SaveDlg :=
      {$IFDEF RX_D3}TSavePictureDialog{$ELSE}TSaveDialog{$ENDIF}.Create(Application);
    with SaveDlg do
    try
      Options := [ofHideReadOnly, ofOverwritePrompt];
      DefaultExt := GraphicExtension(TBitmap);
      Filter := GraphicFilter(TBitmap);
      if Execute then
      begin
        Bitmap := TBitmap.Create;
        try
          with Bitmap do
          begin
            Width := ImageList.Width * ImageList.Count;
            Height := ImageList.Height;
            if ImageList.BkColor <> clNone then
              Canvas.Brush.Color := ImageList.BkColor
            else
              Canvas.Brush.Color := clWindow;
            Canvas.FillRect(Bounds(0, 0, Width, Height));
            for I := 0 to ImageList.Count - 1 do
              ImageList.Draw(Canvas, ImageList.Width * I, 0, I);
{$IFDEF RX_D3}
            HandleType := bmDIB;
            if PixelFormat in [pf15bit, pf16bit] then
            try
              PixelFormat := pf24bit;
            except
            end;
{$ENDIF}
          end;
          Bitmap.SaveToFile(FileName);
        finally
          Bitmap.Free;
        end;
      end;
    finally
      Free;
    end;
  end
  else
    Beep;
end;

procedure TRxImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Designer <> nil then
    case Index of
      0: if EditImageList(Component as TImageList) then
           Designer.Modified;
      1: SaveAsBitmap(TImageList(Component));
    end;
end;

function TRxImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
{$IFDEF RX_D3}
    0: Result := SImageListEditor;
{$ELSE}
    0: Result := LoadStr(SImageEditor);
{$ENDIF}
    1: Result := LoadStr(srSaveImageList);
  else
    Result := '';
  end;
end;

function TRxImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$ENDIF DCS}

{ TWeekDayProperty }

type
  TWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

function TWeekDayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

{$IFDEF RX_D3}
resourcestring
  srSamples = 'Samples';
{$ENDIF}

procedure Register;
const
{$IFDEF RX_D3}
  BaseClass: TClass = TPersistent;
{$ELSE}
  BaseClass: TClass = TComponent;
{$ENDIF}
begin
  RegisterComponents(LoadStr(srRXControls), [TComboEdit, TFilenameEdit,
    TDirectoryEdit, TDateEdit, TRxCalcEdit, TCurrencyEdit, TTextListBox,
    TRxCheckListBox, TFontComboBox, TColorComboBox, TRxSplitter, TRxSlider,
    TRxLabel, TRxRichEdit,
    TRxClock, TAnimatedImage, TRxDrawGrid, TRxSpeedButton,
    {$IFDEF USE_RX_GIF} TRxGIFAnimator, {$ENDIF} TRxSpinButton, TRxSpinEdit,
    TRxSwitch, TRxDice]);
{$IFDEF CBUILDER}
 {$IFNDEF RX_V110} { C++Builder 1.0 }
  RegisterComponents(ResStr(srAdditional), [TScroller]);
 {$ELSE}
  RegisterComponents(ResStr(srSamples), [TScroller]);
 {$ENDIF}
{$ELSE}
  RegisterComponents(ResStr(srSamples), [TScroller]);
{$ENDIF}

{$IFDEF RX_D3}
  RegisterNonActiveX([TCustomComboEdit, TCustomDateEdit, TCustomNumEdit,
    TFileDirEdit, TRxCustomListBox, TRxRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);
{$ENDIF RX_D3}

  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TWeekDayProperty);
{$IFDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(string), TCustomNumEdit, 'Text', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(string), TCustomNumEdit, 'Text', TStringProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TFileDirEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomDateEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TFileNameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TFileNameEdit, 'FileName', TFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDirectoryEdit, 'Text', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', THintProperty);
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, 'Hint', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomComboEdit, 'ButtonHint', THintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TRxCheckListBox, 'Items', TCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TProgressControlProperty);
{$IFDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(Boolean), TFontComboBox, 'TrueTypeOnly', nil);
  RegisterPropertyEditor(TypeInfo(TCursor), TRxSplitter, 'Cursor', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(TDateTime), TPersistent, '', TDateProperty);
  RegisterPropertyEditor(TypeInfo(TModalResult), TPersistent, '', TRxModalResultProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TRxLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TRxSpeedButton, 'Caption', THintProperty);

  RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TRxIntegerProperty);

  RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TRxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TRxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TRxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TRxFloatProperty);

  RegisterComponentEditor(TPaintBox, TPaintBoxEditor);
  RegisterComponentEditor(TAnimatedImage, TAnimatedEditor);
{$IFDEF DCS}
  RegisterComponentEditor(TCustomImageList, TRxImageListEditor);
  RegisterComponentEditor(TImageList, TRxImageListEditor);
{$ENDIF}
  RegisterRxColors;
end;

end.