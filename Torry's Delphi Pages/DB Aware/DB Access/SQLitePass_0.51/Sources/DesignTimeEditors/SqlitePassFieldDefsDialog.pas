{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2007 - 2008
    Last update : 31/05/2008

  --------------------------------------------------------------------------- }

unit SqlitePassFieldDefsDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows,
  Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Db, SqlitePassDbo;

type
  { Panels to display FieldDefs information }
  TPanelFieldDefPropertiesValues = class;

  TPanelFieldDefPropertyValue = Class(TPanel)
  Public
  constructor Create(AOwner: TPanel); reintroduce;
  constructor CreateAtPos(AOwner: TPanel; Top, Left: Integer); virtual;
  end;

  TPanelFieldDefTitle = Class(TPanel)
  Public
  constructor Create(AOwner: TComponent); Reintroduce;
  constructor CreateAtPos(AOwner: TComponent; Top, Left: Integer); virtual;
  end;


  TPanelFieldDefBooleanPropertyValue = Class(TPanelFieldDefPropertyValue)
  Private
    FImage: TImage;
    FValue: Boolean;
    procedure SetFValue(const Value: Boolean);
  Public
  Property Value: Boolean Read FValue Write SetFValue;
  constructor CreateAtPos(AOwner: TPanelFieldDefPropertiesValues; Top, Left: Integer); reintroduce;
  destructor Destroy; Override;
  end;

  TPanelFieldDefPropertiesValues = Class(TPanel)
  Private
  FLeft: Integer;
  Protected
  FTop: Integer;
  FVOffset: Integer;
  FDataType: TPanelFieldDefPropertyValue;
  FAutoInc: TPanelFieldDefBooleanPropertyValue;
  FDefaultValue: TPanelFieldDefPropertyValue;
  FDisplayName: TPanelFieldDefPropertyValue;
  FFieldNo: TPanelFieldDefPropertyValue;
  FFieldFullName: TPanelFieldDefPropertyValue;
  FForeignKey: TPanelFieldDefBooleanPropertyValue;
  FHint: TPanelFieldDefPropertyValue;
  FIndexed: TPanelFieldDefBooleanPropertyValue;
  FInternalPrimaryKey: TPanelFieldDefBooleanPropertyValue;
  FNativeDataType: TPanelFieldDefPropertyValue;
  FNativeDataTypeCode: TPanelFieldDefPropertyValue;
  FNotEmpty: TPanelFieldDefBooleanPropertyValue;
  FNoConstraints: TPanelFieldDefBooleanPropertyValue;
  FNotNull: TPanelFieldDefBooleanPropertyValue;
  FPrecision: TPanelFieldDefPropertyValue;
  FPrimaryKey: TPanelFieldDefBooleanPropertyValue;
  FSize: TPanelFieldDefPropertyValue;
  FUnique: TPanelFieldDefBooleanPropertyValue;
  public
  Image: TImage;
  constructor Create(AOwner: TComponent); override;
  constructor CreateAtPos(AOwner: TComponent; Top, Left: Integer; Image: TImage);
  destructor Destroy; Override;
  end;

  TSqlitePassFieldDefsDialog = class(TForm)
    Panel1: TPanel;
    PanelTop: TPanel;
    PanelFieldDefs: TPanel;
    Label16: TLabel;
    Image9: TImage;
    Panel10: TPanel;
    ScrollBarHorizontal: TScrollBar;
    ScrollBoxFieldDefsPropertiesValues: TScrollBox;
    Panel32: TPanel;
    ScrollboxFieldDefsFieldNames: TScrollBox;
    Panel33: TPanel;
    ScrollBoxFieldDefsPropertiesNames: TScrollBox;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel30: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    ScrollBarVertical: TScrollBar;
    ImageCheck: TImage;
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBarHorizontalScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollBarVerticalScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    MyDataset: TSqlitePassDataset;
    IsOpen: Boolean;
    { TList to display FieldDefs information }
    ListofPanelFieldDefProperties: TList;
    ListofPanelFieldDefFieldNames: TList;
    function FieldTypeAsString(FieldType: TFieldType): String;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
  end;

var
  SqlitePassFieldDefsDlg: TSqlitePassFieldDefsDialog;

implementation


{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ TSqlitePassFieldDefsDialog }

constructor TSqlitePassFieldDefsDialog.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset);
var
i: Integer;
NewPanelFieldDefTitle: TPanelFieldDefTitle;
NewPanelFieldDef: TPanelFieldDefPropertiesValues;
PanelFieldDefTop, PanelFieldDefLeft, PanelFieldDefHOffset: Integer;

begin
Inherited Create(AOwner);
MyDataset := Dataset;
IsOpen := Dataset.Active;

if (Not Dataset.Active) and (Dataset.SQLSelectStmt.FieldDefs.Count = 0)
   then Dataset.Open;

ListofPanelFieldDefProperties := TList.Create;
ListofPanelFieldDefFieldNames := TList.Create;

{ --- Display FieldDefs Infos ---  }
ScrollBoxFieldDefsFieldNames.visible := False;
ScrollBoxFieldDefsPropertiesValues.visible := False;

For i := 0 to Pred(ListofPanelFieldDefProperties.Count)
  do begin
  TPanelFieldDefTitle(ListofPanelFieldDefFieldNames[i]).Free;
  TPanelFieldDefPropertiesValues(ListofPanelFieldDefProperties[i]).Free;
  end;

ListofPanelFieldDefProperties.Clear;
ListofPanelFieldDefFieldNames.Clear;

PanelFieldDefHOffset := 0;
PanelFieldDefTop := 2;
PanelFieldDefLeft := 2;

  For i := 0 to Pred(Dataset.SQLSelectStmt.FieldDefs.Count) do
      begin
      NewPanelFieldDefTitle:= TPanelFieldDefTitle.CreateAtPos(ScrollboxFieldDefsFieldNames, PanelFieldDefTop, PanelFieldDefLeft + 2);
      NewPanelFieldDefTitle.Caption := Dataset.SQLSelectStmt.FieldDefs[i].FieldName;
      ListofPanelFieldDefFieldNames.Add(NewPanelFieldDefTitle);
      NewPanelFieldDef := TPanelFieldDefPropertiesValues.CreateAtPos(ScrollBoxFieldDefsPropertiesValues, PanelFieldDefTop, PanelFieldDefLeft, ImageCheck);
      With NewPanelFieldDef do
      begin
      FDataType.Caption := FieldTypeAsString(Dataset.SQLSelectStmt.FieldDefs[i].DataType);
      FDataType.Hint := FDataType.Caption;
      FAutoInc.Value := Dataset.SQLSelectStmt.FieldDefs[i].AutoInc;
      FDefaultValue.Caption := Dataset.SQLSelectStmt.FieldDefs[i].DefaultValue;
      FDefaultValue.Hint := FDefaultValue.Caption;
      FDisplayName.Caption := Dataset.SQLSelectStmt.FieldDefs[i].DisplayName;
      FDisplayName.Hint := FDisplayName.Caption;
//      FFieldNo.Caption := IntToStr(Dataset.SQLSelectStmt.FieldDefs[i].FieldNo);
//        FFieldFullName.Caption := Dataset.SQLSelectStmt.FieldDefs[i].FieldFullName;
      FForeignKey.Value := Dataset.SQLSelectStmt.FieldDefs[i].ForeignKey;
      FHint.Caption := Dataset.SQLSelectStmt.FieldDefs[i].Hint;
      FHint.Hint := FHint.Caption;
      FIndexed.Value := Dataset.SQLSelectStmt.FieldDefs[i].Indexed;
//      FInternalPrimaryKey.Value := Dataset.SQLSelectStmt.FieldDefs[i].InternalPrimaryKey;
      FNativeDataType.Caption := Dataset.SQLSelectStmt.FieldDefs[i].NativeDataType;
      FNativeDataType.Hint := FNativeDataType.Caption;
      FNativeDataTypeCode.Caption := IntToSTr(Dataset.SQLSelectStmt.FieldDefs[i].NativeDataTypeCode);
      FNotEmpty.Value := Dataset.SQLSelectStmt.FieldDefs[i].NotEmpty;
      FNoConstraints.Value := Dataset.SQLSelectStmt.FieldDefs[i].NoConstraints;
      FNotNull.Value := Dataset.SQLSelectStmt.FieldDefs[i].NotNull;
      FPrecision.Caption := IntToStr(Dataset.SQLSelectStmt.FieldDefs[i].Precision);
      FPrimaryKey.Value := Dataset.SQLSelectStmt.FieldDefs[i].PrimaryKey;
      FSize.Caption := IntToStr(Dataset.SQLSelectStmt.FieldDefs[i].Size);
      FUnique.Value := Dataset.SQLSelectStmt.FieldDefs[i].Unique;
      end;
      PanelFieldDefHOffset := NewPanelFieldDef.Width + 2;
      Inc(PanelFieldDefLeft, PanelFieldDefHOffset);
      ListofPanelFieldDefProperties.Add(NewPanelFieldDef);
  end; // For i

ScrollbarHorizontal.Max := PanelFieldDefHOffset + 2000;
ScrollBoxFieldDefsFieldNames.visible := True;
ScrollBoxFieldDefsPropertiesValues.visible := True;
end;

procedure TSqlitePassFieldDefsDialog.FormDestroy(Sender: TObject);
begin
// TODO - Free Items
ListofPanelFieldDefProperties.Free;
ListofPanelFieldDefFieldNames.Free;
MyDataset.Active := IsOpen;
end;

function TSqlitePassFieldDefsDialog.FieldTypeAsString(FieldType: TFieldType): String;
begin
Case FieldType of
     ftString      : Result := 'ftString';
     ftSmallint    : Result := 'ftSmallint';
     ftInteger     : Result := 'ftInteger';
     ftWord        : Result := 'ftWord';
     ftBoolean     : Result := 'ftBoolean';
     ftFloat       : Result := 'ftFloat';
     ftCurrency    : Result := 'ftCurrency';
     ftBCD         : Result := 'ftBCD';
     ftDate        : Result := 'ftDate';
     ftTime        : Result := 'ftTime';
     ftDateTime    : Result := 'ftDateTime';
     ftBytes       : Result := 'ftBytes';
     ftVarBytes    : Result := 'ftVarBytes';
     ftAutoInc     : Result := 'ftAutoInc';
     ftBlob        : Result := 'ftBlob';
     ftMemo        : Result := 'ftMemo';
     ftGraphic     : Result := 'ftGraphic';
     ftFmtMemo     : Result := 'ftFmtMemo';
     ftParadoxOle  : Result := 'ftParadoxOle';
     ftDBaseOle    : Result := 'ftDBaseOle';
     ftTypedBinary : Result := 'ftTypedBinary';
     ftCursor      : Result := 'ftCursor';
     ftFixedChar   : Result := 'ftFixedChar';
     ftWideString  : Result := 'ftWideString';
     ftLargeint    : Result := 'ftLargeint';
     ftADT         : Result := 'ftADT';
     ftArray       : Result := 'ftArray';
     ftReference   : Result := 'ftReference';
     ftDataSet     : Result := 'ftDataSet';
     else
     Result := 'ftUnknown';
     end;
end;

{ TPanelFieldDefPropertyValue }

constructor TPanelFieldDefPropertyValue.Create(AOwner: TPanel);
begin
Inherited Create(AOwner);
Parent := AOWner;
Height := 22;
Width := 102;
Color := clBtnface;
BevelInner := bvSpace;
BevelOuter := bvLowered;
Inc(TPanelFieldDefPropertiesValues(Parent).FTop, TPanelFieldDefPropertiesValues(Parent).FVOffset);
end;

constructor TPanelFieldDefPropertyValue.CreateAtPos(AOwner: TPanel;
  Top, Left: Integer);
begin
Create(AOwner);
Self.Top := Top;
Self.Left := Left;
end;

{ TPanelFieldDefPropertiesValues }

constructor TPanelFieldDefPropertiesValues.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Parent := TScrollBox(AOwner);
end;

constructor TPanelFieldDefPropertiesValues.CreateAtPos(AOwner: TComponent; Top, Left: Integer; Image: TImage);
begin
Create(AOwner);
Self.Top := Top;
Self.Left := Left;
Width := 110;
Height := 600;
Color := clBtnface;
ShowHint := True;
BevelInner := bvSpace;
BevelOuter := bvSpace;
FVOffset := 22;
FTop := 1;
FLeft := 4;
Self.Image := Image;
FDataType := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FAutoInc := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FDefaultValue := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FDisplayName := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FFieldNo := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FFieldFullName := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FForeignKey := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FHint := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FIndexed := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FInternalPrimaryKey := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FNativeDataType := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FNativeDataTypeCode := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FNotEmpty := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FNoConstraints := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FNotNull := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FPrecision := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FPrimaryKey := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
FSize := TPanelFieldDefPropertyValue.CreateAtPos(Self, FTop, FLeft);
FUnique := TPanelFieldDefBooleanPropertyValue.CreateAtPos(Self, FTop, FLeft);
end;

destructor TPanelFieldDefPropertiesValues.Destroy;
begin
FDataType.Free;
FAutoInc.Free;
FDefaultValue.Free;
FDisplayName.Free;
FFieldNo.Free;
FFieldFullName.Free;
FForeignKey.Free;
FHint.Free;
FIndexed.Free;
FInternalPrimaryKey.Free;
FNativeDataType.Free;
FNativeDataTypeCode.Free;
FNotEmpty.Free;
FNoConstraints.Free;
FNotNull.Free;
FPrecision.Free;
FPrimaryKey.Free;
FSize.Free;
FUnique.Free;
Inherited Destroy;
end;

{ TPanelFieldDefBooleanPropertyValue }

constructor TPanelFieldDefBooleanPropertyValue.CreateAtPos(
  AOwner: TPanelFieldDefPropertiesValues; Top, Left: Integer);
begin
Inherited CreateAtPos(TPanel(AOwner), Top, Left);
FImage := TImage.Create(Self);
With FImage do
  begin
  Parent := Self;
  Width := 16;
  Height := 16;
  Transparent := True;
  Left := (Self.Width - Width) div 2;
  Top := (Self.Height - Height) div 2;
  end;
end;

destructor TPanelFieldDefBooleanPropertyValue.Destroy;
begin
FImage.Free;
Inherited Destroy;
end;

procedure TPanelFieldDefBooleanPropertyValue.SetFValue(
  const Value: Boolean);
begin
  FValue := Value;
  If FValue = True
     then FImage.Picture := TPanelFieldDefPropertiesValues(Owner).Image.Picture
     else FImage.Picture := nil;
end;

{ TPanelFieldDefTitle }

constructor TPanelFieldDefTitle.Create(AOwner: TComponent);
begin
Inherited Create(AOwner);
Parent := TScrollBox(AOwner);
Height := 22;
Width := 106;
Color := clBlack;
Font.Color := clWhite;
BevelInner := bvLowered;
BevelOuter := bvLowered;
end;

constructor TPanelFieldDefTitle.CreateAtPos(AOwner: TComponent; Top,
  Left: Integer);
begin
Create(AOwner);
Self.Top := Top;
Self.Left := Left;
end;

procedure TSqlitePassFieldDefsDialog.ScrollBarHorizontalScroll(
  Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
{$IFNDEF FPC}
ScrollboxFieldDefsFieldNames.ScrollBy(ScrollbarHorizontal.Position-ScrollPos,0);
ScrollboxFieldDefsPropertiesValues.ScrollBy(ScrollbarHorizontal.Position-ScrollPos,0);
{$ENDIF}
end;

procedure TSqlitePassFieldDefsDialog.ScrollBarVerticalScroll(
  Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
{$IFNDEF FPC}
ScrollboxFieldDefsPropertiesNames.ScrollBy(0,ScrollbarVertical.Position-ScrollPos);
ScrollboxFieldDefsPropertiesValues.ScrollBy(0,ScrollbarVertical.Position-ScrollPos);
{$ENDIF}
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassFieldDefsDialog.lrs}
 {$ENDIF}
end.
