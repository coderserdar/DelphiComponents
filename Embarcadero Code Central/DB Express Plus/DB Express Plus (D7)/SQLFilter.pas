unit SQLFilter;
                       
{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}


interface
                           
uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, SQLMetaData, DBClient, Dialogs, StdCtrls, Buttons;

type
  TFilterButtonStyle = (fbsButton, fbsSpeedButton, fbsNone);

  TSQLFilter = class(TBitBtn)
  private
    FClientDataSet: TClientDataSet;
    FFilterButtonStyle: TFilterButtonStyle;
    FFilterDbType: String;
    FFilterDialogCaption: String;
    FFilterDialogColor: TColor;
    FFilterTableName: String;
    FFilterTableNameOld: String;
    FGLAcctSizes: Variant;
    FSQLMetaData: TSQLMetaData;
    FSQLWhereClause: String;
    procedure SetFilterButtonStyle(Value: TFilterButtonStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function Execute: String; overload;
    function Execute(aSQLMetaData: TSQLMetaData; aTableName, aDbType, aCaption: ShortString;
       aBackGroundColor: TColor; aGLAcctSizes: Variant): String; overload;
    function Execute(aClientDataSet: TClientDataSet; aTableName, aDbType, aCaption: ShortString;
       aBackGroundColor: TColor; aGLAcctSizes: Variant): String; overload;
    function SetGLAcctSizes(aM, aS1, aS2, aS3, aS4, aS5: Integer): Variant;
  published
    property FilterButtonStyle: TFilterButtonStyle
                read FFilterButtonStyle write SetFilterButtonStyle;
    property FilterDbType: String read FFilterDbType write FFilterDbType;
    property FilterDialogCaption: String read FFilterDialogCaption write FFilterDialogCaption;
    property FilterDialogColor: TColor read FFilterDialogColor write FFilterDialogColor;
    property FilterTableName: String read FFilterTableName write FFilterTableName;
    property GLAcctSizes: Variant read FGLAcctSizes write FGLAcctSizes;
    property SQLWhereClause: String read FSQLWhereClause write FSQLWhereClause;
    property SQLMetaData: TSQLMetaData read FSQLMetaData write FSQLMetaData;
  end;

implementation

uses
  SQLFilterDlg;

constructor TSQLFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF BSSVCLDEMO}
  if TestDemoStatus = False then Halt;
  {$ENDIF}
  ControlStyle := ControlStyle - [csSetCaption];
  Hint := 'Filter';
  ShowHint := True;
  SetFilterButtonStyle(fbsSpeedButton);
  FFilterDialogCaption := 'SQL Filter Dialog Wizard';
  FFilterDialogColor := clTeal;
  Glyph.Handle := LoadBitmap(hInstance, 'TSQLFilterSB');
end;

destructor TSQLFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TSQLFilter.SetFilterButtonStyle(Value: TFilterButtonStyle);
begin
  if Value <> FFilterButtonStyle then begin
    FFilterButtonStyle := Value;
    if Value = fbsButton then begin
      Height := 25;
      Width := 75;
      Caption := 'Filter';
      Layout := blGlyphLeft;
      TabStop := True;
      Visible := True;
    end;
    if Value = fbsSpeedButton then begin
      Height := 25;
      Width := 25;
      Caption := '';
      Layout := blGlyphRight;
      TabStop := False;
      Visible := True;
    end;
    if Value = fbsNone then begin
      Height := 25;
      Width := 25;
      Caption := '';
      Layout := blGlyphRight;
      TabStop := False;
      Visible := False;
    end;
  end;
end;

procedure TSQLFilter.Click;
begin
  inherited Click;
  Execute;
end;

function TSQLFilter.Execute: String;
var
  aTempResult:  String;
begin
  if FFilterTableNameOld <> FFilterTableName then FSQLWhereClause := '';
  aTempResult := SQLFilterDlgCreate(FSQLMetaData, FFilterTableName, FFilterDbType,
     FFilterDialogCaption, FFilterDialogColor, FGLAcctSizes, FSQLWhereClause);
  if aTempResult <> '[ABORT]' then begin
    FFilterTableNameOld := FFilterTableName;
    FSQLWhereClause := aTempResult;
  end;
  Result := aTempResult;
end;

function TSQLFilter.Execute(aSQLMetaData: TSQLMetaData; aTableName, aDbType,
   aCaption: ShortString; aBackGroundColor: TColor; aGLAcctSizes: Variant): String;
var
  aTempResult:  String;
begin
  FSQLMetaData := aSQLMetaData;
  FClientDataSet := nil;
  FFilterTableName := aTableName;
  FFilterDbType := aDbType;
  FFilterDialogCaption := aCaption;
  FFilterDialogColor := aBackGroundColor;
  FGLAcctSizes := aGLAcctSizes;
  if FFilterTableNameOld <> FFilterTableName then FSQLWhereClause := '';
  aTempResult := SQLFilterDlgCreate(FSQLMetaData, FFilterTableName, FFilterDbType,
     FFilterDialogCaption, FFilterDialogColor, FGLAcctSizes, FSQLWhereClause);
  if aTempResult <> '[ABORT]' then begin
    FFilterTableNameOld := FFilterTableName;
    FSQLWhereClause := aTempResult;
  end;
  Result := aTempResult;
end;

function TSQLFilter.Execute(aClientDataSet: TClientDataSet; aTableName, aDbType, aCaption: ShortString;
       aBackGroundColor: TColor; aGLAcctSizes: Variant): String;
var
  aTempResult:  String;
begin
  FSQLMetaData := nil;
  FClientDataSet := aClientDataSet;
  FFilterTableName := aTableName;
  FFilterDbType := aDbType;
  FFilterDialogCaption := aCaption;
  FFilterDialogColor := aBackGroundColor;
  FGLAcctSizes := aGLAcctSizes;
  if FFilterTableNameOld <> FFilterTableName then FSQLWhereClause := '';
  aTempResult := SQLFilterDlgCreate(FClientDataSet, FFilterTableName, FFilterDbType,
     FFilterDialogCaption, FFilterDialogColor, FGLAcctSizes, FSQLWhereClause);
  if aTempResult <> '[ABORT]' then begin
    FFilterTableNameOld := FFilterTableName;
    FSQLWhereClause := aTempResult;
  end;
  Result := aTempResult;
end;

function TSQLFilter.SetGLAcctSizes(aM, aS1, aS2, aS3, aS4, aS5: Integer): Variant;
var
  aTempVar: Variant;
begin
  aTempVar := VarArrayCreate([0, 5], varVariant);
end;

end.
