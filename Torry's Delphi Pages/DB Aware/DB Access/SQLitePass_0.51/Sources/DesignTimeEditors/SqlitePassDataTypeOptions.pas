unit SqlitePassDataTypeOptions;
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
  StdCtrls, ExtCtrls, Buttons, Db, SqlitePassDbo, SqlitePassUtils;

type
  TSqlitePassDatatypeOptionsDlg = class(TForm)
    CbBooleanExtensions: TComboBox;
    Label6: TLabel;
    CbDateFormat: TComboBox;
    CbTimeFormat: TComboBox;
    CbDateTimeFormat: TComboBox;
    Label12: TLabel;
    CbDefaultFieldType: TComboBox;
    Label4: TLabel;
    CbDateTimeStorage: TComboBox;
    Label5: TLabel;
    Shape8: TShape;
    Shape7: TShape;
    Shape9: TShape;
    Label7: TLabel;
    Shape10: TShape;
    Label13: TLabel;
    CbDecimalSeparator: TComboBox;
    PanelBottom: TPanel;
    BtnOk: TButton;
    BtnResetAll: TButton;
    BtnSaveToDatabase: TButton;
    BtnLoadFromDatabase: TButton;
    Label2: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Label3: TLabel;
    Label8: TLabel;
    CbDateStorage: TComboBox;
    Label9: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    Label1: TLabel;
    Label14: TLabel;
    CbTimeStorage: TComboBox;
    Label15: TLabel;
    Shape5: TShape;
    Shape6: TShape;
    Label10: TLabel;
    Label11: TLabel;
    CbBooleanStorage: TComboBox;
    Label17: TLabel;
    CbFieldTypeDetectionMode: TComboBox;
    BtnLoadDefault: TButton;
    procedure BtnLoadFromDatabaseClick(Sender: TObject);
    procedure BtnSaveToDatabaseClick(Sender: TObject);
    procedure BtnResetAllClick(Sender: TObject);
    procedure BtnLoadDefaultClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private
    Db: TSqlitePassDatabase;
    procedure RefreshDisplay;
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

var
  SqlitePassDatatypeOptionsDlg: TSqlitePassDatatypeOptionsDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

constructor TSqlitePassDatatypeOptionsDlg.Create(AOwner: TComponent);
begin
  Inherited Create(nil);
  Db := TSqlitePassDatabase(AOwner);
  BtnResetAllClick(Self);
end;

procedure TSqlitePassDatatypeOptionsDlg.BtnLoadFromDatabaseClick(
  Sender: TObject);
begin
 Db.DatatypeOptions.LoadFromDatabase([loCustomProperties]);
 RefreshDisplay;
end;

procedure TSqlitePassDatatypeOptionsDlg.BtnSaveToDatabaseClick(
  Sender: TObject);
begin
  BtnOkClick(Sender);
  Db.DatatypeOptions.SaveToDatabase([soCustomProperties]);
end;

procedure TSqlitePassDatatypeOptionsDlg.BtnResetAllClick(Sender: TObject);
begin
  RefreshDisplay;
end;

procedure TSqlitePassDatatypeOptionsDlg.RefreshDisplay;
var
ft: TFieldType;
begin
  With Db.DatatypeOptions do
       begin
       CbDateTimeStorage.ItemIndex := Ord(DateTimeStorage);
       CbDateTimeFormat.Text := DateTimeFormat;
       CbDateStorage.ItemIndex := Ord(DateStorage);
       CbDateFormat.Text := DateFormat;
       CbTimeStorage.ItemIndex := Ord(TimeStorage);
       CbTimeFormat.Text := TimeFormat;
       CbBooleanStorage.ItemIndex := Ord(BooleanStorage);
       CbBooleanExtensions.Text := BooleanFormat;
       CbDecimalSeparator.Text := DecimalSeparator;
       CbFieldTypeDetectionMode.ItemIndex := Ord(DetectionMode);
       CbDefaultFieldType.Clear;
       For ft := Low(TFieldType) to High(TFieldType)
           do CbDefaultFieldType.Items.Add(FieldTypeToString(ft));
       CbDefaultFieldType.ItemIndex := CbDefaultFieldType.Items.IndexOf(FieldTypeToString(DefaultFieldType));
       end;
end;


procedure TSqlitePassDatatypeOptionsDlg.BtnLoadDefaultClick(Sender: TObject);
begin
 Db.DatatypeOptions.SetDefaultPropertiesValues;
 RefreshDisplay;
end;

procedure TSqlitePassDatatypeOptionsDlg.BtnOkClick(Sender: TObject);
begin
  With Db.DatatypeOptions do
       begin
       DateTimeStorage := TSqlitePassDateTimeStorage(Ord(CbDateTimeStorage.ItemIndex));
       DateTimeFormat := CbDateTimeFormat.Text;
       DateStorage := TSqlitePassDateStorage(Ord(CbDateStorage.ItemIndex));
       DateFormat := CbDateFormat.Text;
       TimeStorage := TSqlitePassTimeStorage(Ord(CbTimeStorage.ItemIndex));
       TimeFormat := CbTimeFormat.Text;
       BooleanStorage := TSqlitePassBooleanStorage(Ord(CbBooleanStorage.ItemIndex));
       BooleanFormat := CbBooleanExtensions.Text;
       DecimalSeparator := Char(CbDecimalSeparator.Text[1]);
       DefaultFieldType := StringToFieldType(CbDefaultFieldType.Text);
       DetectionMode := TSqlitePassDataTypeDetectionMode(Ord(CbFieldTypeDetectionMode.ItemIndex));
       end;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassDataTypeOptions.lrs}
 {$ENDIF}
end.
