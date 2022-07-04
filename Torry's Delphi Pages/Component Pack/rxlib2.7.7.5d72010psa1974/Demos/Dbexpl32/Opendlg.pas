{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997, 1998 Master-Bank              }
{                                                       }
{*******************************************************}

unit OpenDlg;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Mask, ToolEdit, RXLookup, DB, DBLists, ExtCtrls, Placemnt,
  PicClip;

type
  TOpenDatabaseDlg = class(TForm)
    Bevel1: TBevel;
    DatabaseList: TBDEItems;
    DataSource1: TDataSource;
    rxDBLookupCombo1: TrxDBLookupCombo;
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    FormStorage: TFormStorage;
    PicClip: TPicClip;
    procedure rxDBLookupCombo1Change(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure DBLookupComboGetImage(Sender: TObject; IsEmpty: Boolean;
      var Graphic: TGraphic; var TextMargin: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetDatabaseName: string;
  public
    { Public declarations }
    property DatabaseName: string read GetDatabaseName;
  end;

function GetOpenDatabase(var DBName: string): Boolean;

implementation

{$R *.DFM}

uses SysUtils, DBUtils, BdeUtils;

type
  TDBType = (dtSQL, dtStandard, dtODBC);

const
  NativeDrivers: array[0..6] of string = ('ORACLE', 'INTRBASE', 'SYBASE',
    'INFORMIX', 'DB2', 'MSSQL', 'MSACCESS');

function GetDBType(const DriverName: string): TDBType;
var
  I: Integer;
begin
  if CompareText(DriverName, 'STANDARD') = 0 then
    Result := dtStandard
  else begin
    Result := dtODBC;
    for I := Low(NativeDrivers) to High(NativeDrivers) do begin
      if CompareText(DriverName, NativeDrivers[I]) = 0 then begin
        Result := dtSQL;
        Exit;
      end;
    end;
  end;
end;

function GetOpenDatabase(var DBName: string): Boolean;
begin
  Result := False;
  with TOpenDatabaseDlg.Create(Application) do
  try
    if ShowModal = mrOk then begin
      DBName := DatabaseName;
      Result := DBName <> '';
    end;
  finally
    Free;
  end;
end;

{ TOpenDatabaseDlg }

function TOpenDatabaseDlg.GetDatabaseName: string;
begin
  Result := rxDBLookupCombo1.DisplayValue;
  if Result = '' then Result := DirectoryEdit1.Text;
end;

procedure TOpenDatabaseDlg.rxDBLookupCombo1Change(Sender: TObject);
begin
  if DataSetFindValue(DatabaseList, rxDBLookupCombo1.Value, 'NAME') then
    DirectoryEdit1.Text := DatabaseList.FieldByName('PHYNAME').AsString;
end;

procedure TOpenDatabaseDlg.DirectoryEdit1Change(Sender: TObject);
begin
  if DirectoryEdit1.Text <> '' then begin
    if DataSetFindValue(DatabaseList, DirectoryEdit1.Text, 'PHYNAME') then
      rxDBLookupCombo1.Value := DatabaseList.FieldByName('NAME').AsString
    else rxDBLookupCombo1.ResetField;
  end;
end;

procedure TOpenDatabaseDlg.DBLookupComboGetImage(Sender: TObject;
  IsEmpty: Boolean; var Graphic: TGraphic; var TextMargin: Integer);
begin
  TextMargin := PicClip.Width + 2;
  if not IsEmpty then begin
    Graphic := PicClip.GraphicCell[Ord(GetDBType(
      DatabaseList.FieldByName('DBTYPE').AsString))];
  end;
end;

procedure TOpenDatabaseDlg.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  DirectoryEdit1.DialogText := 'Select a path to the target database.';
  DirectoryEdit1.DialogKind := dkWin32;
{$ENDIF}
end;

end.
