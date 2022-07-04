unit OptDlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, TabNotBk, Placemnt, DB, DBLists,
  RXLookup, Spin, DBTables;

type
  TOptionsDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    FormStorage: TFormStorage;
    Notebook: TTabbedNotebook;
    GroupBox1: TGroupBox;
    KeepConnectBtn: TCheckBox;
    ShowSystemBtn: TCheckBox;
    AutoActivateBtn: TCheckBox;
    GroupBox2: TGroupBox;
    FloatFormatEdit: TEdit;
    DateFormatEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    DataSource1: TDataSource;
    LangDrivList: TBDEItems;
    Label4: TLabel;
    TimeFormatEdit: TEdit;
    Label5: TLabel;
    DateTimeFormatEdit: TEdit;
    GroupBox4: TGroupBox;
    FixedBtn: TRadioButton;
    DelimitedBtn: TRadioButton;
    Image1: TImage;
    Label6: TLabel;
    CharsetEdit: TrxDBLookupCombo;
    Label3: TLabel;
    Image2: TImage;
    Image3: TImage;
    GroupBox3: TGroupBox;
    Image4: TImage;
    Label7: TLabel;
    Label8: TLabel;
    MaxHistoryEdit: TSpinEdit;
    SQLFontBtn: TButton;
    SQLMemo: TMemo;
    FontDialog: TFontDialog;
    GroupBox5: TGroupBox;
    LiveQueryBtn: TCheckBox;
    AbortQueryBtn: TCheckBox;
    Image5: TImage;
    ShowTimeBtn: TCheckBox;
    SQLCountBtn: TCheckBox;
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OptionsChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLFontBtnClick(Sender: TObject);
    procedure LangDrivListAfterOpen(DataSet: TDataset);
  private
    { Private declarations }
    procedure ApplyOptions;
    procedure SetOptions;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

implementation

uses Consts, IniFiles, VCLUtils, DBUtils, BdeUtils, Options, Main;

{$R *.DFM}

{ TOptionsDialog }

procedure TOptionsDialog.ApplyOptions;
begin
  AutoActivate := AutoActivateBtn.Checked;
  SystemTables := ShowSystemBtn.Checked;
  ASCIIDelimited := DelimitedBtn.Checked;
  SetKeepConnections(KeepConnectBtn.Checked);
  ASCIICharSet := CharsetEdit.Value;
  if ASCIICharSet = '' then ASCIICharSet := 'ascii';
  defFloatFormat := FloatFormatEdit.Text;
  defDateFormat := DateFormatEdit.Text;
  defTimeFormat := TimeFormatEdit.Text;
  defDateTimeFormat := DateTimeFormatEdit.Text;
  SQLHistoryCapacity := MaxHistoryEdit.Value;
  LiveQueries := LiveQueryBtn.Checked;
  ShowExecTime := ShowTimeBtn.Checked;
  SQLCalcCount := SQLCountBtn.Checked;
  if (Application.MainForm <> nil) then
    TDBExplorerMainForm(Application.MainForm).SQLFontContainer.Font := SQLMemo.Font;
  EnableQueryAbort := AbortQueryBtn.Checked;
end;

procedure TOptionsDialog.SetOptions;
begin
  AutoActivateBtn.Checked := AutoActivate;
  SQLCountBtn.Checked := SQLCalcCount;
  ShowSystemBtn.Checked := SystemTables;
  DelimitedBtn.Checked := ASCIIDelimited;
  FixedBtn.Checked := not ASCIIDelimited;
  KeepConnectBtn.Checked := Session.KeepConnections;
  LiveQueryBtn.Checked := LiveQueries;
  ShowTimeBtn.Checked := ShowExecTime;
  AbortQueryBtn.Checked := EnableQueryAbort;
  CharsetEdit.Value := ASCIICharSet;
  FloatFormatEdit.Text := defFloatFormat;
  DateFormatEdit.Text := defDateFormat;
  TimeFormatEdit.Text := defTimeFormat;
  DateTimeFormatEdit.Text := defDateTimeFormat;
  MaxHistoryEdit.Value := SQLHistoryCapacity;
  if (Application.MainForm <> nil) then
    SQLMemo.Font := TDBExplorerMainForm(Application.MainForm).SQLFontContainer.Font;
end;

procedure TOptionsDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

procedure TOptionsDialog.ApplyBtnClick(Sender: TObject);
var
  Ini: TIniFile;
begin
  if ApplyBtn.Enabled then begin
    ApplyOptions;
    if (Application.MainForm <> nil) then begin
      TDBExplorerMainForm(Application.MainForm).ApplyOptions;
      Ini := TIniFile.Create(
        TDBExplorerMainForm(Application.MainForm).FormPlacement.IniFileName);
      try
        SaveOptions(Ini);
      finally
        Ini.Free;
      end;
    end;
    ApplyBtn.Enabled := False;
  end;
end;

procedure TOptionsDialog.FormShow(Sender: TObject);
begin
  SetOptions;
  ApplyBtn.Enabled := False;
end;

procedure TOptionsDialog.OptionsChanged(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
end;

procedure TOptionsDialog.FormCreate(Sender: TObject);
begin
  StartWait;
  try
    LangDrivList.Open;
    Notebook.PageIndex := 0;
{$IFNDEF WIN32}
    AbortQueryBtn.Enabled := False;
{$ENDIF}
  finally
    StopWait;
  end;
end;

procedure TOptionsDialog.SQLFontBtnClick(Sender: TObject);
begin
  with FontDialog do begin
    Font := SQLMemo.Font;
    if Execute then begin
      SQLMemo.Font := Font;
      OptionsChanged(Self);
    end;
  end;
end;

procedure TOptionsDialog.LangDrivListAfterOpen(DataSet: TDataset);
begin
  LangDrivList.FieldByName('DESCRIPTION').DisplayWidth := 22;
end;

end.
