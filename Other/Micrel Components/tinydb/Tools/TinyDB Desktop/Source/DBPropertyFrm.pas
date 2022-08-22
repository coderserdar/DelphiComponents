unit DBPropertyFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  TinyDB, BaseFrm;

type
  TDBPropertyForm = class(TBaseForm)
    PageControl: TPageControl;
    MainTabSheet: TTabSheet;
    GroupBox1: TGroupBox;
    TypeLabel: TLabel;
    DateLabel: TLabel;
    SizeLabel: TLabel;
    TableCountLabel: TLabel;
    EncryptLabel: TLabel;
    EncAlgoLabel: TLabel;
    CompressLabel: TLabel;
    CompLevelLabel: TLabel;
    TypeEdit: TEdit;
    DateEdit: TEdit;
    SizeEdit: TEdit;
    TableCountEdit: TEdit;
    EncryptEdit: TEdit;
    EncAlgEdit: TEdit;
    CompressEdit: TEdit;
    CompLevelEdit: TEdit;
    CloseButton: TButton;
    FileNameLabel: TLabel;
    FileNameEdit: TEdit;
    CompAlgoLabel: TLabel;
    CompAlgoEdit: TEdit;
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetData(ATinyDB: TTinyDatabase);
  end;

var
  DBPropertyForm: TDBPropertyForm;

procedure ShowDBPropertyForm(ATinyDB: TTinyDatabase);

implementation

uses Misc, LangMgr;

{$R *.DFM}

procedure ShowDBPropertyForm(ATinyDB: TTinyDatabase);
var
  Frm: TDBPropertyForm;
begin
  Frm := TDBPropertyForm.Create(Application);
  Frm.SetData(ATinyDB);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TDBPropertyForm.SetData(ATinyDB: TTinyDatabase);
var
  CompStr: array[TCompressLevel] of string;
  YesStr, NoStr: string;
begin
  CompStr[clMaximum] := AppLangMgr.Trans('clMaximum');
  CompStr[clNormal] := AppLangMgr.Trans('clNormal');
  CompStr[clFast] := AppLangMgr.Trans('clFast');
  CompStr[clSuperFast] := AppLangMgr.Trans('clSuperFast');
  YesStr := AppLangMgr.Trans('Yes');
  NoStr := AppLangMgr.Trans('No');

  TypeEdit.Text := 'TinyDB';
  FileNameEdit.Text := ATinyDB.FileName;
  TableCountEdit.Text := IntToStr(ATinyDB.TableDefs.Count);
  EncryptEdit.Text := Iif(ATinyDB.Encrypted, YesStr, NoStr);
  EncAlgEdit.Text := Iif(ATinyDB.Encrypted, ATinyDB.EncryptAlgoName, NoStr);
  CompressEdit.Text := Iif(ATinyDB.Compressed, YesStr, NoStr);
  CompAlgoEdit.Text := Iif(ATinyDB.Compressed, ATinyDB.CompressAlgoName, NoStr);
  CompLevelEdit.Text := Iif(ATinyDB.Compressed, CompStr[ATinyDB.CompressLevel], NoStr);
  SizeEdit.Text := AddThoundandFlag(GetFileSize(ATinyDB.FileName)) + ' bytes';
  DateEdit.Text := DateTimeToStr(GetFileDate(ATinyDB.FileName));
end;

procedure TDBPropertyForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.
