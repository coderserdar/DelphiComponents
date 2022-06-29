unit SegCfg;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CompLHA;

type
  TCfgForm = class(TForm)
    Label1: TLabel;
    ESegBaseName: TEdit;
    Label2: TLabel;
    ESegDir: TEdit;
    Label3: TLabel;
    ESegSize: TEdit;
    Label4: TLabel;
    ESegTmpDir: TEdit;
    Label5: TLabel;
    CompMethod: TComboBox;
    CanBtn: TBitBtn;
    ApplyBtn: TBitBtn;
    Label6: TLabel;
    ESegExt: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure CanBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CfgForm: TCfgForm;

implementation

uses SegMain;

{$R *.DFM}


procedure TCfgForm.FormCreate(Sender: TObject);
begin
     with SegForm.SegLHA do
       begin
       ESegBaseName.Text := SegBaseName;
       ESegExt.Text := SegExt;
       ESegDir.Text := SegDir;
       ESegSize.Text := IntToStr(SegSize);
       ESegTmpDir.Text := TempArchivePath;
       CompMethod.ItemIndex := Integer(CompressionMethod);
       end;
end;

procedure TCfgForm.CanBtnClick(Sender: TObject);
begin
     Close;
end;

procedure TCfgForm.ApplyBtnClick(Sender: TObject);
begin
     with SegForm.SegLHA do
       begin
       SegBaseName := ESegBaseName.Text;
       SegExt := ESegExt.Text;
       SegDir := ESegDir.Text;
       SegSize := StrToInt(ESegSize.Text);
       TempArchivePath := ESegTmpDir.Text;
       {SegTmpDir := ESegTmpDir.Text;}
       CompressionMethod := TCompLHAMethod(CompMethod.ItemIndex);
       end;
     Close;
end;

end.
