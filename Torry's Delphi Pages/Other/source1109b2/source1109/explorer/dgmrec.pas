{$I fsdefine.inc}

unit dgmrec;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Mask,
  fsllbase;

type
  TdlgMaxRecords = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtSeed: TEdit;
    lblSeed: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  protected
    FTableName : string;
    FNewSeed : Longint;
  public
    property NewSeed : Longint read FNewSeed write FNewSeed;
    property TableName : string read FTableName write FTableName;

  end;

function ShowMaxRecordsDlg(const aTableName : string;
                          var aNewSeed: Longint): TModalResult;


var
  dlgMaxRecords: TdlgMaxRecords;

implementation

{$R *.DFM}

function ShowMaxRecordsDlg(const aTableName : string;
                          var aNewSeed: Longint): TModalResult;   {!!.10}
begin
  with TdlgMaxRecords.Create(nil) do
  try
    FTableName := aTableName;
    NewSeed := aNewSeed;
    Result := ShowModal;
    if Result = mrOK then
    begin
      aNewSeed := NewSeed;
      end;
  finally
    Free;
  end;
end;

procedure TdlgMaxRecords.FormShow(Sender: TObject);
begin
  Caption := Format(Caption, [FTableName]);
  edtSeed.Text := intToStr(FNewSeed);
end;

procedure TdlgMaxRecords.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Value: Longint;                                               {!!.10}
  Code, c : Integer;
begin
  Val(edtSeed.Text, Value, Code);
  NewSeed := Value;
  CanClose := (Code = 0) or (ModalResult <> mrOK);
  if not CanClose then begin
    MessageBeep(0);
    MessageDlg('A valid seed must be entered.', mtWarning, [mbOK], 0);
  end;
end;

end.
