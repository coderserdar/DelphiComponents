{$I fsdefine.inc}

unit dgautoin;

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
  TdlgAutoInc = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtSeed: TEdit;
    lblSeed: TLabel;
    edtSeed2: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  protected
    FTableName : string;
    FNewSeed : Int64;
    fNewStep: Longint;                                           {!!.10}
  public
    property NewSeed : Int64 read FNewSeed write FNewSeed;      {!!.10}
    property NewStep : Longint read FNewStep write FNewStep;
    property TableName : string read FTableName write FTableName;
    
  end;

function ShowAutoIncDlg(const aTableName : string;
                          var aNewSeed: Int64; Var aStep: Longint): TModalResult;   {!!.10}


var
  dlgAutoInc: TdlgAutoInc;

implementation

{$R *.DFM}

function ShowAutoIncDlg(const aTableName : string;
                          var aNewSeed: int64; Var aStep: Longint): TModalResult;   {!!.10}
begin
  with TdlgAutoInc.Create(nil) do
  try
    FTableName := aTableName;
    NewSeed := aNewSeed;
    NewStep:= aStep;
    Result := ShowModal;
    if Result = mrOK then
    begin
      aNewSeed := NewSeed;
      aStep:= NewStep;
      end;
  finally
    Free;
  end;
end;

procedure TdlgAutoInc.FormShow(Sender: TObject);
begin
  Caption := Format(Caption, [FTableName]);
  edtSeed.Text := intToStr(FNewSeed);
  edtSeed2.Text := intToStr(FNewStep);
end;

procedure TdlgAutoInc.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Value : int64;
  Value2: Longint;                                               {!!.10}
  Code, c : Integer;
begin
  Val(edtSeed.Text, Value, Code);
  Val(edtSeed2.Text, Value2, C);
  NewSeed := Value;
  NewStep:= Value2;
  CanClose := ((Code = 0) and (c = 0)) or (ModalResult <> mrOK);
  if not CanClose then begin
    MessageBeep(0);
    MessageDlg('A valid seed must be entered.', mtWarning, [mbOK], 0);
  end;
end;

end.
