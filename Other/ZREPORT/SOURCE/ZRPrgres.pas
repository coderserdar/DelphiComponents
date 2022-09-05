unit ZRPrgres;

{$I ZRDefine.inc}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, ComCtrls, ZRPrntr, Buttons;

type
  TZRProgressForm = class(TForm)
    Gauge : TProgressBar;
    Info  : TLabel;
    Cancel: TBitBtn;
    procedure CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fPrinter : TZRPrinter;
    procedure SetPrinter(Value : TZRPrinter);
    function  GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
  protected
    procedure CMZRProgressUpdate(var Message: TCMZRProgressUpdate); message CM_ZRPROGRESSUPDATE;
  public
    constructor CreateProgress(aOwner: TComponent; aPrinter: TZRPrinter);
    property Printer : TZRPrinter read fPrinter write SetPrinter;
    property Position: Integer read GetPosition write SetPosition;
  end;

implementation

{$R *.DFM}

uses ZRConst;

constructor TZRProgressForm.CreateProgress(aOwner: TComponent; aPrinter: TZRPrinter);
begin
  inherited Create(aOwner);
  Printer := aPrinter;
end;

procedure TZRProgressForm.FormCreate(Sender: TObject);
begin
  Caption       := LoadStr(szrPrinting);
  Cancel.Caption:= LoadStr(szrCancel);
end;

procedure TZRProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TZRProgressForm.FormDestroy(Sender: TObject);
begin
  Printer := nil;
end;

procedure TZRProgressForm.CancelClick(Sender: TObject);
begin
  if Assigned(Printer) then Printer.Cancel;
end;

procedure TZRProgressForm.SetPrinter(Value: TZRPrinter);
begin
  if Printer <> Value then begin
    if Assigned(Printer) then Printer.ProgressForm := nil;
    fPrinter := Value;
    if Assigned(Printer) then Printer.ProgressForm := Self;
  end;
end;

function TZRProgressForm.GetPosition: Integer;
begin
  Result := Gauge.Position;
end;

procedure TZRProgressForm.SetPosition(const Value: Integer);
begin
  Gauge.Position := Value;
  Info .Caption  := IntToStr(Value)+'% ' + LoadStr(szrCompleted);
end;

procedure TZRProgressForm.CMZRProgressUpdate(var Message: TCMZRProgressUpdate);
begin
  Position := Message.Position;
end;

end.
