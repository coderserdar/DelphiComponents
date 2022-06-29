unit UFormProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFormProgress = class(TForm)
    labCaption: TLabel;
    ProgressBar1: TProgressBar;
    btnCancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FFinished: boolean;
  public
    { Public declarations }
    function Progress(const APos, APosMax: Int64): boolean;
  end;

var
  FormProgress: TFormProgress;

implementation

{$R *.dfm}

function TFormProgress.Progress(const APos, APosMax: Int64): boolean;
var
  APercent: Int64;
begin
  Assert(APos >= 0, 'Progress position is negative');
  Assert(APosMax >= 0, 'Progress maximal position is negative');
  Assert(APos <= APosMax, 'Progress position is out of range');

  if APosMax = 0 then
    APercent:= 0
  else
    APercent:= APos * 100 div APosMax;

  ProgressBar1.Position:= APercent;
  labCaption.Caption:= Format('Searching: %d%% (Offset %d of %d)', [APercent, APos, APosMax]);

  Application.ProcessMessages;

  Result:= not FFinished;
end;

procedure TFormProgress.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  //
end;

procedure TFormProgress.FormShow(Sender: TObject);
begin
  FFinished:= False;
end;

procedure TFormProgress.btnCancelClick(Sender: TObject);
begin
  FFinished:= True;
end;

end.
