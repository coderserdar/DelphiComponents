unit TabSetUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnTabSet;

type
  TFormTabSet = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTabSet: TCnTabSet;
    procedure OnCloseTab(Sender: TObject; Index: Integer;
      var CanClose: Boolean);
    procedure OnTabHint(Sender: TObject; Index: Integer; var HintStr: string);
  public
    { Public declarations }
  end;

var
  FormTabSet: TFormTabSet;

implementation

{$R *.DFM}

procedure TFormTabSet.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FTabSet := TCnTabSet.Create(Self);
  FTabSet.Parent := Self;
  FTabSet.Left := 40;
  FTabSet.Top := 40;
  FTabSet.Width := 400;
  FTabSet.ShowHint := True;

  FTabSet.DblClickClose := True;
  FTabSet.OnCloseTab := OnCloseTab;
  FTabSet.ShowTabHint := True;
  FTabSet.OnTabHint := OnTabHint;

  for I := 0 to 10 do
    FTabSet.Tabs.Add('Tab ' + IntToStr(I));
end;

procedure TFormTabSet.FormDestroy(Sender: TObject);
begin
  FTabSet.Free;
end;

procedure TFormTabSet.OnCloseTab(Sender: TObject; Index: Integer;
  var CanClose: Boolean);
begin
  ShowMessage('To Close Tab, Index: ' + IntToStr(Index));
end;

procedure TFormTabSet.OnTabHint(Sender: TObject; Index: Integer;
  var HintStr: string);
begin
  HintStr := 'Hint String for Tab ' + IntToStr(Index);
end;

end.
