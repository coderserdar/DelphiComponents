unit Demo7Frm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVEdit, ExtCtrls, StdCtrls;

type
  TfrmDemo7 = class(TForm)
    Panel1: TPanel;
    rve: TRichViewEdit;
    txt: TEdit;
    btnMark: TButton;
    btnUnmark: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnMarkClick(Sender: TObject);
    procedure btnUnmarkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses MainFrm;

{$R *.DFM}

procedure TfrmDemo7.FormCreate(Sender: TObject);
begin
  rve.Clear;
  rve.LoadText(ExtractFilePath(Application.ExeName)+'MainFrm.pas',0,0, False);
  rve.Format;
end;

procedure TfrmDemo7.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;

procedure TfrmDemo7.btnMarkClick(Sender: TObject);
begin
  //moving caret to the beginning of document
  // (this call is correct only if the 0-th item is text item)
  rve.SetSelectionBounds(0,1,0,1);
  while rve.SearchText(txt.Text, [rvseoDown]) do
    rve.ApplyTextStyle(sncomMarked);
// Note: every call of ApplyTextStyle reformats affected paragraphs
end;

procedure TfrmDemo7.btnUnmarkClick(Sender: TObject);
begin
  rve.SelectAll;
  rve.ApplyTextStyle(sncomNormal);
  rve.Deselect;
  rve.Refresh;  
end;

end.
