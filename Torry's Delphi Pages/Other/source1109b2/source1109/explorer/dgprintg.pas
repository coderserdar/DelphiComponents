{$I fsdefine.inc}

unit dgprintg;

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
  ExtCtrls;

type
  TdlgPrinting = class(TForm)
    Bevel1: TBevel;
    lblPrintingCaption: TLabel;
  private
    FCursor: TCursor;
  public
  end;

var
  dlgPrinting: TdlgPrinting;

procedure HidePrintingDlg;

procedure ShowPrintingDlg(const aCaption: string);

implementation

{$R *.DFM}

procedure HidePrintingDlg;
begin
  with dlgPrinting do begin
    Screen.Cursor := FCursor;
    Visible := False;
    dlgPrinting.Free;
    dlgPrinting := nil;
  end;

end;

procedure ShowPrintingDlg(const aCaption: string);
begin
  if not Assigned(dlgPrinting) then
    dlgPrinting := TdlgPrinting.Create(nil);
  with dlgPrinting do begin
    FCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    lblPrintingCaption.Caption := aCaption;
    Visible := True;
  end;
end;


end.
