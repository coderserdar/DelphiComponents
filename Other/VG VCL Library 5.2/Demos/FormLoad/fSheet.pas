unit fSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  vgCtrls, Menus, ExtCtrls;

type
  TSheetForm = class(TCustomSheetForm)
    mmMain: TMainMenu;
    miSheet: TMenuItem;
    miClose: TMenuItem;
    paSheet: TPanel;
    paSpeedBar: TPanel;
    procedure miCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SheetForm: TSheetForm;

implementation
uses vgVCLUtl;

{$R *.DFM}

type
  TControlHack = class(TControl);

procedure TSheetForm.FormCreate(Sender: TObject);
const
  Count: Integer = 0;
  procedure FormatCaption(Control: TControl);
  begin
    TControlHack(Control).Caption := Format('%s %d', [TControlHack(Control).Caption, Count]);
  end;
begin
  FormatCaption(Self);
  FormatCaption(paSpeedBar);
  FormatCaption(paSheet);
  miSheet.Caption := Format('%s %d', [miSheet.Caption, Count]);  
  Inc(Count);
end;

procedure TSheetForm.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSheetForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
 