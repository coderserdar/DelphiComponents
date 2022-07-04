unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, vgCtrls, Menus, ExtCtrls, StdCtrls;

type
  TMainForm = class(TForm)
    flLoader: TFormLoader;
    paSpeedBar: TPanel;
    mnMain: TMainMenu;
    miFile: TMenuItem;
    miClose: TMenuItem;
    pcSheet: TvgPageControl;
    miLine: TMenuItem;
    miNewSheet: TMenuItem;
    procedure miCloseClick(Sender: TObject);
    procedure miNewSheetClick(Sender: TObject);
    procedure flLoaderGetWorkplace(Sender: TObject; SheetForm: TCustomSheetForm;
      var Control: TWinControl);
    procedure flLoaderInsert(Sender: TObject; SheetForm: TCustomSheetForm);
    procedure pcSheetChange(Sender: TObject);
    procedure flLoaderRemove(Sender: TObject; SheetForm: TCustomSheetForm);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses fSheet;

{$R *.DFM}

procedure TMainForm.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miNewSheetClick(Sender: TObject);
var
  Form: TCustomSheetForm;
begin
  Form := TSheetForm.Create(Application);
  try
    flLoader.AddForm(Form, True);
  except
    Form.Free;
    raise;
  end;
end;

procedure TMainForm.flLoaderInsert(Sender: TObject;
  SheetForm: TCustomSheetForm);
begin
  with TvgTabSheet.Create(Application) do
  try
    Caption := SheetForm.Caption;
    PageControl := pcSheet;
    pcSheet.ActivePage := pcSheet.Pages[pcSheet.PageCount - 1];
  except
    Free;
    raise;
  end;
end;

procedure TMainForm.flLoaderRemove(Sender: TObject;
  SheetForm: TCustomSheetForm);
begin
  pcSheet.Pages[SheetForm.Index].Free;
  pcSheet.ActivePage := pcSheet.FindNextPage(nil, False, True);
end;

procedure TMainForm.flLoaderGetWorkplace(Sender: TObject;
  SheetForm: TCustomSheetForm; var Control: TWinControl);
begin
  Control := pcSheet.Pages[SheetForm.Index];
end;

procedure TMainForm.pcSheetChange(Sender: TObject);
begin
  flLoader.ActiveForm := flLoader.Forms[pcSheet.ActivePage.PageIndex];
end;

end.
