unit agPrnDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, agDialog;

type
  TfmPrinterSetup = class(TForm)
    FormPrintCommonDialog: TFormPrintCommonDialog;
    gbOptions: TGroupBox;
    cbConvertToOEM: TCheckBox;
    cbIgnoreFontStyles: TCheckBox;
    lbEscapeModel: TLabel;
    cbEscapeModel: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses ZRConst, ZREscape;

{$R *.DFM}

procedure TfmPrinterSetup.FormCreate(Sender: TObject);
var
  i : TZREscapeModel;
begin
  cbConvertToOEM.Caption       := LoadStr(szrConvertToOEM);
  cbIgnoreFontStyles.Caption   := LoadStr(szrIgnoreFontStyles);
  lbEscapeModel.Caption        := LoadStr(szrEscapeModel)+':';
  Self.Caption                 := LoadStr(szrPrintDialogCaption);
  with cbEscapeModel.Items do begin
    Clear;
    for i:= Low(TZREscapeModel) to High(TZREscapeModel) do Add(EscapeModelNames[i]);
  end;
  cbEscapeModel.ItemIndex := Integer(emEpson);

  gbOptions.Left := FormPrintCommonDialog.Width;
  ClientWidth := gbOptions.Left + gbOptions.Width + 8;
end;

end.

