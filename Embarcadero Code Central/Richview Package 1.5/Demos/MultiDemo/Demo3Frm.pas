unit Demo3Frm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RVStyle, RVScroll, RichView,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  ExtCtrls;

type
  TfrmDemo3 = class(TForm)
    PageControl1: TPageControl;
    rv: TRichView;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Page4: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Memo1: TMemo;
    Label4: TLabel;
    il: TImageList;
    procedure Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rvClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure BuildSummary;
  end;


implementation
uses MainFrm;
{$R *.DFM}

procedure TfrmDemo3.BuildSummary;
const OffOn: array [Boolean] of String = ('Off', 'On');
      Checks: array [Boolean] of Integer = (1, 2);
begin
  rv.Clear;
  rv.AddNL('Query', sncomHeading,1);

  rv.AddBulletEx('', 0, il, 0);
  rv.Add('Edit1: ', sncomKeyword);
  rv.Add(Edit1.Text, sncomNormal);

  rv.AddBulletEx('', 0, il, 0);
  rv.Add('Edit2: ', sncomKeyword);
  rv.Add(Edit2.Text, sncomNormal);

  rv.AddBreakEx(1, rvbsLine, clRed);

  if RadioGroup1.ItemIndex<>-1 then begin
    rv.AddBulletEx('', 3, il, 0);
    rv.Add('Menu: ', sncomKeyword);
    rv.Add(RadioGroup1.Items[RadioGroup1.ItemIndex], sncomNormal);
  end;

  rv.AddBreakEx(1, rvbsLine, clRed);

  rv.AddNL('Check1: ', sncomKeyword, 0);
  rv.AddBulletEx('', Checks[Checkbox1.Checked], il, -1);
  rv.Add(OffOn[Checkbox1.Checked], sncomNormal);

  rv.AddNL('Check2: ', sncomKeyword, 0);
  rv.AddBulletEx('', Checks[Checkbox2.Checked], il, -1);
  rv.Add(OffOn[Checkbox2.Checked], sncomNormal);

  rv.AddNL('Check3: ', sncomKeyword, 0);
  rv.AddBulletEx('', Checks[Checkbox3.Checked], il, -1);
  rv.Add(OffOn[Checkbox3.Checked], sncomNormal);

  rv.AddBreakEx(1, rvbsLine, clRed);

  rv.AddBulletEx('', 0, il, 0);
  rv.Add('Memo1:',sncomKeyword);
  rv.AddTextNL(Memo1.Lines.Text,sncomNormal, -1, 0);
  rv.Format;
end;

procedure TfrmDemo3.Change(Sender: TObject);
begin
  BuildSummary;
end;

procedure TfrmDemo3.FormCreate(Sender: TObject);
begin
  BuildSummary;
end;

procedure TfrmDemo3.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;

procedure TfrmDemo3.rvClick(Sender: TObject);
begin
  if not rv.SelectionExists then
    Application.MessageBox('This RichView only for view, do not click it', 'Info',
                         MB_OK or MB_ICONEXCLAMATION);
end;

end.
