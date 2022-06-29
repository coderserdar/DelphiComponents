{**********************************************************}
{                                                          }
{  Report Designer                                         }
{  Devrace Extension Library example of                    }
{  TELDesigner, TELDesignPanel                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit dlgReportPropsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, QuickRpt, QRPrntr, Printers;

type
  TdlgReportProps = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    ComboBox2: TComboBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label4: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    Edit6: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Edit7: TEdit;
    Label8: TLabel;
    Edit8: TEdit;
    UpDown1: TUpDown;
    GroupBox3: TGroupBox;
    ComboBox3: TComboBox;
    Label9: TLabel;
    FontDialog1: TFontDialog;
    GroupBox4: TGroupBox;
    Panel1: TPanel;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FReport: TQuickRep;
    procedure UpdateValues;
    procedure VerifyValues;
    procedure SetValues;
  public
    { Public declarations }
    function Execute(AReport: TQuickRep): Boolean;
  end;

var
  dlgReportProps: TdlgReportProps;

implementation

{$R *.dfm}

{ TdlgReportProps }

function TdlgReportProps.Execute(AReport: TQuickRep): Boolean;
begin
  FReport := AReport;
  UpdateValues;
  Result := ShowModal = mrOk;
end;

procedure TdlgReportProps.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['1'..'9', DecimalSeparator]) then Key := #0;
end;

procedure TdlgReportProps.UpdateValues;
var
  LIndex: Integer;
begin
  LIndex := -1;
  case FReport.Page.PaperSize of
    Default: LIndex := 0;
    A3: LIndex := 1;
    A4: LIndex := 2;
    A4Small: LIndex := 3;
    A5: LIndex := 4;
    B4: LIndex := 5;
    B5: LIndex := 6;
    Custom: LIndex := 7;
  end;
  ComboBox1.ItemIndex := LIndex;
  if LIndex = 7 then
  begin
    Edit1.Text := FloatToStr(FReport.Page.Width);
    Edit2.Text := FloatToStr(FReport.Page.Length);
    Edit1.Enabled := True;
    Edit2.Enabled := True;
    Label1.Enabled := True;
    Label2.Enabled := True;
  end
  else
  begin
    Edit1.Enabled := False;
    Edit2.Enabled := False;
    Edit1.Text := '';
    Edit2.Text := '';
    Label1.Enabled := False;
    Label2.Enabled := False;
  end;
  LIndex := -1;
  case FReport.Page.Orientation of
    poPortrait: LIndex := 0;
    poLandscape: LIndex := 1;
  end;
  ComboBox2.ItemIndex := LIndex;
  Edit3.Text := FloatToStr(FReport.Page.LeftMargin);
  Edit4.Text := FloatToStr(FReport.Page.RightMargin);
  Edit5.Text := FloatToStr(FReport.Page.TopMargin);
  Edit6.Text := FloatToStr(FReport.Page.BottomMargin);
  Edit7.Text := FloatToStr(FReport.Page.ColumnSpace);
  UpDown1.Position := FReport.Page.Columns;
  Panel1.Font.Assign(FReport.Font);
  LIndex := -1;
  case FReport.Units of
    MM: LIndex := 0;
    Inches: LIndex := 1;
    Pixels: LIndex := 2;
    Characters: LIndex := 3;
    Native: LIndex := 4;
  end;
  ComboBox3.ItemIndex := LIndex;
  CheckBox1.Checked := FirstPageHeader in FReport.Options;
  CheckBox2.Checked := LastPageFooter in FReport.Options;
end;

procedure TdlgReportProps.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex = 7 then
  begin
    Edit1.Text := FloatToStr(FReport.Page.Width);
    Edit2.Text := FloatToStr(FReport.Page.Length);
    Edit1.Enabled := True;
    Edit2.Enabled := True;
    Label1.Enabled := True;
    Label2.Enabled := True;
  end
  else
  begin
    Edit1.Text := '';
    Edit2.Text := '';
    Edit1.Enabled := False;
    Edit2.Enabled := False;
    Label1.Enabled := False;
    Label2.Enabled := False;
  end;
end;

procedure TdlgReportProps.Button3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(Panel1.Font);
  if FontDialog1.Execute then
    Panel1.Font.Assign(FontDialog1.Font);
end;

procedure TdlgReportProps.VerifyValues;

  function _CanConvertToFloat(const AStr: string): Boolean;
  begin
    Result := True;
    try
      StrToFloat(AStr);
    except
      Result := False;
    end;
  end;

begin
  if ComboBox1.ItemIndex = -1 then
    raise Exception.Create('You mast chouse paper type');
  if (ComboBox1.ItemIndex = 7) and not _CanConvertToFloat(Edit1.Text) then
    raise Exception.Create('Wrong page width');
  if (ComboBox1.ItemIndex = 7) and not _CanConvertToFloat(Edit2.Text) then
    raise Exception.Create('Wrong page height');
  if ComboBox2.ItemIndex = -1 then
    raise Exception.Create('You mast chouse paper orientation');
  if not _CanConvertToFloat(Edit3.Text) then
    raise Exception.Create('Wrong left margin');
  if not _CanConvertToFloat(Edit4.Text) then
    raise Exception.Create('Wrong right margin');
  if not _CanConvertToFloat(Edit5.Text) then
    raise Exception.Create('Wrong top margin');
  if not _CanConvertToFloat(Edit6.Text) then
    raise Exception.Create('Wrong bottom margin');
  if not _CanConvertToFloat(Edit7.Text) then
    raise Exception.Create('Wrong column space');
  if ComboBox3.ItemIndex = -1 then
    raise Exception.Create('You mast chouse unit type');
end;

procedure TdlgReportProps.Button1Click(Sender: TObject);
begin
  VerifyValues;
  SetValues;
  ModalResult := mrOk;
end;

procedure TdlgReportProps.SetValues;
begin
  case ComboBox1.ItemIndex of
    0: FReport.Page.PaperSize := Default;
    1: FReport.Page.PaperSize := A3;
    2: FReport.Page.PaperSize := A4;
    3: FReport.Page.PaperSize := A4Small;
    4: FReport.Page.PaperSize := A5;
    5: FReport.Page.PaperSize := B4;
    6: FReport.Page.PaperSize := B5;
    7: FReport.Page.PaperSize := Custom;
  end;
  if ComboBox1.ItemIndex = 7 then
  begin
    FReport.Page.Width := StrToFloat(Edit1.Text);
    FReport.Page.Length := StrToFloat(Edit2.Text);
  end;
  case ComboBox2.ItemIndex of
    0: FReport.Page.Orientation := poPortrait;
    1: FReport.Page.Orientation := poLandscape;
  end;
  FReport.Page.LeftMargin := StrToFloat(Edit3.Text);
  FReport.Page.RightMargin := StrToFloat(Edit4.Text);
  FReport.Page.TopMargin := StrToFloat(Edit5.Text);
  FReport.Page.BottomMargin := StrToFloat(Edit6.Text);
  FReport.Page.Columns := UpDown1.Position;
  FReport.Font.Assign(Panel1.Font);
  case ComboBox3.ItemIndex of
    0: FReport.Units := MM;
    1: FReport.Units := Inches;
    2: FReport.Units := Pixels;
    3: FReport.Units := Characters;
    4: FReport.Units := Native;
  end;
  if CheckBox1.Checked then
    FReport.Options := FReport.Options + [FirstPageHeader]
  else
    FReport.Options := FReport.Options - [FirstPageHeader];
  if CheckBox2.Checked then
    FReport.Options := FReport.Options + [LastPageFooter]
  else
    FReport.Options := FReport.Options - [LastPageFooter];
end;

end.


