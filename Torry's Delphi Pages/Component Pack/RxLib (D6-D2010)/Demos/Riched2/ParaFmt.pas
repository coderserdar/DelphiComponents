unit ParaFmt;

{$I RX.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RxRichEd, ExtCtrls, RXSpin;

type
  TParaFormatDlg = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    IndentBox: TGroupBox;
    LeftIndent: TRxSpinEdit;
    Label1: TLabel;
    RightIndent: TRxSpinEdit;
    Label2: TLabel;
    FirstIndent: TRxSpinEdit;
    Label3: TLabel;
    Alignment: TRadioGroup;
    SpacingBox: TGroupBox;
    Label4: TLabel;
    SpaceBefore: TRxSpinEdit;
    Label5: TLabel;
    SpaceAfter: TRxSpinEdit;
    Label6: TLabel;
    LineSpacing: TRxSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SetAttr(Paragraph: TRxParaAttributes);
    procedure GetAttr(Paragraph: TRxParaAttributes);
  public
    { Public declarations }
  end;

function FormatParagraph(Paragraph: TRxParaAttributes): Boolean;

implementation

{$R *.DFM}

function FormatParagraph(Paragraph: TRxParaAttributes): Boolean;
begin
  with TParaFormatDlg.Create(Application) do
  try
    SetAttr(Paragraph);
    Result := ShowModal = mrOk;
    if Result then GetAttr(Paragraph);
  finally
    Free;
  end;
end;

procedure TParaFormatDlg.SetAttr(Paragraph: TRxParaAttributes);
begin
  LeftIndent.AsInteger := Paragraph.LeftIndent;
  RightIndent.AsInteger := Paragraph.RightIndent;
  FirstIndent.AsInteger := Paragraph.FirstIndent;
  Alignment.ItemIndex := Ord(Paragraph.Alignment);
  SpaceBefore.AsInteger := Paragraph.SpaceBefore;
  SpaceAfter.AsInteger := Paragraph.SpaceAfter;
  LineSpacing.AsInteger := Paragraph.LineSpacing;
end;

procedure TParaFormatDlg.GetAttr(Paragraph: TRxParaAttributes);
begin
  Paragraph.LeftIndent := LeftIndent.AsInteger;
  Paragraph.RightIndent := RightIndent.AsInteger;
  Paragraph.FirstIndent := FirstIndent.AsInteger;
  Paragraph.Alignment := TParaAlignment(Alignment.ItemIndex);
  Paragraph.SpaceBefore := SpaceBefore.AsInteger;
  Paragraph.SpaceAfter := SpaceAfter.AsInteger;
  if LineSpacing.AsInteger > 0 then
    Paragraph.LineSpacingRule := lsSpecifiedOrMore
  else
    Paragraph.LineSpacingRule := lsSingle;
  Paragraph.LineSpacing := LineSpacing.AsInteger;
end;

procedure TParaFormatDlg.FormCreate(Sender: TObject);
begin
  SpacingBox.Enabled := (RichEditVersion >= 2);
end;

end.
