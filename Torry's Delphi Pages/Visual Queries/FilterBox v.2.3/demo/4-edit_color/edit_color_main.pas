unit edit_color_main;

interface
{$I psc_defines.inc}
uses
  {$IFDEF D7}
  Variants, XPMan,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, psc_procs, psc_edit, psc_edit_color, ExtCtrls, StdCtrls, psc_colorbox;

type
  TForm1 = class(TForm)
    PSCColorEdit1: TPSCColorEdit;
    Panel1: TPanel;
    cbTrackColor: TCheckBox;
    cbDisplayNames: TCheckBox;
    cbEnabled: TCheckBox;
    cbHighlightActive: TCheckBox;
    Label1: TLabel;
    cbShowDefaultAuto: TCheckBox;
    cbShowDefaultNone: TCheckBox;
    cbShowCustomColors: TCheckBox;
    cbShowStdColors: TCheckBox;
    cbShowDocColors: TCheckBox;
    cbShowWinColors: TCheckBox;
    cbShowBkgndColors: TCheckBox;
    cbShowFontColors: TCheckBox;
    cbShowMoreColors: TCheckBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Shape1: TShape;
    Label4: TLabel;
    PSCColorEdit2: TPSCColorEdit;
    procedure cbTrackColorClick(Sender: TObject);
    procedure cbDisplayNamesClick(Sender: TObject);
    procedure cbEnabledClick(Sender: TObject);
    procedure cbHighlightActiveClick(Sender: TObject);
    procedure cbShowDefaultAutoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure PSCColorEdit1Change(Sender: TObject);
    procedure PSCColorEdit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cbTrackColorClick(Sender: TObject);
begin
  PSCColorEdit1.TrackColor:= cbTrackColor.Checked;
end;

procedure TForm1.cbDisplayNamesClick(Sender: TObject);
begin
  PSCColorEdit1.DisplayNames:= cbDisplayNames.Checked;
end;

procedure TForm1.cbEnabledClick(Sender: TObject);
begin
  PSCColorEdit1.Enabled:= cbEnabled.Checked;
end;

procedure TForm1.cbHighlightActiveClick(Sender: TObject);
begin
  PSCColorEdit1.HighlightActive:= cbHighlightActive.Checked;
  PSCColorEdit2.Enabled:= cbHighlightActive.Checked;
  PSCColorEdit1.HighlightColor:= PSCColorEdit2.SelectedColor;
end;

procedure AdjustOptions;
begin
  with Form1, PSCColorEdit1 do
  begin
    if cbShowDefaultAuto.Checked then
      Options:= Options + [cboShowDefaultAuto]
    else
      Options:= Options - [cboShowDefaultAuto];
    if cbShowDefaultNone.Checked then
      Options:= Options + [cboShowDefaultNone]
    else
      Options:= Options - [cboShowDefaultNone];
    if cbShowCustomColors.Checked then
      Options:= Options + [cboShowCustomColors]
    else
      Options:= Options - [cboShowCustomColors];
    if cbShowStdColors.Checked then
      Options:= Options + [cboShowStdColors]
    else
      Options:= Options - [cboShowStdColors];
    if cbShowDocColors.Checked then
      Options:= Options + [cboShowDocColors]
    else
      Options:= Options - [cboShowDocColors];
    if cbShowWinColors.Checked then
      Options:= Options + [cboShowWinColors]
    else
      Options:= Options - [cboShowWinColors];
    if cbShowBkgndColors.Checked then
      Options:= Options + [cboShowBkgndColors]
    else
      Options:= Options - [cboShowBkgndColors];
    if cbShowFontColors.Checked then
      Options:= Options + [cboShowFontColors]
    else
      Options:= Options - [cboShowFontColors];
    if cbShowMoreColors.Checked then
      Options:= Options + [cboShowMoreColors]
    else
      Options:= Options - [cboShowMoreColors];
  end;
end;

var
  RespondOnClick: Boolean;

procedure AdjustOptionsCheckBoxes;
begin
  with Form1, PSCColorEdit1 do
  begin
    RespondOnClick:= False;
    try
      cbShowDefaultAuto.Checked:= cboShowDefaultAuto in Options;
      cbShowDefaultNone.Checked:= cboShowDefaultNone in Options;
      cbShowCustomColors.Checked:= cboShowCustomColors in Options;
      cbShowStdColors.Checked:= cboShowStdColors in Options;
      cbShowDocColors.Checked:= cboShowDocColors in Options;
      cbShowWinColors.Checked:= cboShowWinColors in Options;
      cbShowBkgndColors.Checked:= cboShowBkgndColors in Options;
      cbShowFontColors.Checked:= cboShowFontColors in Options;
      cbShowMoreColors.Checked:= cboShowMoreColors in Options;
    finally
      RespondOnClick:= True;
    end;
  end;
end;

procedure AdjustStyle;
begin
  RespondOnClick:= False;
  try
    case Form1.ComboBox1.ItemIndex of
      0: Form1.PSCColorEdit1.Style:= cbsCustom;
      1: Form1.PSCColorEdit1.Style:= cbsWordBk;
      2: Form1.PSCColorEdit1.Style:= cbsWordFont;
      3: Form1.PSCColorEdit1.Style:= cbsFrontPage;
      4: Form1.PSCColorEdit1.Style:= cbsSysColors;
      5: Form1.PSCColorEdit1.Style:= cbsFrontPageBtn;
    end;
    AdjustOptionsCheckBoxes;
  finally
    RespondOnClick:= True;
  end;
end;

procedure AdjustComboBox;
begin
  case Form1.PSCColorEdit1.Style of
    cbsCustom: Form1.ComboBox1.ItemIndex:= 0;
    cbsWordBk: Form1.ComboBox1.ItemIndex:= 1;
    cbsWordFont: Form1.ComboBox1.ItemIndex:= 2;
    cbsFrontPage: Form1.ComboBox1.ItemIndex:= 3;
    cbsSysColors: Form1.ComboBox1.ItemIndex:= 4;
    cbsFrontPageBtn: Form1.ComboBox1.ItemIndex:= 5;
  end;
end;

procedure TForm1.cbShowDefaultAutoClick(Sender: TObject);
begin
  if RespondOnClick then
  begin
    AdjustOptions;
    AdjustComboBox;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RespondOnClick:= false;
  AdjustOptionsCheckBoxes;
  AdjustComboBox;
  RespondOnClick:= true;
  ComboBox2.Text:= PSCColorEdit1.ThemeName;
  Shape1.Brush.Color:= PSCColorEdit1.SelectedColor;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  AdjustStyle;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  PSCColorEdit1.ThemeName:= ComboBox2.Text;
end;

procedure TForm1.PSCColorEdit1Change(Sender: TObject);
begin
  Shape1.Brush.Color:= PSCColorEdit1.SelectedColor;
end;

procedure TForm1.PSCColorEdit2Change(Sender: TObject);
begin
  PSCColorEdit1.HighlightColor:= PSCColorEdit2.SelectedColor;
end;

end.
