unit F_Klaw;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls;

type
  TfmKlaw = class(TForm)
    btn1: TSpeedButton;
    btn2: TSpeedButton;
    btn3: TSpeedButton;
    btn4: TSpeedButton;
    btn5: TSpeedButton;
    btn6: TSpeedButton;
    btn7: TSpeedButton;
    btn8: TSpeedButton;
    btnTyl: TSpeedButton;
    btn9: TSpeedButton;
    btn0: TSpeedButton;
    btnMinus: TSpeedButton;
    btnRowny: TSpeedButton;
    btnW: TSpeedButton;
    btnE: TSpeedButton;
    btnR: TSpeedButton;
    btnT: TSpeedButton;
    btnY: TSpeedButton;
    btnU: TSpeedButton;
    btnI: TSpeedButton;
    btnO: TSpeedButton;
    btnQ: TSpeedButton;
    btnP: TSpeedButton;
    btnNawKwa1: TSpeedButton;
    btnNawKwa2: TSpeedButton;
    btnApostr: TSpeedButton;
    btnA: TSpeedButton;
    btnS: TSpeedButton;
    btnD: TSpeedButton;
    btnF: TSpeedButton;
    btnG: TSpeedButton;
    btnH: TSpeedButton;
    btnJ: TSpeedButton;
    btnK: TSpeedButton;
    btnSred: TSpeedButton;
    btnPrzec: TSpeedButton;
    btnKrop: TSpeedButton;
    btnCudz: TSpeedButton;
    btnL: TSpeedButton;
    btnZ: TSpeedButton;
    btnX: TSpeedButton;
    btnC: TSpeedButton;
    btnV: TSpeedButton;
    btnB: TSpeedButton;
    btnN: TSpeedButton;
    btnM: TSpeedButton;
    btnMnie: TSpeedButton;
    btnWiek: TSpeedButton;
    btnPytaj: TSpeedButton;
    btnSpac: TSpeedButton;
    btnBack: TSpeedButton;
    btnCaps: TSpeedButton;
    btnShift: TSpeedButton;
    btnCLear: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnShiftClick(Sender: TObject);
    procedure btnTylClick(Sender: TObject);
    procedure btnSpacClick(Sender: TObject);
    procedure btnCapsClick(Sender: TObject);
    procedure btnCLearClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  private
    fEdit: TCustomEdit;
    { Private declarations }
    procedure Zmien;
  public
    { Public declarations }
    procedure SetEdit(AEdit: TCustomEdit);
    property Edit: TCustomEdit read fEdit;
  end;


implementation

{$R *.dfm}


type
  TKeyShift =  (ksNo, ksShift);

var
  g_key_shift: TKeyShift;
  g_key_caps: TKeyShift;

  g_tab_key: array[0..48] of record
    key: array[TKeyShift] of string;
    btn: TSpeedButton;
  end;

procedure TfmKlaw.btnBackClick(Sender: TObject);
begin
  fEdit.Text:= Copy(fEdit.Text, 1, Length(fEdit.Text) - 1);
end;

procedure TfmKlaw.btnCapsClick(Sender: TObject);
begin
  if g_key_caps = ksNo then
    g_key_caps:= ksShift
  else
    g_key_caps:= ksNo;

  Zmien;
end;

procedure TfmKlaw.btnCLearClick(Sender: TObject);
begin
  fEdit.Clear;
end;

procedure TfmKlaw.btnShiftClick(Sender: TObject);
begin
  g_key_shift:= ksShift;
  Zmien;
end;

procedure TfmKlaw.Zmien;
var
  i: Integer;
  save: TKeyShift;
begin
  case g_key_shift of
    ksNo: btnShift.Font.Color:= clBlack;
    ksShift: btnShift.Font.Color:= clRed;
  end;
  case g_key_caps of
    ksNo: btnCaps.Font.Color:= clBlack;
    ksShift: btnCaps.Font.Color:= clRed;
  end;

  save:= g_key_shift;
  try
    if g_key_caps = ksShift then g_key_shift:= g_key_caps;
    for i := 0 to 48 do
    with g_tab_key[i] do
    begin
      if btn <> nil then
        btn.Caption := key[g_key_shift];
    end;
  finally
    g_key_shift:= save;
  end;
end;

procedure TfmKlaw.btnSpacClick(Sender: TObject);
begin
  fEdit.Text:= fEdit.Text + ' ';
end;

procedure TfmKlaw.btnTylClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Caption <> '' then
    fEdit.Text:= fEdit.Text + TSpeedButton(Sender).Caption;

  if g_key_shift <> ksNo then
  begin
    g_key_shift:= ksNo;
    Zmien;
  end;
end;

procedure TfmKlaw.FormCreate(Sender: TObject);
begin
  with g_tab_key[0] do
  begin
    key[ksNo]:= '`';
    key[ksShift]:= '~';
    btn:= btnTyl;
  end;
  with g_tab_key[1] do
  begin
    key[ksNo]:= '1';
    key[ksShift]:= '!';
    btn:= btn1;
  end;
  with g_tab_key[2] do
  begin
    key[ksNo]:= '2';
    key[ksShift]:= '@';
    btn:= btn2;
  end;
  with g_tab_key[3] do
  begin
    key[ksNo]:= '3';
    key[ksShift]:= '#';
    btn:= btn3;
  end;
  with g_tab_key[4] do
  begin
    key[ksNo]:= '4';
    key[ksShift]:= '$';
    btn:= btn4;
  end;
  with g_tab_key[5] do
  begin
    key[ksNo]:= '5';
    key[ksShift]:= '%';
    btn:= btn5;
  end;
  with g_tab_key[6] do
  begin
    key[ksNo]:= '6';
    key[ksShift]:= '^';
    btn:= btn6;
  end;
  with g_tab_key[7] do
  begin
    key[ksNo]:= '7';
    key[ksShift]:= '&';
    btn:= btn7;
  end;
  with g_tab_key[8] do
  begin
    key[ksNo]:= '8';
    key[ksShift]:= '*';
    btn:= btn8;
  end;
  with g_tab_key[9] do
  begin
    key[ksNo]:= '9';
    key[ksShift]:= '(';
    btn:= btn9;
  end;
  with g_tab_key[10] do
  begin
    key[ksNo]:= '0';
    key[ksShift]:= ')';
    btn:= btn0;
  end;
  with g_tab_key[11] do
  begin
    key[ksNo]:= '-';
    key[ksShift]:= '_';
    btn:= btnMinus;
  end;
  with g_tab_key[12] do
  begin
    key[ksNo]:= '=';
    key[ksShift]:= '+';
    btn:= btnRowny;
  end;
//------------------------------------------------------------------------------  
  with g_tab_key[13] do
  begin
    key[ksNo]:= 'q';
    key[ksShift]:= 'Q';
    btn:= btnQ;
  end;
  with g_tab_key[14] do
  begin
    key[ksNo]:= 'w';
    key[ksShift]:= 'W';
    btn:= btnW;
  end;
  with g_tab_key[15] do
  begin
    key[ksNo]:= 'e';
    key[ksShift]:= 'E';
    btn:= btnE;
  end;
  with g_tab_key[16] do
  begin
    key[ksNo]:= 'r';
    key[ksShift]:= 'R';
    btn:= btnR;
  end;
  with g_tab_key[17] do
  begin
    key[ksNo]:= 't';
    key[ksShift]:= 'T';
    btn:= btnT;
  end;
  with g_tab_key[18] do
  begin
    key[ksNo]:= 'y';
    key[ksShift]:= 'Y';
    btn:= btnY;
  end;
  with g_tab_key[19] do
  begin
    key[ksNo]:= 'u';
    key[ksShift]:= 'U';
    btn:= btnU;
  end;
  with g_tab_key[20] do
  begin
    key[ksNo]:= 'i';
    key[ksShift]:= 'I';
    btn:= btnI;
  end;
  with g_tab_key[21] do
  begin
    key[ksNo]:= 'o';
    key[ksShift]:= 'O';
    btn:= btnO;
  end;
  with g_tab_key[22] do
  begin
    key[ksNo]:= 'p';
    key[ksShift]:= 'P';
    btn:= btnP;
  end;
  with g_tab_key[23] do
  begin
    key[ksNo]:= '[';
    key[ksShift]:= '{';
    btn:= btnNawKwa1;
  end;
  with g_tab_key[24] do
  begin
    key[ksNo]:= ']';
    key[ksShift]:= '}';
    btn:= btnNawKwa2;
  end;
  with g_tab_key[25] do
  begin
    key[ksNo]:= #39;
    key[ksShift]:= '\';
    btn:= btnApostr;
  end;
//------------------------------------------------------------------------------
  with g_tab_key[26] do
  begin
    key[ksNo]:= 'a';
    key[ksShift]:= 'A';
    btn:= btnA;
  end;
  with g_tab_key[27] do
  begin
    key[ksNo]:= 's';
    key[ksShift]:= 'S';
    btn:= btnS;
  end;
  with g_tab_key[28] do
  begin
    key[ksNo]:= 'd';
    key[ksShift]:= 'D';
    btn:= btnD;
  end;
  with g_tab_key[29] do
  begin
    key[ksNo]:= 'f';
    key[ksShift]:= 'F';
    btn:= btnF;
  end;
  with g_tab_key[30] do
  begin
    key[ksNo]:= 'g';
    key[ksShift]:= 'G';
    btn:= btnG;
  end;
  with g_tab_key[31] do
  begin
    key[ksNo]:= 'h';
    key[ksShift]:= 'H';
    btn:= btnH;
  end;
  with g_tab_key[32] do
  begin
    key[ksNo]:= 'j';
    key[ksShift]:= 'J';
    btn:= btnJ;
  end;
  with g_tab_key[33] do
  begin
    key[ksNo]:= 'k';
    key[ksShift]:= 'K';
    btn:= btnK;
  end;
  with g_tab_key[34] do
  begin
    key[ksNo]:= 'l';
    key[ksShift]:= 'L';
    btn:= btnL;
  end;
  with g_tab_key[35] do
  begin
    key[ksNo]:= ';';
    key[ksShift]:= ':';
    btn:= btnSred;
  end;
  with g_tab_key[36] do
  begin
    key[ksNo]:= ',';
    key[ksShift]:= '';
    btn:= btnPrzec;
  end;
  with g_tab_key[37] do
  begin
    key[ksNo]:= '.';
    key[ksShift]:= '';
    btn:= btnKrop;
  end;
  with g_tab_key[38] do
  begin
    key[ksNo]:= '"';
    key[ksShift]:= '';
    btn:= btnCudz;
  end;
//------------------------------------------------------------------------------
  with g_tab_key[39] do
  begin
    key[ksNo]:= 'z';
    key[ksShift]:= 'Z';
    btn:= btnZ;
  end;
  with g_tab_key[40] do
  begin
    key[ksNo]:= 'x';
    key[ksShift]:= 'X';
    btn:= btnX;
  end;
  with g_tab_key[41] do
  begin
    key[ksNo]:= 'c';
    key[ksShift]:= 'C';
    btn:= btnC;
  end;
  with g_tab_key[42] do
  begin
    key[ksNo]:= 'v';
    key[ksShift]:= 'V';
    btn:= btnV;
  end;
  with g_tab_key[43] do
  begin
    key[ksNo]:= 'b';
    key[ksShift]:= 'B';
    btn:= btnB;
  end;
  with g_tab_key[44] do
  begin
    key[ksNo]:= 'n';
    key[ksShift]:= 'N';
    btn:= btnN;
  end;
  with g_tab_key[45] do
  begin
    key[ksNo]:= 'm';
    key[ksShift]:= 'M';
    btn:= btnM;
  end;
  with g_tab_key[46] do
  begin
    key[ksNo]:= '<';
    key[ksShift]:= '';
    btn:= btnMnie;
  end;
  with g_tab_key[47] do
  begin
    key[ksNo]:= '>';
    key[ksShift]:= '';
    btn:= btnWiek;
  end;
  with g_tab_key[48] do
  begin
    key[ksNo]:= '?';
    key[ksShift]:= '/';
    btn:= btnPytaj;
  end;

  Zmien;
end;

procedure TfmKlaw.SetEdit(AEdit: TCustomEdit);
begin
  fEdit:= AEdit;
end;

end.
