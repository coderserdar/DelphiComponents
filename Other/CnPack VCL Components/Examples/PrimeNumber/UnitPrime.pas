unit UnitPrime;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TFormPrime = class(TForm)
    pgc1: TPageControl;
    tsGenPrime: TTabSheet;
    btnGen: TButton;
    edtMax: TEdit;
    mmoResult: TMemo;
    tsIsPrime: TTabSheet;
    lblCheck: TLabel;
    edtToPrime: TEdit;
    btnIsPrime: TButton;
    chkQuickGen: TCheckBox;
    lblInt64: TLabel;
    edtInt64: TEdit;
    btnInt64IsPrime: TButton;
    btnCarmichael: TButton;
    mmoCar: TMemo;
    btnGen64: TButton;
    tsMontgomery: TTabSheet;
    edtMonA: TEdit;
    lbl1: TLabel;
    edtMonB: TEdit;
    lblMonMod: TLabel;
    edtMonC: TEdit;
    btnMon: TButton;
    edtMonRes: TEdit;
    btnMonPowerMod64: TButton;
    btn2: TSpeedButton;
    btn65537: TSpeedButton;
    bvl1: TBevel;
    edtMulModA: TEdit;
    lblMul: TLabel;
    edtMulModB: TEdit;
    lblMulMod: TLabel;
    edtMulModC: TEdit;
    edtMulModRes: TEdit;
    btnMulMod: TButton;
    btnMulMod64: TButton;
    bvl2: TBevel;
    edtAddModA: TEdit;
    lblAddMod: TLabel;
    edtAddModB: TEdit;
    edtAddModC: TEdit;
    lbl3: TLabel;
    btnAddMod: TButton;
    btnAddMod64: TButton;
    edtAddModRes: TEdit;
    tsDH: TTabSheet;
    lblInt64DHP: TLabel;
    edtDHPrime: TEdit;
    lblInt64DHRoot: TLabel;
    edtDHRoot: TEdit;
    btnGenInt64DH: TButton;
    btnGenInt32Prime: TButton;
    btnGenInt32DH: TButton;
    lblDHA: TLabel;
    lblXA: TLabel;
    edtDHXa: TEdit;
    edtDHXb: TEdit;
    lblXb: TLabel;
    lblB: TLabel;
    btnCalcXA: TButton;
    btnCalcYb: TButton;
    edtDHYa: TEdit;
    edtDHYb: TEdit;
    btnDHACKey: TButton;
    btnDHBCK: TButton;
    edtAKey: TEdit;
    edtBKey: TEdit;
    btnDHIsRoot32: TButton;
    btnDHIsPrimitiveRoot64: TButton;
    btnDHRand: TButton;
    procedure btnGenClick(Sender: TObject);
    procedure btnIsPrimeClick(Sender: TObject);
    procedure btnInt64IsPrimeClick(Sender: TObject);
    procedure btnCarmichaelClick(Sender: TObject);
    procedure btnGen64Click(Sender: TObject);
    procedure btnMonClick(Sender: TObject);
    procedure btnMonPowerMod64Click(Sender: TObject);
    procedure btn65537Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btnMulMod64Click(Sender: TObject);
    procedure btnMulModClick(Sender: TObject);
    procedure btnAddModClick(Sender: TObject);
    procedure btnAddMod64Click(Sender: TObject);
    procedure btnGenInt64DHClick(Sender: TObject);
    procedure btnGenInt32PrimeClick(Sender: TObject);
    procedure btnGenInt32DHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCalcXAClick(Sender: TObject);
    procedure btnCalcYbClick(Sender: TObject);
    procedure btnDHACKeyClick(Sender: TObject);
    procedure btnDHBCKClick(Sender: TObject);
    procedure btnDHIsRoot32Click(Sender: TObject);
    procedure btnDHIsPrimitiveRoot64Click(Sender: TObject);
    procedure btnDHRandClick(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  FormPrime: TFormPrime;

implementation

uses
  CnPrimeNumber, CnNativeDecl, CnClasses;

{$R *.DFM}

{$I Carmichael.inc}

function IsPrime(N: Cardinal): Boolean;
var
  I: Cardinal;
  Sq: Cardinal;
begin
  Result := False;
  if N < 2 then
    Exit;
  if N = 2 then
  begin
    Result := True;
    Exit;
  end;

  if N mod 2 = 0 then
    Exit;

  Sq := Trunc(Sqrt(N));
  I := 3;
  while I <= Sq do
  begin
    if N mod I = 0 then
      Exit;
    Inc(I, 2);
  end;
  Result := True;
end;

procedure TFormPrime.btnGenClick(Sender: TObject);
var
  M, I: Cardinal;
  F: TFileStream;
  L: TStrings;
  T, C: DWORD;
  S: AnsiString;
begin
  M := StrToInt64(edtMax.Text);
  mmoResult.Clear;
  if chkQuickGen.Checked then
  begin
    T := GetTickCount;
    C := 0;
    F := TFileStream.Create('C:\primes.txt', fmCreate);
    L := TStringList.Create;
    for I := 2 to M do
    begin
      if CnUInt32IsPrime(I) then
      begin
        Inc(C);
        L.Add(IntToStr(I) + ',');
      end;
      if I mod 10000000 = 0 then
      begin
        mmoResult.Lines[0] := FloatToStr(I / M) + ' - ' + IntToStr(GetTickCount - T) + ' - ' + IntToStr(C);
        S := L.Text;
        F.Write(S[1], Length(S));
        L.Clear;
        Application.ProcessMessages;
      end;
    end;
    S := L.Text;
    F.Write(S[1], Length(S));
    mmoResult.Lines.Add('C:\primes.txt Done. ' + ' - ' + IntToStr(GetTickCount - T) + 'ms. - Count: ' + IntToStr(C));
    F.Free;
    L.Free;
  end
  else
    for I := 2 to M do
      if IsPrime(I) then
        mmoResult.Lines.Add(IntToStr(I) + ',');
end;

procedure TFormPrime.btnIsPrimeClick(Sender: TObject);
var
  N: Cardinal;
  F: TCnUInt32List;
  S: string;
  I: Integer;
begin
  N := Cardinal(StrToInt64(edtToPrime.Text));
  if CnUInt32IsPrime(N) then
    ShowMessage('Is Prime Number.')
  else
  begin
    F := TCnUInt32List.Create;
    CnUInt32FindFactors(N, F);
    S := #13#10#13#10;
    for I := 0 to F.Count - 1 do
      S := S + ' ' + IntToStr(F[I]);
    F.Free;
    N := CnEulerUInt32(N);
    S := S + #13#10 + 'Euler: ' + IntToStr(N);
    ShowMessage('Not Prime Number. Factors are:' + S);
  end;
end;

procedure TFormPrime.btnInt64IsPrimeClick(Sender: TObject);
var
  N: TUInt64;
  F: TCnUInt64List;
  S: string;
  I: Integer;
begin
  N := StrToUInt64(edtInt64.Text);
  if CnInt64IsPrime(N) then
    ShowMessage('Is Prime Number.')
  else
  begin
    F := TCnUInt64List.Create;
    CnInt64FindFactors(N, F);
    S := #13#10#13#10;
    for I := 0 to F.Count - 1 do
      S := S + ' ' + UInt64ToStr(F[I]);
    F.Free;
    N := CnEulerInt64(N);
    S := S + #13#10 + 'Euler: ' + UInt64ToStr(N);
    ShowMessage('Not Prime Number. Factors are:' + S);
  end;
end;

procedure TFormPrime.btnCarmichaelClick(Sender: TObject);
var
  I: Integer;
begin
  mmoCar.Clear;
  for I := Low(CN_CARMICHAEL_NUMBERS_GT_UINT32) to High(CN_CARMICHAEL_NUMBERS_GT_UINT32) do
    if CnInt64IsPrime(CN_CARMICHAEL_NUMBERS_GT_UINT32[I]) then
      mmoCar.Lines.Add(IntToStr(CN_CARMICHAEL_NUMBERS_GT_UINT32[I]));
end;

procedure TFormPrime.btnGen64Click(Sender: TObject);
var
  U: TUInt64;
begin
  U := CnGenerateInt64Prime;
  MessageBox(Handle, PChar(UInt64ToStr(U) + ' ($' + IntToHex(U, 2) + ')'), 'Prime', MB_OK);
end;

procedure TFormPrime.btnMonClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtMonA.Text));
  B := StrToUInt64((edtMonB.Text));
  C := StrToUInt64((edtMonC.Text));
  R := MontgomeryPowerMod(A, B, C);
  edtMonRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnMonPowerMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtMonA.Text));
  B := StrToUInt64((edtMonB.Text));
  C := StrToUInt64((edtMonC.Text));
  R := MontgomeryPowerMod64(A, B, C);
  edtMonRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btn65537Click(Sender: TObject);
begin
  edtMonB.Text := '65537';
end;

procedure TFormPrime.btn2Click(Sender: TObject);
begin
  edtMonA.Text := edtMonRes.Text;
end;

procedure TFormPrime.btnMulMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtMulModA.Text));
  B := StrToUInt64((edtMulModB.Text));
  C := StrToUInt64((edtMulModC.Text));
  R := MultipleMod64(A, B, C);
  edtMulModRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btnMulModClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtMulModA.Text));
  B := StrToUInt64((edtMulModB.Text));
  C := StrToUInt64((edtMulModC.Text));
  R := MultipleMod(A, B, C);
  edtMulModRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnAddModClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtAddModA.Text));
  B := StrToUInt64((edtAddModB.Text));
  C := StrToUInt64((edtAddModC.Text));
  R := AddMod(A, B, C);
  edtAddModRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnAddMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtAddModA.Text));
  B := StrToUInt64((edtAddModB.Text));
  C := StrToUInt64((edtAddModC.Text));
  R := AddMod64(A, B, C);
  edtAddModRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btnGenInt64DHClick(Sender: TObject);
var
  P, R: TUInt64;
  //Rs: TCnUInt64List;
begin
  //Rs := TCnUInt64List.Create;
  //CnGenerateInt64DiffieHellmanPrimeRoots(P, Rs);
  CnGenerateInt64DiffieHellmanPrimeRoot(P, R);
  edtDHPrime.Text := UInt64ToStr(P);
  //ShowMessage('Found ' + IntToStr(Rs.Count) + 'Primetive Roots for ' + edtDHPrime.Text);
  edtDHRoot.Text := UInt64ToStr(R);
  //Rs.Free;
end;

procedure TFormPrime.btnGenInt32PrimeClick(Sender: TObject);
var
  U: Cardinal;
begin
  U := CnGenerateUInt32Prime();
  MessageBox(Handle, PChar(IntToStr(U) + ' ($' + IntToHex(U, 2) + ')'), 'Prime', MB_OK);
end;

procedure TFormPrime.btnGenInt32DHClick(Sender: TObject);
var
  P, R: Cardinal;
  //Rs: TCnUInt32List;
begin
  //Rs := TCnUInt32List.Create;
  //CnGenerateUInt32DiffieHellmanPrimeRoots(P, Rs);
  CnGenerateUInt32DiffieHellmanPrimeRoot(P, R);
  edtDHPrime.Text := UInt64ToStr(P);
  //ShowMessage('Found ' + IntToStr(Rs.Count) + 'Primetive Roots for ' + edtDHPrime.Text);
  edtDHRoot.Text := UInt64ToStr(R);
  //Rs.Free;
end;

procedure TFormPrime.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;
end;

procedure TFormPrime.btnCalcXAClick(Sender: TObject);
var
  Xa, Ya, Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);
  Xa := StrToUInt64(edtDHXa.Text);
  Ya := MontgomeryPowerMod(Root, Xa, Prime);
  edtDHYa.Text := UInt64ToStr(Ya);
end;

procedure TFormPrime.btnCalcYbClick(Sender: TObject);
var
  Xb, Yb, Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);
  Xb := StrToUInt64(edtDHXb.Text);
  Yb := MontgomeryPowerMod(Root, Xb, Prime);
  edtDHYb.Text := UInt64ToStr(Yb);
end;

procedure TFormPrime.btnDHACKeyClick(Sender: TObject);
var
  Xa, Yb, Prime, Key: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Xa := StrToUInt64(edtDHXa.Text);
  Yb := StrToUInt64(edtDHYb.Text);
  Key := MontgomeryPowerMod(Yb, Xa, Prime);
  edtAKey.Text := UInt64ToStr(Key);
end;

procedure TFormPrime.btnDHBCKClick(Sender: TObject);
var
  Xb, Ya, Prime, Key: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Xb := StrToUInt64(edtDHXb.Text);
  Ya := StrToUInt64(edtDHYa.Text);
  Key := MontgomeryPowerMod(Ya, Xb, Prime);
  edtBKey.Text := UInt64ToStr(Key);
end;

procedure TFormPrime.btnDHIsRoot32Click(Sender: TObject);
var
  Prime, Root: Cardinal;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);

  if CnIsUInt32PrimitiveRoot(Prime, Root) then
    ShowMessage('Is Primitive Root.')
  else
    ShowMessage('NOT Primitive Root.');
end;

procedure TFormPrime.btnDHIsPrimitiveRoot64Click(Sender: TObject);
var
  Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);

  if CnIsInt64PrimitiveRoot(Prime, Root) then
    ShowMessage('Is Primitive Root.')
  else
    ShowMessage('NOT Primitive Root.');
end;

procedure TFormPrime.btnDHRandClick(Sender: TObject);
var
  Prime: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  edtDHXa.Text := UInt64ToStr(Trunc(Random * Prime));
  edtDHXb.Text := UInt64ToStr(Trunc(Random * Prime));
end;

end.

