{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V+,W-,X+,Y+}
{$I BTDefine.Inc}
{$IFNDEF Win32}
 {$M 16384,8192}
{$ENDIF}
{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

unit Deldemo2;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  TNetTypeDlg = class(TForm)
    OKBtn: TBitBtn;
    Bevel1: TBevel;
    NoNetBtn: TRadioButton;
    MsNetBtn: TRadioButton;
    NovellBtn: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NetTypeDlg: TNetTypeDlg;

implementation

{$R *.DFM}

procedure TNetTypeDlg.FormCreate(Sender: TObject);
begin
{$IFNDEF Novell}
  NovellBtn.Enabled := False;
{$ENDIF}
end;

end.
