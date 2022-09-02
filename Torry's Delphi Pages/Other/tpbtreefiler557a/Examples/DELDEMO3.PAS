{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V+,W-,X+,Y+}
{$IFNDEF Win32}
 {$M 16384,8192}
{$ENDIF}
{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

unit Deldemo3;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  TEditDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    FName: TEdit;
    LName: TEdit;
    StreetEdit: TEdit;
    CityEdit: TEdit;
    Cntry: TEdit;
    ZipCode: TEdit;
    PhoneEdit: TEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditDlg: TEditDlg;

implementation

{$R *.DFM}

procedure TEditDlg.FormShow(Sender: TObject);
begin
  ActiveControl := FName;
end;

end.
