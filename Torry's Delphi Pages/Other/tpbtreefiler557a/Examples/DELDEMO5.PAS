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

unit Deldemo5;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons;

type
  TStatForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StatForm: TStatForm;

implementation

{$R *.DFM}

end.
