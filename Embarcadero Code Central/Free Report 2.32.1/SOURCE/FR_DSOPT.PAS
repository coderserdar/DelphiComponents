
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Designer options            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_DsOpt;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, FR_Const;

type
  TfrDesOptionsForm = class(TForm)
    PageControl1: TPageControl;
    Tab1: TTabSheet;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    CB2: TCheckBox;
    GroupBox2: TGroupBox;
    RB4: TRadioButton;
    RB5: TRadioButton;
    GroupBox3: TGroupBox;
    RB6: TRadioButton;
    RB7: TRadioButton;
    RB8: TRadioButton;
    GroupBox4: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    GroupBox5: TGroupBox;
    CB3: TCheckBox;
    CB4: TCheckBox;
    CB5: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

procedure TfrDesOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 280);
  Tab1.Caption := LoadStr(frRes + 281);
  GroupBox1.Caption := LoadStr(frRes + 282);
  GroupBox2.Caption := LoadStr(frRes + 283);
  GroupBox3.Caption := LoadStr(frRes + 284);
  GroupBox4.Caption := LoadStr(frRes + 285);
  GroupBox5.Caption := LoadStr(frRes + 297);
  CB1.Caption := LoadStr(frRes + 286);
  CB2.Caption := LoadStr(frRes + 287);
  CB3.Caption := LoadStr(frRes + 288);
  CB4.Caption := LoadStr(frRes + 298);
  CB5.Caption := LoadStr(frRes + 299);
  RB1.Caption := LoadStr(frRes + 289);
  RB2.Caption := LoadStr(frRes + 290);
  RB3.Caption := LoadStr(frRes + 291);
  RB4.Caption := LoadStr(frRes + 292);
  RB5.Caption := LoadStr(frRes + 293);
  RB6.Caption := LoadStr(frRes + 294);
  RB7.Caption := LoadStr(frRes + 295);
  RB8.Caption := LoadStr(frRes + 296);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

end.

