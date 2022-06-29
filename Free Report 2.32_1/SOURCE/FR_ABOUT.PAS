
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              About window               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_About;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, FR_Const;

type
  TfrAboutForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frAboutForm: TfrAboutForm;

implementation

{$R *.DFM}

procedure TfrAboutForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 540);
  Button1.Caption := LoadStr(SOk);
//  Label2.Caption := Label2.Caption + ' build 2';
end;

end.

