unit fmAboutBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    BitBtn1: TBitBtn;
    Image1: TImage;
    Label8: TLabel;
    Bevel1: TBevel;
    lWeb2: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lWeb: TLabel;
    Label7: TLabel;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lWebClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  AboutBox: TAboutBox;

implementation
uses ShellAPI;
{$R *.DFM}

procedure TAboutBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if fsUnderline in lWeb.Font.Style then
    lWeb.Font.Style := lWeb.Font.Style - [fsUnderline];
  if fsUnderline in lWeb2.Font.Style then
    lWeb2.Font.Style := lWeb2.Font.Style - [fsUnderline];
end;

procedure TAboutBox.lWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with Sender as TLabel do
    begin
     if not(fsUnderline in Font.Style) then
      Font.Style := Font.Style + [fsUnderline]
    end;
end;

procedure TAboutBox.lWebClick(Sender: TObject);
begin
  with Sender as TLabel do
    ShellExecute(Self.Handle,
                 nil,
                 PChar(Caption),
                 nil,
                 nil,
                 SW_SHOWNORMAL);
end;

end.
