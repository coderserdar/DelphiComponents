unit fmTextViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls;

type
  TTextViewer = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    RichEdit1: TRichEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Déclarations privées }
    FOldCaption : String;
  public
    { Déclarations publiques }
    FileName : String;
  end;

var
  TextViewer: TTextViewer;

implementation

{$R *.DFM}

procedure TTextViewer.Button2Click(Sender: TObject);
begin
  with FontDialog1 do
    begin
      Font.Assign( RichEdit1.Font );
      if Execute then
        RichEdit1.Font.Assign( Font );
    end;
end;

procedure TTextViewer.Button1Click(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

procedure TTextViewer.FormShow(Sender: TObject);
begin
  FOldCaption := Caption;
  Caption := Format( Caption, [FileName] );
end;

procedure TTextViewer.FormHide(Sender: TObject);
begin
  Caption := FOldCaption;
end;

end.
