unit fmLastOutput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TLastOutput = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    RichEdit1: TRichEdit;
    FontDialog1: TFontDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  procedure ClearLog;
  procedure AddToLog( const msg : String );

var
  LastOutput: TLastOutput;

implementation

{$R *.DFM}

procedure TLastOutput.Button1Click(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

procedure TLastOutput.Button2Click(Sender: TObject);
begin
  with FontDialog1 do
    begin
      Font.Assign( RichEdit1.Font );
      if Execute then
        RichEdit1.Font.Assign( Font );
    end;
end;

procedure ClearLog;
begin
  if Assigned( LastOutput ) then
    LastOutput.RichEdit1.Clear;
end;

procedure AddToLog( const msg : String );
begin
  if Assigned( LastOutput ) then
    LastOutput.RichEdit1.Lines.Add( msg );
end;


end.
