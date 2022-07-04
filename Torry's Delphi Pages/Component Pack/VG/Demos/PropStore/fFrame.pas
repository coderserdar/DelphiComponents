unit fFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, vgStndrt;

type
  TStoredFrame = class(TFrame)
    psFrame: TPropStorage;
    cmLoad: TButton;
    cmSave: TButton;
    me: TMemo;
    cmFont: TButton;
    fnFont: TFontDialog;
    procedure cmLoadClick(Sender: TObject);
    procedure cmSaveClick(Sender: TObject);
    procedure cmFontClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses dData;

{$R *.DFM}

procedure TStoredFrame.cmLoadClick(Sender: TObject);
begin
//  psFrame.StoredProps.Text := 'me.Lines';
  psFrame.Load;
end;

procedure TStoredFrame.cmSaveClick(Sender: TObject);
begin
//  psFrame.StoredProps.Text := 'me.Lines';
  psFrame.Save;
end;

procedure TStoredFrame.cmFontClick(Sender: TObject);
begin
  fnFont.Font := me.Font;
  if fnFont.Execute then
    me.Font := fnFont.Font;
end;

end.
