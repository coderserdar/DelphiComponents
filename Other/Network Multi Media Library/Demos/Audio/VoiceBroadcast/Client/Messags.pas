unit Messags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TfrmMessages = class(TForm)
    RichEdit: TRichEdit;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMessages: TfrmMessages;

implementation

{$R *.dfm}

end.
