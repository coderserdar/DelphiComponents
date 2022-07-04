unit BlobF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, DBCtrls, StdCtrls;

type
  TBlobForm = class(TForm)
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BlobForm: TBlobForm;

implementation

{$R *.DFM}

procedure TBlobForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree;
end;

end.
