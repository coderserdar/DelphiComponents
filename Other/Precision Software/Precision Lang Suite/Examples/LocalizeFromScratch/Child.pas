{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2010
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Child form unit, see WMLanguageChanged handler in another demos
           to make the localization texts fully functional

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

unit Child;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChild: TfrmChild;

implementation

{$R *.dfm}

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

end.
