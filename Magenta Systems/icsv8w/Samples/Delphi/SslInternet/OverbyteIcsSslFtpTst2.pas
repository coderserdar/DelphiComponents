unit OverbyteIcsSslFtpTst2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDirectoryForm = class(TForm)
    DirListBox: TListBox;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

{$R *.DFM}

end.
