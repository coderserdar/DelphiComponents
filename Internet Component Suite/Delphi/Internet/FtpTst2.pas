unit FtpTst2;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
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
