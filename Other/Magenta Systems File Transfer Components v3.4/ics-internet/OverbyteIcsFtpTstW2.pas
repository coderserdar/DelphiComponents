unit OverByteIcsFtpTstW2;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDirectoryForm = class(TForm)
    DirListBox: TListBox;
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

{$R *.DFM}

end.
