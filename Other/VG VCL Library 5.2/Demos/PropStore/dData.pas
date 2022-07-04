unit dData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Explorer, ExplShl, vgTools, vgStndrt;

type
  Tdm = class(TDataModule)
    psData: TPropStorage;
    afIniFile: TAppIniFile;
    ExplorerRootNode1: TExplorerRootNode;
    enShell: TExplorerShellFolderNode;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm: Tdm;

implementation

{$R *.DFM}

end.
