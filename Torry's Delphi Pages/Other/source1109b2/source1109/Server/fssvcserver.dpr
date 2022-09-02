Program fssvcserver;

{$I FSDEFINE.INC}

uses
  fssrvsvccntl in 'fssrvsvccntl.pas',
  fssrjrn in '..\fssrjrn.pas',
  fssrvegmgr in 'fssrvegmgr.pas' {FSEngineManager: TFSBaseEngineManager};

{$R fsserver.res}
Begin
  If IsNT Then
    RunNT
  Else
    RunWin9x;
End.

