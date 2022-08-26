program DummyDockProj;

uses
  Forms,
  BaseDock in 'BaseDock.pas' {BaseDockHostForm},
  DeskForm in 'DeskForm.pas' {DesktopForm},
  DeskUtil in 'DeskUtil.pas',
  DeskStrs in 'DeskStrs.pas',
  DockForm in 'DockForm.pas' {DockableForm},
  DockToolForm in 'DockToolForm.pas' {DockableToolbarForm},
  begin,
  pb_data in '..\Source\pb_data.pas' {Data: TDataModule},
  pb_base in '..\Source\pb_base.pas' {Base: TDataModule};

uses
  Forms,
  BaseDock in 'BaseDock.pas' {BaseDockHostForm},
  DeskForm in 'DeskForm.pas' {DesktopForm},
  DeskUtil in 'DeskUtil.pas',
  DeskStrs in 'DeskStrs.pas',
  DockForm in 'DockForm.pas' {DockableForm},
  DockToolForm in 'DockToolForm.pas' {DockableToolbarForm},

{$R *.RES}

begin
end.
