unit Grid3Unit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QGraphics,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QDialogs,
  QMenus,
  Types,
  QControls,
  QComCtrls,
  QGrids,
  QTypes,
{$endif}
  Classes,
  SysUtils,
  bvDBGrid,
  bvLocalization,

  bvDBGridDemoUnit, DB, DBClient, bvFormSaver;

type
  TbvDBGridDemoForm3 = class(TbvDBGridDemoForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  bvDBGridDemoForm3: TbvDBGridDemoForm3;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

end.
