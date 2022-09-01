unit bvGridPopupUnit;

interface

uses
  {$ifndef LINUX}
   menus;
  {$else}
   QMenus;
  {$endif}

type
    TbvCommonGridMenuItem = class(tMenuItem);
    TbvCommonGridPopupMenu = class(TPopupMenu);

implementation

end.
