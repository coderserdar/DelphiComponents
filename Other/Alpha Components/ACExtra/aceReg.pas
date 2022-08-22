unit aceReg;
{$I sDefs.inc}

interface

uses
  Classes;

procedure Register;

implementation

uses
  Registry,
  aceScrollPanel, aceCheckComboBox, aceFloatPanel, aceListView, aceComboEditEx, aceColorSelector,
  aceSkinMenu;

procedure Register;
begin
  RegisterComponents('AlphaExtra', [TacScrollPanel, TacCheckComboBox, TacFloatPanel, TacListView,
    TacComboEditEx, TacColorSelector, TacSkinMenu]);
end;

end.
