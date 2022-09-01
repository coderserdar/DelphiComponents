unit bvCursorUnit;

interface

uses
  {$ifndef LINUX}
  Controls,
  Forms;
  {$else}
  QControls,
  QForms;
  {$endif}
//  System;

type TWaitCursor=class
  private
    OldCursor:TCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

constructor TWaitCursor.Create;
begin
  OldCursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
end;

destructor TWaitCursor.Destroy;
begin
  inherited;
  Screen.Cursor:=OldCursor;
end;

end.
