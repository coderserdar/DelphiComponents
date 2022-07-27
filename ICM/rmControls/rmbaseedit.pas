{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmBaseEdit
Purpose  : Base Edit class used by other "rmEdit" controls.
Date     : 01-15-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmBaseEdit;

interface

{$I CompilerDefines.INC}

uses Messages, Windows, Classes, StdCtrls;

type
  TrmCustomEdit = class(TCustomEdit)
  private
    fWantTabs: boolean;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner:TComponent); override;
    property BorderStyle;
    property ReadOnly;
    property WantTabs:boolean read fWantTabs write fWantTabs default false;
  end;

implementation

{ TrmCustomEdit }

procedure TrmCustomEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
   inherited;

   if fWantTabs then
      Message.Result := Message.Result or DLGC_WANTTAB;
end;

constructor TrmCustomEdit.Create(AOwner: TComponent);
begin
  inherited;
  fWantTabs := false;
end;

end.
