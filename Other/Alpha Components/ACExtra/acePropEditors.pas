unit acePropEditors;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ImgList, Consts, ComStrs, CommCtrl, TypInfo,
  {$IFDEF DELPHI6UP}
  DesignEditors, DesignIntf, VCLEditors,
  {$ELSE}
  dsgnintf,
  {$ENDIF}
  sConst, ExtCtrls, sPanel, sGraphUtils, acntUtils;


type
  TacScrollPanelEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;


procedure Register;

implementation


uses sDefaults, sSkinManager, FileCtrl, sMaskData, sSkinProps, aceScrollPanel;


function TacScrollPanelEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;


function TacScrollPanelEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add new panel'
  else
    Result := inherited GetVerb(Index);
end;


procedure TacScrollPanelEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    Designer.CreateComponent(TacScrollPanelBand, Component, 0, 0, 0, 50)
  else
    inherited ExecuteVerb(Index);
end;


procedure TacScrollPanelEditor.Edit;
begin
end;


procedure Register;
begin
  RegisterClass(TacScrollPanelBand);
  RegisterComponentEditor(TacScrollPanel, TacScrollPanelEditor);
end;

end.



