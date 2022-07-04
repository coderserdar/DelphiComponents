{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
}
unit SMBoxReg;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses
  {$IFDEF SMForDelphi6} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF} ;

type
  TSMBoxEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(index: Integer): string; override;
    procedure ExecuteVerb(index: Integer); override;
  end;

  TSMPanelEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(index: Integer): string; override;
    procedure ExecuteVerb(index: Integer); override;
  end;

procedure Register;

implementation
uses Classes, SMBox;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMPanel, TSMBox]);
  RegisterComponentEditor(TSMPanel, TSMPanelEditor);
  RegisterComponentEditor(TSMBox, TSMBoxEditor);
end;


{ TSMBoxEditor }
function TSMBoxEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TSMBoxEditor.GetVerb(index: Integer): string;
begin
  case index of
    0: Result := 'Add page';
    1: Result := 'Move page up';
    2: Result := 'Move page down';
  end;
end;

procedure TSMBoxEditor.ExecuteVerb(index: Integer);
begin
  case index of
     0: TSMBox(Component).SMPanel.InsertPage(TSMBox(Designer.CreateComponent(TSMBox, TSMBox(Component).SMPanel,0,0,20,60)));
     1: TSMBox(Component).MovePageUp;
     2: TSMBox(Component).MovePageDown;
  end;
end;

function TSMPanelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TSMPanelEditor.GetVerb(index: Integer): string;
begin
  case index of
    0: Result :='Add page';
  end;
end;

procedure TSMPanelEditor.ExecuteVerb(index: Integer);
begin
  case index of
     0: TSMPanel(Component).InsertPage(TSMBox(Designer.CreateComponent(TSMBox, Component,0,0,20,60)));
  end;
end;

end.
