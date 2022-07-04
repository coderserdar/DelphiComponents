{*********************************************************}
{*                  VPPRTFMTED.PAS 1.03                  *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpPrtFmtEd;
  {- property editor for TVpControlLink.Printer.PrintFormats property}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF VERSION6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  StdCtrls, ExtCtrls, Buttons, VpSR,

  VpBase, VpBaseDS, VpEdFmtLst;

type
  TVpPrtFmtPropertyEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

  TVpPrtFmtEditor = class(TfrmPrnFormat)
  public
    Designer   : IDesigner;
  end;


implementation

var
  frmPrtFmtEd : TVpPrtFmtEditor;

{$IFDEF VERSION6}
  procedure EditPrtFmts(Designer : IDesigner; Link : TVpControlLink);
{$ELSE}
  procedure EditPrtFmts(Designer : IFormDesigner; Link : TVpControlLink);
{$ENDIF}
begin
  frmPrtFmtEd := TVpPrtFmtEditor.Create(Application);
  frmPrtFmtEd.Designer := Designer;
  frmPrtFmtEd.ControlLink := Link;
  frmPrtFmtEd.Execute;
  if Assigned(Designer) then
    Designer.Modified;
  frmPrtFmtEd.Free;  
end;


{ TVpPrtFmtEditor }

procedure TVpPrtFmtPropertyEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    EditPrtFmts(Designer, (Component as TVpControlLink));
end;

function TVpPrtFmtPropertyEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RSEditPrintFormat;  
end;

function TVpPrtFmtPropertyEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
