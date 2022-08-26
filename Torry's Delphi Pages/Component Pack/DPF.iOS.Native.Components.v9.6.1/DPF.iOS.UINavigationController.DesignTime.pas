// ------------------------------------------------------------------------------
// DPF.iOS.UINavigationController.DesignTime Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.UINavigationController.DesignTime;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,

{$IFDEF IOS}
  DPF.iOS.Common,
{$ELSE}
  DesignEditors, DesignIntf, ToolsAPI,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

{$IFNDEF IOS}

type
  // ----------------------------------------------------------------------------
  IFormDesigner = IDesigner;
  TFormDesigner = IFormDesigner;

  // ----------------------------------------------------------------------------
  TDPFNavigationControllerActivePageProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue( const value: string ); override;
    function GetValue: string; override;
  end;

  TNavigationControllerEditor = class( TComponentEditor )
  public
    procedure ExecuteVerb( Index: Integer ); override;
    function GetVerb( Index: Integer ): string; override;
    function GetVerbCount: Integer; override;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------

implementation

uses

{$IFDEF WIN32}
  Windows,
{$ENDIF}
  DPF.iOS.UINavigationController;

const
  NavBarControllerVerbs: array [0 .. 2] of string = ( 'New Page', 'Next Page', 'Previous Page' );

{$IFNDEF IOS}

  // ------------------------------------------------------------------------------
function TDPFNavigationControllerActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  result := inherited GetAttributes + [paValueEditable];
end;

// ------------------------------------------------------------------------------
function TDPFNavigationControllerActivePageProperty.GetValue: string;
var
  C: TDPFNavigationController;
begin
  C      := TComponent( Self.GetComponent( 0 ) ) as TDPFNavigationController;
  result := C.ActivePage.Name;
end;

// ------------------------------------------------------------------------------
procedure TDPFNavigationControllerActivePageProperty.SetValue( const value: string );
var
  C: TDPFNavigationController;
begin
  inherited;
  C := TComponent( Self.GetComponent( 0 ) ) as TDPFNavigationController;
  if Assigned( C ) then
  begin
    Designer.SelectComponent( C.ActivePage );
    ( C as TDPFNavigationController ).ActivePage.BringToFront;
  end;
  Designer.Modified;
end;

// ------------------------------------------------------------------------------
{ TNavigationControllerEditor }
procedure TNavigationControllerEditor.ExecuteVerb( Index: Integer );
var
  PageControl: TDPFNavigationController;
  Page       : TDPFNavigationControllerPage;
  Designer   : TFormDesigner;
  PIndex     : Integer;
begin
  if Component is TDPFNavigationControllerPage then
  begin
    PageControl := TDPFNavigationControllerPage( Component ).PageControl;
  end
  else
  begin
    PageControl := TDPFNavigationController( Component );
  end;

  if PageControl <> nil then
  begin
    Designer := Self.Designer;
    if index = 0 then
    begin
      Page := TDPFNavigationControllerPage.Create( TDPFNavigationController( Designer.Root ) );
      try
        Page.Name        := Designer.UniqueName( System.copy( TDPFNavigationControllerPage.ClassName, 2, MaxInt ) );
        Page.Parent      := PageControl;
        Page.PageControl := PageControl;
        PageControl.AddObject( Page );
      except
        Page.DisposeOf;
        raise;
      end;
      PageControl.ActivePage := Page;
      Page.BringToFront;
      Page.PagePrompt := Page.PagePrompt;
      Designer.SelectComponent( Page );
      Designer.Modified;
    end
    else if PageControl.PageCount > 0 then
    begin

      if not Assigned( PageControl.ActivePage ) then
        PageControl.ActivePage := PageControl.Pages[0];

      PIndex := PageControl.ActivePage.PageIndex;
      if index = 1 then
      begin
        if PIndex < PageControl.PageCount - 1 then
          PIndex := PIndex + 1
      end
      else
      begin
        if PIndex > 0 then
          PIndex := PIndex - 1
      end;

      if PIndex > -1 then
      begin
        PageControl.ActivePage := PageControl.Pages[PIndex];
        Designer.SelectComponent( PageControl.ActivePage );
        Designer.Modified;
      end;
    end;
    TDPFNavigationController( PageControl ).Change;
  end;
end;

// ------------------------------------------------------------------------------
function TNavigationControllerEditor.GetVerb( Index: Integer ): string;
begin
  result := NavBarControllerVerbs[index];
end;

// ------------------------------------------------------------------------------
function TNavigationControllerEditor.GetVerbCount: Integer;
begin
  result := Length( NavBarControllerVerbs );
end;
{$ENDIF}

end.
