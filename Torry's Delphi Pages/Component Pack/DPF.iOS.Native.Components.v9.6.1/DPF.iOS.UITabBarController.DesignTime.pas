// ------------------------------------------------------------------------------
// DPF.iOS.UITabBarController.DesignTime Class
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
unit DPF.iOS.UITabBarController.DesignTime;

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
  TDPFTabBarControllerActivePageIndexProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue( const value: string ); override;
    function GetValue: string; override;
  end;

  TTabBarControllerEditor = class( TComponentEditor )
  public
    procedure ExecuteVerb( Index: Integer ); override;
    function GetVerb( Index: Integer ): string; override;
    function GetVerbCount: Integer; override;
  end;

  // ----------------------------------------------------------------------------
{$ENDIF}

  // ------------------------------------------------------------------------------

implementation

uses

{$IFDEF WIN32}
  Windows,
{$ENDIF}
  DPF.iOS.UITabBarController;

const
  TabBarControllerVerbs: array [0 .. 2] of string = ( 'New Page', 'Next Page', 'Previous Page' );

{$IFNDEF IOS}
// ------------------------------------------------------------------------------
{ TTabBarControllerEditor }

procedure TTabBarControllerEditor.ExecuteVerb( Index: Integer );
var
  PageControl: TDPFTabBarController;
  Page       : TDPFTabBarItem;
  Designer   : TFormDesigner;
begin
  if Component is TDPFTabBarItem then
  begin
    PageControl := TDPFTabBarItem( Component ).PageControl;
  end
  else
  begin
    PageControl := TDPFTabBarController( Component );
  end;

  // if ( PageControl is TDPFTabBarController ) and ( index = 0 ) then exit;

  if PageControl <> nil then
  begin
    Designer := Self.Designer;
    if index = 0 then
    begin
      Page := TDPFTabBarItem.Create( Designer.Root );
      try
        Page.Name        := Designer.UniqueName( System.copy( TDPFTabBarItem.ClassName, 2, MaxInt ) );
        Page.Parent      := PageControl;
        Page.PageControl := PageControl;
        PageControl.AddObject( Page );
      except
        Page.DisposeOf;
        raise;
      end;
      PageControl.ActivePage := Page;
      Page.BringToFront;
      Designer.SelectComponent( Page );
      Designer.Modified;
    end
    else
    begin
      { Page := PageControl.FindNextPage( PageControl.ActivePage, index = 1, False );
        if ( Page <> nil ) and ( Page <> PageControl.ActivePage ) then
        begin
        PageControl.ActivePage := Page;
        if Component is TDPFTabBarItem then
        Designer.SelectComponent( Page );
        Designer.Modified;
        end; }
    end;
    //TDPFTabBarController( PageControl ).Change;
  end;
end;

// ------------------------------------------------------------------------------
function TTabBarControllerEditor.GetVerb( Index: Integer ): string;
begin
  result := TabBarControllerVerbs[index];
end;

// ------------------------------------------------------------------------------
function TTabBarControllerEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

// ------------------------------------------------------------------------------
{ TDPFTabBarControllerActivePageIndexProperty }
function TDPFTabBarControllerActivePageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  result := inherited GetAttributes + [paValueEditable];
end;

// ------------------------------------------------------------------------------
function TDPFTabBarControllerActivePageIndexProperty.GetValue: string;
var
  C: TDPFTabBarController;
begin
  C      := TComponent( Self.GetComponent( 0 ) ) as TDPFTabBarController;
  result := IntToSTr( C.ActivePageIndex );
end;

// ------------------------------------------------------------------------------
procedure TDPFTabBarControllerActivePageIndexProperty.SetValue( const value: string );
var
  C: TDPFTabBarController;
begin
  inherited;
  C := TComponent( Self.GetComponent( 0 ) ) as TDPFTabBarController;
  if ( C as TDPFTabBarController ).ActivePageIndex = StrToInt( value ) then
    Exit;
  ( C as TDPFTabBarController ).ActivePageIndex := StrToInt( value );

  if ( C as TDPFTabBarController ).ActivePageIndex <> -1 then
  begin
    Designer.SelectComponent( C.ActivePage );
    ( C as TDPFTabBarController ).ActivePage.BringToFront;
  end;
  Designer.Modified;
end;
{$ENDIF}

end.
