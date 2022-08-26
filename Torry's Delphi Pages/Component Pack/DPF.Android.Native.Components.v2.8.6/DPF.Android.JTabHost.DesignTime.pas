// ------------------------------------------------------------------------------
// DPF.Android.JTabHost.DesignTime Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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
unit DPF.Android.JTabHost.DesignTime;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,

{$IFDEF IOS}
  DPF.Android.Common,
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
  TDPFJTabHostActivePageIndexProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue( const value: string ); override;
    function GetValue: string; override;
  end;

  TAndroidTabBarControllerEditor = class( TComponentEditor )
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
  DPF.Android.JTabHost;

const
  TabBarControllerVerbs: array [0 .. 2] of string = ( 'New Page', 'Next Page', 'Previous Page' );

{$IFNDEF IOS}
// ------------------------------------------------------------------------------
{ TAndroidTabBarControllerEditor }

procedure TAndroidTabBarControllerEditor.ExecuteVerb( Index: Integer );
var
  PageControl: TDPFJTabHost;
  Page       : TDPFAndroidTabBarItem;
  Designer   : TFormDesigner;
begin
  if Component is TDPFAndroidTabBarItem then
  begin
    PageControl := TDPFAndroidTabBarItem( Component ).PageControl;
  end
  else
  begin
    PageControl := TDPFJTabHost( Component );
  end;

  // if ( PageControl is TDPFJTabHost ) and ( index = 0 ) then exit;

  if PageControl <> nil then
  begin
    Designer := Self.Designer;
    if index = 0 then
    begin
      Page := TDPFAndroidTabBarItem.Create( Designer.Root );
      try
        Page.Name        := Designer.UniqueName( System.copy( TDPFAndroidTabBarItem.ClassName, 2, MaxInt ) );
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
        if Component is TDPFAndroidTabBarItem then
        Designer.SelectComponent( Page );
        Designer.Modified;
        end; }
    end;
    TDPFJTabHost( PageControl ).Change;
  end;
end;

// ------------------------------------------------------------------------------
function TAndroidTabBarControllerEditor.GetVerb( Index: Integer ): string;
begin
  result := TabBarControllerVerbs[index];
end;

// ------------------------------------------------------------------------------
function TAndroidTabBarControllerEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

// ------------------------------------------------------------------------------
{ TDPFJTabHostActivePageIndexProperty }
function TDPFJTabHostActivePageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  result := inherited GetAttributes + [paValueEditable];
end;

// ------------------------------------------------------------------------------
function TDPFJTabHostActivePageIndexProperty.GetValue: string;
var
  C: TDPFJTabHost;
begin
  C      := TComponent( Self.GetComponent( 0 ) ) as TDPFJTabHost;
  result := IntToSTr( C.ActivePageIndex );
end;

// ------------------------------------------------------------------------------
procedure TDPFJTabHostActivePageIndexProperty.SetValue( const value: string );
var
  C: TDPFJTabHost;
begin
  inherited;
  C := TComponent( Self.GetComponent( 0 ) ) as TDPFJTabHost;
  if ( C as TDPFJTabHost ).ActivePageIndex = StrToInt( value ) then
    Exit;
  ( C as TDPFJTabHost ).ActivePageIndex := StrToInt( value );

  if ( C as TDPFJTabHost ).ActivePageIndex <> -1 then
  begin
    Designer.SelectComponent( C.ActivePage );
    ( C as TDPFJTabHost ).ActivePage.BringToFront;
  end;
  Designer.Modified;
end;
{$ENDIF}

end.
