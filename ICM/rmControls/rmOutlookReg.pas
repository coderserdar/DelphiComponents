{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmOutlookReg
Purpose  : Declares the component and property editors for the rmOutlook control
Date     : 03-06-01
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmOutlookReg;

interface

{$I CompilerDefines.INC}

{$ifdef D6_or_higher}
uses
  Classes, DesignIntf, DesignEditors, TypInfo;
{$else}
uses
  Classes, DsgnIntf, TypInfo;
{$endif}

type
  TrmOutlookActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TrmOutlookControlEditor = class(TDefaultEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

uses rmOutlook;

const
  SOutlookIndexError = 'Outlook Page Index Error';
  StrAddPage = 'New Page';
  StrNextPage = 'Next Page';
  StrPrevPage = 'Previous Page';

{ TrmOutlookActivePageProperty }

function TrmOutlookActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TrmOutlookActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
{$ifdef D6_or_higher}
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    Component := Designer.Root.Components[I];
    if (Component.Name <> '') and (Component is TrmOutLookPage) and
      (TrmOutLookPage(Component).OutlookControl = GetComponent(0)) then
      Proc(Component.Name);
  end;
{$else}
  for I := 0 to Designer.Form.ComponentCount - 1 do
  begin
    Component := Designer.Form.Components[I];
    if (Component.Name <> '') and (Component is TrmOutLookPage) and
      (TrmOutLookPage(Component).OutlookControl = GetComponent(0)) then
      Proc(Component.Name);
  end;
{$endif}
end;

{ TrmOutlookControlEditor }

procedure TrmOutlookControlEditor.ExecuteVerb(Index: Integer);
var
  OutlookControl: TrmOutLookControl;
  Page: TrmOutLookPage;
//  Designer: IDesigner;
begin
  if Component is TrmOutLookPage then
    OutlookControl := TrmOutLookPage(Component).OutlookControl
  else
    OutlookControl := TrmOutLookControl(Component);

  if OutlookControl <> nil then
  begin
//    Designer := Self.Designer;
    if Index = 0 then
    begin
      {$ifdef D6_or_higher}
      Page := TrmOutLookPage.Create(Designer.Root);
      {$else}
      Page := TrmOutLookPage.Create(Designer.Form);
      {$endif}
      try
        Page.Name := Designer.UniqueName(TrmOutLookPage.ClassName);
        Page.OutlookControl := OutlookControl;
        Page.Caption := Page.Name;
      except
        Page.Free;
        raise;
      end;
      OutlookControl.ActivePage := Page;
      Designer.SelectComponent(Page);
      Designer.Modified;
    end else
    begin
      Page := OutlookControl.FindNextPage(OutlookControl.ActivePage, Index = 1);
      if (Page <> nil) and (Page <> OutlookControl.ActivePage) then
      begin
        OutlookControl.ActivePage := Page;
        if Component is TrmOutLookPage then
          Designer.SelectComponent(Page);
        Designer.Modified;
      end;
    end;
  end;
end;

function TrmOutlookControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result:= StrAddPage;
    1 : Result:= StrNextPage;
    2 : Result:= StrPrevPage;
  end;
end;

function TrmOutlookControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.
