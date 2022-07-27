{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmNotebookReg
Purpose  : Declares the component and property editors for the rmNotebook control
Date     : 11-20-02
Author   : Ryan J. Mills
Version  : 1.90
Notes    :
================================================================================}

unit rmNotebookReg;

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
  TrmNotebookActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TrmNotebookControlEditor = class(TDefaultEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

uses rmNotebook2;

const
  SNotebookIndexError = 'Notebook Page Index Error';
  StrAddPage = 'New Page';
  StrNextPage = 'Next Page';
  StrPrevPage = 'Previous Page';

{ TrmNotebookActivePageProperty }

function TrmNotebookActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TrmNotebookActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
{$ifdef D6_or_higher}
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    Component := Designer.Root.Components[I];
    if (Component.Name <> '') and (Component is TrmNotebookPage) and
      (TrmNotebookPage(Component).NotebookControl = GetComponent(0)) then
      Proc(Component.Name);
  end;
{$else}
  for I := 0 to Designer.Form.ComponentCount - 1 do
  begin
    Component := Designer.Form.Components[I];
    if (Component.Name <> '') and (Component is TrmNotebookPage) and
      (TrmNotebookPage(Component).NotebookControl = GetComponent(0)) then
      Proc(Component.Name);
  end;
{$endif}
end;

{ TrmNotebookControlEditor }

procedure TrmNotebookControlEditor.ExecuteVerb(Index: Integer);
var
  NotebookControl: TrmNotebookControl;
  Page: TrmNotebookPage;
begin
  if Component is TrmNotebookPage then
    NotebookControl := TrmNotebookPage(Component).NotebookControl
  else
    NotebookControl := TrmNotebookControl(Component);

  if NotebookControl <> nil then
  begin
    if Index = 0 then
    begin
      {$ifdef D6_or_higher}
      Page := TrmNotebookPage.Create(Designer.Root);
      {$else}
      Page := TrmNotebookPage.Create(Designer.Form);
      {$endif}
      try
        Page.Name := Designer.UniqueName(TrmNotebookPage.ClassName);
        Page.NotebookControl := NotebookControl;
        Page.Caption := Page.Name;
      except
        Page.Free;
        raise;
      end;
      NotebookControl.ActivePage := Page;
      Designer.SelectComponent(Page);
      Designer.Modified;
    end else
    begin
      Page := NotebookControl.FindNextPage(NotebookControl.ActivePage, Index = 1);
      if (Page <> nil) and (Page <> NotebookControl.ActivePage) then
      begin
        NotebookControl.ActivePage := Page;
        if Component is TrmNotebookPage then
          Designer.SelectComponent(Page);
        Designer.Modified;
      end;
    end;
  end;
end;

function TrmNotebookControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result:= StrAddPage;
    1 : Result:= StrNextPage;
    2 : Result:= StrPrevPage;
  end;
end;

function TrmNotebookControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.
