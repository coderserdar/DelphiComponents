unit SXSkinNotebookReg;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Classes, DesignIntf, DesignEditors, TypInfo;

type

 TSXSkinNotebookActivePageProperty=class(TComponentProperty)
  public
   function GetAttributes:TPropertyAttributes; override;
   procedure GetValues(Proc:TGetStrProc); override;
 end;

 TSXSkinCustomNotebookEditor=class(TDefaultEditor)
  procedure ExecuteVerb(Index:Integer); override;
  function GetVerb(Index:Integer): string; override;
  function GetVerbCount:Integer; override;
 end;

procedure Register;

implementation

uses SXSkinNotebook;

procedure Register;
begin
 RegisterPropertyEditor(TypeInfo(TSXSkinNotebookPage),TSXSkinNotebook,'ActivePage',TSXSkinNotebookActivePageProperty);
 RegisterComponentEditor(TSXSkinCustomNotebook,TSXSkinCustomNotebookEditor);
 RegisterComponentEditor(TSXSkinNotebookPage,TSXSkinCustomNotebookEditor);
end;

const

 SNotebookIndexError = 'Notebook Page Index Error';
 StrAddPage          = 'New Page';
 StrNextPage         = 'Next Page';
 StrPrevPage         = 'Previous Page';

{ TSXSkinNotebookActivePageProperty }

function TSXSkinNotebookActivePageProperty.GetAttributes:TPropertyAttributes;
begin
 Result:=[paValueList];
end;

procedure TSXSkinNotebookActivePageProperty.GetValues(Proc: TGetStrProc);
var      I:Integer;
 Component:TComponent;
begin
 for I:=0 to Designer.Root.ComponentCount-1 do
  begin
   Component:=Designer.Root.Components[I];
   if (Component.Name<>'') and (Component is TSXSkinNotebookPage) and
      (TSXSkinNotebookPage(Component).NotebookControl=GetComponent(0)) then
    Proc(Component.Name);
  end;
end;

{ TSXSkinCustomNotebookEditor }

procedure TSXSkinCustomNotebookEditor.ExecuteVerb(Index:Integer);
var NotebookControl:TSXSkinCustomNotebook;
    Page:TSXSkinNotebookPage;
begin
 if Component is TSXSkinNotebookPage then
  NotebookControl:=TSXSkinNotebookPage(Component).NotebookControl else
   NotebookControl:=TSXSkinCustomNotebook(Component);
 if NotebookControl<>nil then
  begin
   if Index=0 then
    begin
     Page:=TSXSkinNotebookPage.Create(Designer.Root);
     try
      Page.Name:=Designer.UniqueName(TSXSkinNotebookPage.ClassName);
      Page.NotebookControl:=NotebookControl;
     except
      Page.Free;
      raise;
     end;
     NotebookControl.ActivePage:=Page;
     Designer.SelectComponent(Page);
     Designer.Modified;
    end else
   if Index<3 then
    begin
     Page:=NotebookControl.FindNextPage(NotebookControl.ActivePage,Index=1);
     if (Page<>nil) and (Page<>NotebookControl.ActivePage) then
      begin
       NotebookControl.ActivePage:=Page;
       if Component is TSXSkinNotebookPage then
        Designer.SelectComponent(Page);
       Designer.Modified;
      end;
    end else
     begin
      Dec(Index,3);
      if Index<NotebookControl.PageCount then
       begin
        Page:=NotebookControl.Pages[Index];
        if (Page<>nil) and (Page<>NotebookControl.ActivePage) then
         begin
          NotebookControl.ActivePage:=Page;
          if Component is TSXSkinNotebookPage then
           Designer.SelectComponent(Page);
          Designer.Modified;
         end;
       end;
     end;
  end;
end;

function TSXSkinCustomNotebookEditor.GetVerb(Index:Integer):String;
var NotebookControl:TSXSkinCustomNotebook;
begin
 case Index of
  0:   Result:=StrAddPage;
  1:   Result:=StrNextPage;
  2:   Result:=StrPrevPage;
  else begin
        Result:='';
        if Component is TSXSkinNotebookPage then
         NotebookControl:=TSXSkinNotebookPage(Component).NotebookControl else
          NotebookControl:=TSXSkinCustomNotebook(Component);
        if NotebookControl<>nil then
         begin
          Dec(Index,3);
          if Index<NotebookControl.PageCount then
           Result:='Open '+NotebookControl.Pages[Index].Name;
         end;
       end;
 end;
end;

function TSXSkinCustomNotebookEditor.GetVerbCount:Integer;
var NotebookControl:TSXSkinCustomNotebook;
          PageCount:Integer;
begin
 PageCount:=0;
 if Component is TSXSkinNotebookPage then
  NotebookControl:=TSXSkinNotebookPage(Component).NotebookControl else
   NotebookControl:=TSXSkinCustomNotebook(Component);
 if NotebookControl<>nil then
  PageCount:=NotebookControl.PageCount;
 Result:=3+PageCount;
end;

end.
