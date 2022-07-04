unit SXSkinNotebook;

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

uses Windows, Messages, Forms, Classes, Controls, Graphics, ImgList, SysUtils,
     ExtCtrls, SXSkinPanel;

type

 TSXSkinCustomNotebook=class;
 TSXSkinNotebookPage=class;

 TSXSkinNotebookPageEvent=procedure(ASheet:TSXSkinNotebookPage)of object;
 TSXNotebookQueryPageEvent=procedure(ASheet:TSXSkinNotebookPage;var CanClose:Boolean)of object;

 TSXSkinNotebookPage=class(TSXSkinPanel)
  private
   FNotebookControl:TSXSkinCustomNotebook;
   function GetPageOrderIndex:Integer;
   procedure SetNotebookControl(ANotebookControl:TSXSkinCustomNotebook);
   procedure SetPageOrderIndex(Value:Integer);
  protected
   procedure ReadState(Reader:TReader); override;
   procedure Loaded; override;
  public
   function CanShowControl:Boolean; override;  
   constructor Create(AOwner:TComponent); override;
   procedure Paint; override;
   property NotebookControl:TSXSkinCustomNotebook read FNotebookControl write SetNotebookControl;
  published
   property Font;
   property Hint;
   property ParentShowHint;
   property PopupMenu;
   property SkinLibrary;
   property ShowHint;
   property ParentFont;
   property ParentColor;
   property PageOrderIndex:Integer read GetPageOrderIndex write SetPageOrderIndex stored False;
   property OnEnter;
   property OnExit;
   property OnResize;
 end;

 TPageChangingEvent=procedure(Sender:TObject;NewPageIndex:Integer;var AllowChange:Boolean)of object;

 TSXSkinCustomNotebook=class(TSXSkinPanel)
  private
   FPages:TList;
   FActivePage:TSXSkinNotebookPage;
   FPageChanged:TNotifyEvent;
   FPageChanging:TPageChangingEvent;
   function GetPage(Index:Integer):TSXSkinNotebookPage;
   function GetPageCount:Integer;
   function GetActivePageIndex:Integer;
  protected
   procedure GetChildren(Proc:TGetChildProc;Root:TComponent); override;
   procedure SetChildOrder(Child:TComponent;Order:Integer); override;
   procedure ShowControl(AControl:TControl); override;
   procedure Notification(AComponent:TComponent;Operation:TOperation); override;
   procedure InsertPage(Page:TSXSkinNotebookPage); virtual;
   procedure RemovePage(Page:TSXSkinNotebookPage); virtual;
   procedure SetActivePage(Page:TSXSkinNotebookPage); virtual;
   procedure SetActivePageIndex(PageIndex:Integer); virtual;
   procedure DoPageChanged; virtual;
   function DoPageChanging(NewPageIndex:Integer):Boolean; virtual;
   property ActivePageIndex:Integer read GetActivePageIndex write SetActivePageIndex stored False;
   property OnPageChanging:TPageChangingEvent read FPageChanging write FPageChanging;
   property OnPageChanged:TNotifyEvent read FPageChanged write FPageChanged;
  public
   procedure Paint; override;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   function NewPage:TSXSkinNoteBookPage;
   function FindNextPage(CurPage:TSXSkinNotebookPage;GoForward:Boolean):TSXSkinNotebookPage;
   procedure SelectNextPage(GoForward:Boolean);
   property PageCount:Integer read GetPageCount;
   property Pages[Index:Integer]:TSXSkinNotebookPage read GetPage;
  published
   property ActivePage:TSXSkinNotebookPage read FActivePage write SetActivePage;
 end;

 TSXSkinNotebook=class(TSXSkinCustomNotebook)
  published
   property Align;
   property Font;
   property OnResize;
   property SkinLibrary;
   property ActivePageIndex;
   property OnPageChanging;
   property OnPageChanged;
 end;

implementation

const SNotebookIndexError='Sheet Index Error';

{ TSXSkinNotebookPage }

constructor TSXSkinNotebookPage.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 Visible:=False;
 Align:=alClient;
end;

function TSXSkinNotebookPage.GetPageOrderIndex:Integer;
begin
 if FNotebookControl<>nil then
  Result:=FNotebookControl.FPages.IndexOf(Self) else
   Result:=-1;
end;

function TSXSkinNotebookPage.CanShowControl:Boolean;
begin
 Result:=Visible;
end;

procedure TSXSkinNotebookPage.ReadState(Reader:TReader);
begin
 inherited ReadState(Reader);
 if Reader.Parent is TSXSkinCustomNotebook then
  NotebookControl:=TSXSkinCustomNotebook(Reader.Parent);
end;

procedure TSXSkinNotebookPage.SetNotebookControl(ANotebookControl:TSXSkinCustomNotebook);
begin
 if FNotebookControl<>ANotebookControl then
  begin
   if FNotebookControl<>nil then
    FNotebookControl.RemovePage(Self);
   Parent:=ANotebookControl;
   if ANotebookControl<>nil then
    ANotebookControl.InsertPage(Self);
  end;
end;

procedure TSXSkinNotebookPage.SetPageOrderIndex(Value:Integer);
var MaxPageIndex:Integer;
begin
 if FNotebookControl<>nil then
  begin
   MaxPageIndex:=FNotebookControl.FPages.Count-1;
    if Value>MaxPageIndex then
     raise EListError.CreateFmt(SNotebookIndexError,[Value,MaxPageIndex]);
    FNotebookControl.FPages.Move(PageOrderIndex,Value);
  end;
end;

procedure TSXSkinNotebookPage.Paint;
begin
 inherited;
end;

procedure TSXSkinNotebookPage.Loaded;
begin
 inherited;
 if (csDesigning in ComponentState) and not Visible then
  SendToBack;
end;

{ TSXSkinCustomNotebook }

constructor TSXSkinCustomNotebook.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 BevelOuter:=bvNone;
 Width:=250;
 Height:=250;
 FPages:=TList.Create;
end;

destructor TSXSkinCustomNotebook.Destroy;
var A:Integer;
begin
 for A:=FPages.Count-1 downto 0 do
  TSXSkinNotebookPage(FPages[A]).Free;
 FPages.Free;
 inherited Destroy;
end;

function TSXSkinCustomNotebook.FindNextPage(CurPage:TSXSkinNotebookPage;GoForward:Boolean):TSXSkinNotebookPage;
var StartIndex:Integer;
begin
 Result:=nil;
 if FPages.Count<>0 then
  begin
   StartIndex:=FPages.IndexOf(CurPage);
   if StartIndex=-1 then
    begin
     if GoForward then
      StartIndex:=FPages.Count-1 else
       StartIndex:=0;
    end;
   if GoForward then
    begin
     Inc(StartIndex);
     if StartIndex=FPages.Count then
      StartIndex:=0;
    end else
     begin
      if StartIndex=0 then
       StartIndex:=FPages.Count;
      Dec(StartIndex);
     end;
    Result:=FPages[StartIndex];
  end;
end;

procedure TSXSkinCustomNotebook.GetChildren(Proc:TGetChildProc;Root:TComponent);
var I:Integer;
begin
 for I:=0 to FPages.Count-1 do
  Proc(TComponent(FPages[I]));
end;

function TSXSkinCustomNotebook.GetPage(Index:Integer):TSXSkinNotebookPage;
begin
 Result:=FPages[Index];
end;

function TSXSkinCustomNotebook.GetPageCount:Integer;
begin
 Result:=FPages.Count;
end;

procedure TSXSkinCustomNotebook.InsertPage(Page:TSXSkinNotebookPage);
begin
 FPages.Add(Page);
 Page.FNotebookControl:=Self;
 Page.FreeNotification(Self);
end;

procedure TSXSkinCustomNotebook.RemovePage(Page:TSXSkinNotebookPage);
var APage:TSXSkinNotebookPage;
begin
 if FActivePage=Page then
  begin
   APage:=FindNextPage(FActivePage,True);
   if APage=Page then FActivePage:=nil else
    FActivePage:=APage;
  end;
 FPages.Remove(Page);
 Page.FNotebookControl:=nil;
 if not (csDestroying in ComponentState) then
  Invalidate;
end;

procedure TSXSkinCustomNotebook.Paint;
begin
 inherited;
end;

procedure TSXSkinCustomNotebook.SelectNextPage(GoForward:Boolean);
begin
 SetActivePage(FindNextPage(ActivePage,GoForward));
end;

procedure TSXSkinCustomNotebook.SetActivePage(Page:TSXSkinNotebookPage);
var  AOldPage:TSXSkinNotebookPage;
 APageChanged:Boolean;
begin
 APageChanged:=False;
 if not (csDestroying in ComponentState) then
  begin
   if (Assigned(Page) and (Page.NotebookControl=Self)) or (Page=nil) then
    begin
     if Assigned(FActivePage) then
      begin
       if FActivePage<>Page then
        begin
         AOldPage:=FActivePage;
         FActivePage:=Page;
         AOldPage.Visible:=False;
         FActivePage.Visible:=True;
         if csDesigning in ComponentState then
          begin
           FActivePage.BringToFront;
           AOldPage.SendToBack;
           Invalidate;
          end;
{         try
          FActivePage.SelectFirst;
         except
         end;}
         APageChanged:=True;
        end;
      end else
       begin
        FActivePage:=Page;
        FActivePage.Visible:=True;
{        try
         FActivePage.SelectFirst;
        except
        end;}
        APageChanged:=True;
       end;
    end;
  end;
 if APageChanged then DoPageChanged;
end;

procedure TSXSkinCustomNotebook.SetChildOrder(Child:TComponent;Order:Integer);
begin
 TSXSkinNotebookPage(Child).PageOrderIndex:=Order;
end;

procedure TSXSkinCustomNotebook.ShowControl(AControl:TControl);
begin
 if (AControl is TSXSkinNotebookPage) and (TSXSkinNotebookPage(AControl).NotebookControl=Self) then
  SetActivePage(TSXSkinNotebookPage(AControl));
 inherited ShowControl(AControl);
end;

procedure TSXSkinCustomNotebook.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
  begin
   if (AComponent is TSXSkinNotebookPage) and (TSXSkinNotebookPage(AComponent).NotebookControl=Self) then
    RemovePage(TSXSkinNotebookPage(AComponent));
  end;
end;

procedure TSXSkinCustomNotebook.SetActivePageIndex(PageIndex:Integer);
begin
 if not (csLoading in ComponentState) then
  SetActivePage(TSXSkinNotebookPage(FPages[PageIndex]));
end;

function TSXSkinCustomNotebook.GetActivePageIndex:Integer;
begin
 if Assigned(FActivePage) then
  Result:=FPages.IndexOf(FActivePage) else Result:=-1;
end;

function TSXSkinCustomNotebook.NewPage:TSXSkinNoteBookPage;
begin
 Result:=TSXSkinNotebookPage.Create(nil);
 Result.NotebookControl:=Self;
 ActivePage:=Result;
end;

procedure TSXSkinCustomNotebook.DoPageChanged;
begin
 if not (csDestroying in ComponentState) and Assigned(FPageChanged) then
  FPageChanged(Self);
end;

function TSXSkinCustomNotebook.DoPageChanging(NewPageIndex:Integer):Boolean;
begin
 Result:=True;
 if Assigned(FPageChanging) then
  FPageChanging(Self,NewPageIndex,Result);
end;

end.
