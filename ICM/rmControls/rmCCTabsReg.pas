{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCCTabsReg
Purpose  : Component editor for the CCTab controls
Date     : 10-26-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmCCTabsReg;

interface

{$I CompilerDefines.inc}

{$ifdef D6_or_higher}
uses
  DesignIntf, DesignEditors, TypInfo;    
{$else}
uses
  DsgnIntf, TypInfo;
{$endif}

type
  TrmCCPageControlEditor = class(TComponentEditor)
  private
    procedure NewPage;
    procedure ChangePage(Forward:boolean);
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(index:integer); override;
    procedure Edit; Override;
  end;

implementation

Uses sysutils, Classes, rmCCTabs;

{ TrmCCPageControlEditor }

function TrmCCPageControlEditor.GetVerb(index:integer):string;
begin
     case index of
          0:result := 'New Page';
          1:result := 'Next Page';
          2:result := 'Previous Page';
     end;
end;

function TrmCCPageControlEditor.GetVerbCount:integer;
begin
     result := 3;
end;

procedure TrmCCPageControlEditor.Edit;
var
   eventname : string;
   changeevent : TNotifyEvent;
   ppi: PPropInfo;
   wPageControl : trmccpagecontrol;
begin
     if component is trmcctabsheet then
       wPageControl := trmcctabsheet(component).pagecontrol
     else
       wPageControl := trmccpagecontrol(component);

     changeevent := wPageControl.OnChange;
     if assigned(changeevent) then
        eventname := designer.getmethodname(tmethod(changeevent))
     else
     begin
          eventname := wPageControl.name + 'Change';
          ppi := GetPropInfo( wPageControl.classinfo,'OnChange');
          changeevent := tnotifyevent(designer.createmethod(eventname,gettypedata(ppi.proptype^)));
          wPageControl.onchange := changeevent;
          designer.modified;
     end;
     designer.showmethod(eventname);
end;

procedure TrmCCPageControlEditor.ExecuteVerb(index:integer);
begin
     case index of
          0:NewPage;
          1:ChangePage(true); //Next Page
          2:ChangePage(False); //Previous Page
     end;
end;

procedure TrmCCPageControlEditor.NewPage;
var
   wPageControl : trmccpagecontrol;
   wPage : trmcctabsheet;
begin
     if component is trmcctabsheet then
       wPageControl := trmcctabsheet(component).pagecontrol
     else
       wPageControl := trmccpagecontrol(component);

     if wPageControl <> nil then
     begin
          {$ifdef D6_or_higher}
          wPage := trmcctabsheet.Create(designer.Root);
          {$else}
          wPage := trmcctabsheet.Create(designer.Form);
          {$endif}
          try
            wPage.name := designer.uniquename(trmcctabsheet.classname);
            wPage.parent := wPageControl;
            wPage.pagecontrol := wPageControl;
            wPage.caption := wPage.name;
          except
            wPage.free;
            raise;
          end;
          wPageControl.activepage := wPage;
          designer.selectcomponent(wPage);
          designer.modified;
     end;
end;

procedure TrmCCPageControlEditor.ChangePage(forward:boolean);
var
   wPageControl : trmccpagecontrol;
   wPage : trmcctabsheet;
begin
     if component is trmcctabsheet then
       wPageControl := trmcctabsheet(component).pagecontrol
     else
       wPageControl := trmccpagecontrol(component);

     if wPageControl <> nil then
     begin
       wPage := wPageControl.findnextpage(wPageControl.activepage, forward, false);
       if (wPage <> nil) and (wPage <> wPageControl.activepage) then
       begin
         wPageControl.activepage := wPage;
         if component is trmcctabsheet then designer.selectcomponent(wPage);
         designer.modified;
       end;
     end;
end;

end.
 