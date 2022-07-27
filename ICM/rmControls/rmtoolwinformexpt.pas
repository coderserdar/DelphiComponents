{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit       : rmToolWinFormExpt
Purpose    : Custom Form Expert, used in controlling the TrmToolWinForm and
             decendants....
Components : TrmToolWinFormExpert
Date       : 06-15-1999
Author     : Ryan J. Mills
Version    : 1.90
Note       : This unit was originally apart of Sergey Orlik's Custom Forms Pack
             I've included it here with modifications for the rmToolWinForm.
             I've also left the original file header in to help explain
             more about it.  
================================================================================
}

{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Custom Forms Pack (CFPack)                      }
{                                                       }
{       Copyright (c) 1997-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}
{$I Sergey_Orlik_DEF.INC}
{$Warnings OFF}

unit rmToolWinFormExpt;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExptIntf, ToolIntf, EditIntf, VirtIntf;

type
  { TrmToolWinFormExpert }

  TrmToolWinFormExpert = class(TIExpert)
  private
    procedure RunExpert(ToolServices: TIToolServices);
  public
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetStyle: TExpertStyle; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;

implementation

uses rmToolWin;

const
  CRLF = #13#10;
  CRLF2 = #13#10#13#10;
  DefaultModuleFlags = [cmShowSource, cmShowForm, cmMarkModified, cmUnNamed];

resourcestring
  sCustFormExpertAuthor = 'Ryan J. Mills';
  sCustFormExpertName   = 'rmToolWin Form';
  sCustFormExpertDesc   = 'Create a new TrmToolWinForm';

{ TrmToolWinFormModuleCreator }

type
{$IFDEF VER_CB}
  TrmToolWinFormModuleCreator = class(TIModuleCreatorEx)
{$ELSE}
  TrmToolWinFormModuleCreator = class(TIModuleCreator)
{$ENDIF}
  private
    FAncestorIdent : string;
    FAncestorClass : TClass;
//    FNewFormOption : TNewFormOption;
    FFormIdent : string;
    FUnitIdent : string;
    FFileName  : string;
  public
    function Existing: Boolean; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function GetFormName: string; override;
    function GetAncestorName: string; override;
  {$IFNDEF VER100}
    {$IFDEF VER_CB}
    function GetIntfName: string; override;
    function NewIntfSource(const UnitIdent, FormIdent,
      AncestorIdent: string): string; override;
    {$ENDIF}
    function NewModuleSource(const UnitIdent, FormIdent,
      AncestorIdent: string): string; override;
  {$ELSE}
    function NewModuleSource(UnitIdent, FormIdent,
      AncestorIdent: string): string; override;
  {$ENDIF}
    procedure FormCreated(Form: TIFormInterface); override;
  end;

function TrmToolWinFormModuleCreator.Existing:boolean;
begin
  Result:=False
end;

function TrmToolWinFormModuleCreator.GetFileName:string;
begin
  Result:=FFileName; //'';
end;

function TrmToolWinFormModuleCreator.GetFileSystem:string;
begin
  Result:='';
end;

function TrmToolWinFormModuleCreator.GetFormName:string;
begin
  Result:=FFormIdent;
end;

function TrmToolWinFormModuleCreator.GetAncestorName:string;
begin
  Result:=FAncestorIdent;
end;

{$IFDEF VER_CB}
function UnitName2Namespace(const Value:string):string;
var
  s1,s2 : string;
begin
  s1:=Value[1];
  s2:=LowerCase(Value);
  System.Delete(s2,1,1);
  Result:=UpperCase(s1)+s2;
end;

function TrmToolWinFormModuleCreator.GetIntfName: string;
begin
  Result:='';
end;

function TrmToolWinFormModuleCreator.NewIntfSource(const UnitIdent, FormIdent,
      AncestorIdent: string): string;
var
  s : string;
begin
  s:=s+'//---------------------------------------------------------------------------'+
      CRLF+
      '#ifndef '+UnitIdent+'H'+CRLF+
      '#define '+UnitIdent+'H'+CRLF+
      '//---------------------------------------------------------------------------'+
      CRLF+
      '#include <Classes.hpp>'+CRLF+
      '#include <Controls.hpp>'+CRLF+
      '#include <StdCtrls.hpp>'+CRLF+
      '#include <Forms.hpp>'+CRLF;

  if (AncestorIdent<>'Form') and  (AncestorIdent<>'DataModule') then
    s:=s+
     '#include "'+GetCustomFormUnit(FAncestorClass.ClassName)+'.h"'+CRLF;

  s:=s+'//---------------------------------------------------------------------------'+
      CRLF+
      'class T'+FormIdent+' : public '+FAncestorClass.ClassName+CRLF+
      '{'+CRLF+
      '__published:'+CRLF+
      'private:'+CRLF+
      'protected:'+CRLF+
      'public:'+CRLF+
      '        __fastcall T'+FormIdent+'(TComponent* Owner);'+CRLF+
      '};'+CRLF;

  s:=s+
     '//---------------------------------------------------------------------------'+
     CRLF+
     'extern PACKAGE T'+FormIdent+' *'+FormIdent+';'+CRLF;

  s:=s+
     '//---------------------------------------------------------------------------'+
     CRLF+
     '#endif';
  Result:=s;
end;

function TrmToolWinFormModuleCreator.NewModuleSource(const UnitIdent, FormIdent,
      AncestorIdent: string): string;
var
  s : string;
begin

  s:='//---------------------------------------------------------------------------'+
     CRLF+
     '#include <vcl.h>'+CRLF;

  s:=s+
     '#pragma hdrstop'+CRLF2+

     '#include "'+UnitIdent+'.h"'+CRLF+
     '//---------------------------------------------------------------------------'+
     CRLF+
     '#pragma package(smart_init)'+CRLF;

  if (AncestorIdent<>'Form') and  (AncestorIdent<>'DataModule') then
    s:=s+
     '#pragma link "'+GetCustomFormUnit(FAncestorClass.ClassName)+'"'+CRLF;

  s:=s+
   '#pragma resource "*.dfm"'+CRLF+
   'T'+FormIdent+' *'+FormIdent+';'+CRLF;

  s:=s+
     '//---------------------------------------------------------------------------'+
     CRLF+
     '__fastcall T'+FormIdent+'::T'+FormIdent+'(TComponent* Owner)'+CRLF+
     '        : '+FAncestorClass.ClassName+'(Owner)'+CRLF+
     '{'+CRLF+
     '}'+CRLF+
     '//---------------------------------------------------------------------------'+
     CRLF;

  Result:=s;
end;

{$ELSE}
  {$IFDEF VER100}
function TrmToolWinFormModuleCreator.NewModuleSource(UnitIdent,FormIdent,AncestorIdent:string):string;
  {$ELSE}
function TrmToolWinFormModuleCreator.NewModuleSource(const UnitIdent,FormIdent,AncestorIdent:string):string;
  {$ENDIF}
var
  s : string;
begin
  s:='unit '+FUnitIdent+';'+CRLF2+
     'interface'+CRLF2+
     'uses'+CRLF+
     '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs';

  if (FAncestorIdent<>'Form') and (FAncestorIdent<>'DataModule') then
    s:=s+','+CRLF+
//      '  '+GetCustomFormUnit(FAncestorClass.ClassName);
      '  rmToolWin';
      
  s:=s+';'+CRLF2+
    'type'+CRLF+
//    '  T'+FFormIdent+' = class('+FAncestorClass.ClassName+')'+CRLF+
    '  T'+FFormIdent+' = class(TrmToolWinForm)'+CRLF+
    '  private'+CRLF+
    '    { Private declarations }'+CRLF+
    '  public'+CRLF+
    '    { Public declarations }'+CRLF+
    '  end;'+CRLF2;

  s:=s+
    'var'+CRLF+
    '  '+FFormIdent+' : T'+FFormIdent+';'+CRLF2;

  s:=s+
      'implementation'+CRLF2;

  s:=s+
    '{$R *.DFM}'+CRLF2;

  s:=s+
      'end.';

  Result:=s;
end;
{$ENDIF}

procedure TrmToolWinFormModuleCreator.FormCreated(Form:TIFormInterface);
begin
end;

{ HandleException }

procedure HandleException;
begin
  ToolServices.RaiseException(ReleaseException);
end;

{ TrmToolWinFormExpert }

function TrmToolWinFormExpert.GetName: string;
begin
  try
    Result := sCustFormExpertName;
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetComment: string;
begin
  try
    Result := sCustFormExpertDesc;
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetGlyph: HICON;
begin
  try
    Result := LoadIcon(HInstance, 'NEWRMDOCKFORM');
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetStyle: TExpertStyle;
begin
  try
    Result := esForm;
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetState: TExpertState;
begin
  try
    Result := [esEnabled];
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetIDString: string;
begin
  try
    Result := 'MillsEnterprise.'+sCustFormExpertName;
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetMenuText: string;
begin
   try
     result := '';
   except
     HandleException;
   end;
end;

function TrmToolWinFormExpert.GetAuthor: string;
begin
  try
    Result := sCustFormExpertAuthor;
  except
    HandleException;
  end;
end;

function TrmToolWinFormExpert.GetPage: string;
begin
  try
    Result := 'New';
  except
    HandleException;
  end;
end;

procedure TrmToolWinFormExpert.Execute;
begin
  try
    RunExpert(ToolServices);
  except
    HandleException;
  end;
end;

procedure TrmToolWinFormExpert.RunExpert(ToolServices: TIToolServices);
var
  ModuleFlags : TCreateModuleFlags;
  IModuleCreator : TrmToolWinFormModuleCreator;
  IModule : TIModuleInterface;
begin
  if ToolServices = nil then Exit;
  IModuleCreator:=TrmToolWinFormModuleCreator.Create;
  IModuleCreator.FAncestorIdent:='rmToolWinForm';
  IModuleCreator.FAncestorClass:=TrmToolWinForm;
  ToolServices.GetNewModuleAndClassName(IModuleCreator.FAncestorIdent,IModuleCreator.FUnitIdent,IModuleCreator.FFormIdent,IModuleCreator.FFileName);
  ModuleFlags:=DefaultModuleFlags;
  ModuleFlags:=ModuleFlags+[cmAddToProject];
  try
{$IFDEF VER_CB}
    IModule:=ToolServices.ModuleCreateEx(IModuleCreator,ModuleFlags);
{$ELSE}
    IModule:=ToolServices.ModuleCreate(IModuleCreator,ModuleFlags);
{$ENDIF}
    IModule.Free;
  finally
    IModuleCreator.Free;
  end;
end;

end.


