{*******************************************************}
{                                                       }
{       Readme display component                        }
{       Freeware                                        }
{       Copyright (C) 1998 Jaro Benes                   }
{       All right reserved                              }
{       E-mail: ijcro@micrel.cz                         }
{                                                       }
{*******************************************************}

unit Readme;

{If you make any modification, please, let me know}
{Comments or suggestions are welcome}

{description:}
{-component for show the README file or build in readme from list}
{property:}
{header - title of readme}
{legalcopyright - extra line for copyright display}
{name - name of component}
{readme - here is store build in list of readme (without file)}
{readmefile - this is prefered of readme property, if used, reading file from
 disk (file length limit is 32K for memo component) if is empty, use readme
 stringlist property}
{method:}
{execute - simply call, returned true if user accept this readme file}
{instruction:}
 {1. install it into library}
 {2. drop component to form}
 {3. setup Header,LegalCopyright, Readme or  ReadmeFile property}
 {4. attached it execute method from your event}
 {5. run it}
interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ReadMeDlg;

type
  TReadme = class(TComponent)
  private
    FHeader: string;
    FTitleFrm: string;
    FLegalCopyright: string;
    FReadmeFile: string;
    FBtnAcceptCaption: string;
    FBtnRefuseCaption: string;
    //FShowAtStart: boolean;
    FReadme: TStringList;
    procedure SetReadmeList(Value: TStringList);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean;
  published
    property ReadmeFile: string read FReadmeFile write FReadmeFile;
    property Header: string read FHeader write FHeader;
    property LegalCopyright: string read FLegalCopyright write FLegalCopyright;
    property BtnAcceptCaption: string read FBtnAcceptCaption write FBtnAcceptCaption;
    property BtnRefuseCaption: string read FBtnRefuseCaption write FBtnRefuseCaption;
    property TitleFrm: string read FTitleFrm write FTitleFrm;
    property Readme: TStringList read FReadme write SetReadmeList;
  end;

implementation

{  TReadme  }

constructor TReadme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Create TStringList to store build in Readme }
  FReadme := TStringList.Create;
end;

destructor TReadme.Destroy;
begin
  { Free TStringList }
  FReadme.Free;
  inherited Destroy;
end;

procedure TReadme.SetReadmeList(Value: TStringList);
begin
  if FReadme <> Value then FReadme.Assign(Value)
end;

function TReadme.Execute: Boolean;
begin
  { Call dialog and show it }
  Result := ReadmeDlgExecute(FTitleFrm, FHeader, FBtnAcceptCaption, FBtnRefuseCaption,
    FLegalCopyright,
    ReadmeFile, FReadme);
  {if result is true ---> user accept this file}
end;

end.

