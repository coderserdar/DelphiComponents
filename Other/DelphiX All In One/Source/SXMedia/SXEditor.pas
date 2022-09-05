{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
}
unit SXEditor;

{$INCLUDE DelphiXcfg.inc}

interface

uses Windows, Classes,
  {$IfNDef VER6UP} DsgnIntf, {$Else} Designintf, DesignEditors, {$EndIf}
  Dialogs, SXMovie, SXModPlayer, SXEngine, SXAbout;

const

 SXMOVIE_FILTER = 'All Media Files|*.avi;*.mpg;*.mov|' +
                     'AVI (*.avi)|*.avi|MPG (*.mpg)|*.mpg|MOV (*.mov)|*.mov';
 SXMODPLAYER_FILTER = 'All Media Files|*.mod;*.it;*.sm3|' +
                     'Impulse Tracker (*.it)|*.it|Scream Tracker (*.s3m)|*.s3m|Fast Tracker (*.xm)|*.xm';

type

TFilenameProperty = class(TPropertyEditor)
  function  GetAttributes : TPropertyAttributes; override;
  function  GetValue : string; override;
  procedure SetValue(const Value : string); override;
  procedure Edit; override;
end;

TSXComponentEditor = class(TComponentEditor)
   procedure ExecuteVerb(Index: Integer); override;
   function GetVerb(Index: Integer): string; override;
   function GetVerbCount: Integer; override;
end;

implementation

{--------------------------------------}
{ TFilename Property Editor            }
{--------------------------------------}
procedure TFilenameProperty.Edit;
var
  Dialog : TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  with Dialog do
    try
      DefaultExt := 'All Media Files';
      if GetComponent(0) is TSXMovie then
         Filter := SXMOVIE_FILTER
      else
      if GetComponent(0) is TSXModPlayer then
         Filter := SXMODPLAYER_FILTER
      else
         Exit;
      if Dialog.Execute then
        begin
          SetStrValue(Dialog.FileName);
          Designer.Modified;
        end;
    finally
      Free;
    end;
end;

function TFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TFilenameProperty.GetValue : string;
begin
  Result := GetStrValue;
end;

procedure TFilenameProperty.SetValue(const Value : string);
begin
  SetStrValue(Value);
  Designer.Modified;
end;

procedure TSXComponentEditor.ExecuteVerb(Index: Integer);
begin
  with TAboutBox.Create(nil) do
  begin
     try
        ShowModal;
     finally
        Free;
     end;
  end;
end;
function TSXComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'A&bout SXMedia';
end;
function TSXComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
