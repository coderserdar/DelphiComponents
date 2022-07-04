{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TConverterEditor                              }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgConvEd;

interface

uses
  Classes, Graphics, Controls, Forms, vgItems, vgDBConv, Menus,
  StdCtrls, vgTools, Explorer, ComCtrls, ExplCtrl;

type
  TConverterEditorForm = class(TItemsEditorForm)
  private
    { Private declarations }
    procedure NewConverter;
  public
    { Public declarations }
    procedure GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut); override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TConverterEditor = class(TItemsEditor)
  public
    function GetFormClass: TItemsEditorFormClass; override;
  end;

var
  ConvertItemClass: TDBConvertItemClass = TDBConvertItem;

implementation
uses ConvAdd;

function TConverterEditor.GetFormClass: TItemsEditorFormClass;
begin
  Result := TConverterEditorForm;
end;

procedure TConverterEditorForm.GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut);
begin
  case Index of
    0:
      begin
        Caption := '&New...';
        ShortCut := Menus.ShortCut(ord('N'), [ssCtrl]);
      end;
    1:
      begin
        Caption := '&Add...';
        ShortCut := Menus.ShortCut(ord('A'), [ssCtrl]);
      end;
  else
    inherited GetVerb(Index - 2, Caption, ShortCut);
  end;
end;

{ TConverterEditorForm }
function TConverterEditorForm.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

procedure TConverterEditorForm.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: NewConverter;
    1: EditConverter(TDBConverter(ItemList), Designer);
  else
    inherited ExecuteVerb(Index - 2);
  end;
end;

procedure TConverterEditorForm.NewConverter;
var
  Item: TDBConvertItem;
begin
  Item := TDBConvertItem(Designer.CreateComponent(ConvertItemClass, TDBConverter(ItemList), 0, 0, 0, 0));
  try
    Item.DesignInfo := 0;
    Item.Converter := TDBConverter(ItemList);
    Designer.Modified;
  except
    Item.Free;
    raise;
  end;
end;

end.
