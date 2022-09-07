{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Custom Containers Pack (CCPack)                 }
{                                                       }
{       Copyright (c) 1997-99, Sergey Orlik             }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       e-mail:  sorlik@inprise.ru                      }
{       WWW: http://www.inprise.ru                      }
{                                                       }
{       Personal Home Page:                             }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

unit ccwdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, ImgList;

type
  TCCWDialog = class(TForm)
    CCtree: TTreeView;
    CCtext: TStaticText;
    CCimage: TImage;
    CCimagelist: TImageList;
    Bevel1: TBevel;
    CCradio: TRadioGroup;
    CClabel: TLabel;
    CCfinish: TBitBtn;
    CCcancel: TBitBtn;
    CCstatus: TStatusBar;
    CCeditClassName: TEdit;
    CClabel2: TLabel;
    procedure CCtreeChange(Sender: TObject; Node: TTreeNode);
    procedure CCradioClick(Sender: TObject);
  private
    function GetContainerNode(AClassName:string):TTreeNode;
    procedure SelectImage;
    procedure LoadNodes;
  protected
    procedure Loaded; override;
  public
    { Public declarations }
  end;

var
  CCWDialog: TCCWDialog;

implementation

uses
  ccreg;

{$R *.DFM}

function TCCWDialog.GetContainerNode(AClassName:string):TTreeNode;
begin
  Result := CCtree.Items.GetFirstNode;
  while (Result <> nil) do
  begin
    if AnsiCompareText(Result.Text,AClassName)=0 then Break;
    Result:=Result.GetNext;
  end;
end;

procedure TCCWDialog.SelectImage;
var
  BMP: TBitmap;
  Index: integer;
begin
  Index:=GetBaseContainer(TComponentClass(CCtree.Selected.Data));
  if Index=-1 then
    Exit;
  BMP:=TBitmap.Create;
  CCtext.Caption:=CCtree.Selected.Text;
  CCimagelist.GetBitmap(Index+6*CCradio.ItemIndex,BMP);
  CCimage.Picture.Bitmap.Assign(BMP);
  BMP.Free;
end;

procedure TCCWDialog.CCtreeChange(Sender: TObject; Node: TTreeNode);
begin
  SelectImage;
end;

procedure TCCWDialog.CCradioClick(Sender: TObject);
begin
  SelectImage;
end;

procedure TCCWDialog.LoadNodes;
var
  I: integer;
  AClass : TComponentClass;
begin
  with CCTree.Items do
    for I:=0 to GetCustomContainerClassListCount-1 do
    begin
      AClass:=GetCustomContainerClass(I);
      if IsBaseContainer(AClass)<>-1 then
        AddObject(nil,AClass.ClassName,AClass)
      else
        AddChildObject(GetContainerNode(AClass.ClassParent.ClassName),AClass.ClassName,AClass);
    end;
end;

procedure TCCWDialog.Loaded;
begin
  inherited;
  with CCtree.Items do
  begin
    LoadNodes;
    GetFirstNode.Selected:=True;
  end;
end;

end.
