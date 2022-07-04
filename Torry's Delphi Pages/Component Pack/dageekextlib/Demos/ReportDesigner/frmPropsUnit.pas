{*******************************************************}
{                                                       }
{       Report Designer                                 }
{       Extension Library example of                    }
{       TELDesigner, TELDesignPanel                     }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit frmPropsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QuickRpt, QRCtrls, StdCtrls, ELDsgnr, Buttons, ExtCtrls, ComCtrls, QRPrntr,
  Grids, ELPropInsp, TypInfo, DB;

type
  TfrmProps = class(TForm)
    PropInsp: TELPropertyInspector;
    procedure PropInspModified(Sender: TObject);
    procedure PropInspFilterProp(Sender: TObject; AInstance: TPersistent;
      APropInfo: PPropInfo; var AIncludeProp: Boolean);
  private
    FDoc: TForm;
    procedure SetDoc(const Value: TForm);
    { Private declarations }
  public
    { Public declarations }
    property Doc: TForm read FDoc write SetDoc;
  end;

var
  frmProps: TfrmProps;

implementation

uses frmDocUnit;

{$R *.dfm}


{ TfrmProps }

procedure TfrmProps.SetDoc(const Value: TForm);
begin
  FDoc := Value;
end;

procedure TfrmProps.PropInspModified(Sender: TObject);
begin
  if FDoc <> nil then
    TfrmDoc(FDoc).Modify;
end;

procedure TfrmProps.PropInspFilterProp(Sender: TObject;
  AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
  if (APropInfo.PropType^.Kind = tkClass) and
    (GetTypeData(APropInfo.PropType^).ClassType.InheritsFrom(TDataSet) or
    GetTypeData(APropInfo.PropType^).ClassType.InheritsFrom(TQuickRepBands)) then
    AIncludeProp := False;
end;

end.
