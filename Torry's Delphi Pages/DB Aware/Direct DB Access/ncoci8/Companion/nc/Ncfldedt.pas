{***********************************************}
{File:      NCFldEdt.PAS                        }
{Revision:  1.01 / 06.02.2000                   }
{Comment:   Fields property editor dialog       }
{Copyright: (c) 1997-2000, Dmitry Arefiev       }
{Author:    Dmitry Arefiev, dmitrya@inthink.com }
{***********************************************}
{$I NCOciDef.inc}

unit NCFldEdt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, NCDblLst, StdCtrls, Buttons, DB;

type
  TNCFieldsEditFrm = class(TForm)
    lbFields: TNCDblListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetFields: String;
  public
    { Public declarations }
    property Fields: String read GetFields;
    procedure SetDSFields(ADataSet: TDataSet; AFields: String);
  end;

  function EditFields(var AFields: String; ADataSet: TDataSet; ACaption: String): Boolean;

var
  NCFieldsEditFrm: TNCFieldsEditFrm;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{$R *.DFM}

Uses NCUIUtil, NCStrs;

procedure TNCFieldsEditFrm.FormCreate(Sender: TObject);
begin
    IUStr2Font(SDefaultFont, Font);
end;

function TNCFieldsEditFrm.GetFields: String;
var
    i: Integer;
begin
    Result := '';
    for i := 0 to lbFields.DstItems.Count - 1 do begin
        if Result <> '' then
            Result := Result + ';';
        Result := Result + lbFields.DstItems[i];
    end;
end;

procedure TNCFieldsEditFrm.SetDSFields(ADataSet: TDataSet; AFields: String);
var
    j, i: Integer;
    s: String;
begin
    lbFields.DstItems.BeginUpdate;
    lbFields.SrcItems.BeginUpdate;
    try
        lbFields.DstItems.Clear;
        lbFields.SrcItems.Clear;
        if ADataSet <> nil then begin
            ADataSet.GetFieldNames(lbFields.SrcItems);
            i := 1;
            while i <= Length(AFields) do begin
                s := ExtractFieldName(AFields, i);
                j := lbFields.SrcItems.IndexOf(s);
                if j <> -1 then begin
                    lbFields.SrcItems.Delete(j);
                    lbFields.DstItems.Add(s);
                end;
            end;
        end;
    finally
        lbFields.DstItems.EndUpdate;
        lbFields.SrcItems.EndUpdate;
    end;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function EditFields(var AFields: String; ADataSet: TDataSet; ACaption: String): Boolean;
begin
    Result := False;
    if ADataSet = nil then
        Exit;
    if ACaption = '' then
        ACaption := SSFCaption;
    if NCFieldsEditFrm = nil then
        Application.CreateForm(TNCFieldsEditFrm, NCFieldsEditFrm);
    with NCFieldsEditFrm do
    try
        SetDSFields(ADataSet, AFields);
        Caption := ACaption;
        Result := ShowModal = mrOK;
        if Result then
            AFields := Fields;
    finally
        Free;
        NCFieldsEditFrm := nil;
    end;
end;

end.
