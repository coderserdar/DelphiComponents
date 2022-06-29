unit DBSpinEdit;
(*
TDBSpinEdit,(c) 2000-2005 Brad Prendergast (bradp@bpsoftware.com),
            http://www.bpsoftware.com/products/delphi.htm

Version: 1.5.2.0, Aug 05, 2005

TDBSpinEdit is a data-aware descendant of native TSpinEdit control.


New:
  Properties -
    DataField: String - Specifies the field which the DBSpinEdit control displays data.
    DataSource: TDataSource - Links the DBSpinEdit to the dataset that contains the 
                              field it represents.


There is no guarantee or warranty, expressed or implied, concerning the applicability of 
code and techniques included in this example.  This example code is supplied AS IS.  If
you wish to use this code or technique, it is your responsibility to test and certify 
the code in your project.
*)
interface

uses
  Windows, Messages, SysUtils, Classes,Graphics, Controls, Spin,
  StdCtrls, DB, DBCtrls, {$IFDEF VER170}Types, Variants, {$ENDIF}
{$IFDEF WIN32}
  Forms, Dialogs;
{$ELSE}
   System.ComponentModel;
{$ENDIF}

type
  TDBSpinEdit = class(TSpinEdit)
  private
    { Private declarations }
    fDataLink : TFieldDataLink;
    Function GetDataField: string;
    Function GetDataSource: TDataSource;
    Procedure SetDataField(const Value: string);
    Procedure SetDataSource(Value: TDataSource);
    Procedure DataChange(Sender: TObject);
    Procedure DataUpdate(Sender: TObject);
    Procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    { Published declarations }
    Property DataField: string read GetDataField write SetDataField;
    Property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

procedure Register;

implementation

Constructor TDBSpinEdit.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  fDataLink:= TFieldDataLink.Create;
  fDataLink.Control:= Self;
  fDataLink.OnDataChange:= DataChange;
  fDataLink.OnUpdateData:= DataUpdate;
End;

Destructor TDBSpinEdit.Destroy;
Begin
  fDataLink.OnDataChange:= nil;
  fDataLink.OnUpdateData:= nil;
  fDataLink.Free;
  fDataLink:= nil;
  inherited;
End;

Function TDBSpinEdit.GetDataField: string;
Begin
  Result:= fDataLink.FieldName;
End;

Function TDBSpinEdit.GetDataSource: TDataSource;
Begin
  Result:= fDataLink.DataSource;
End;

Procedure TDBSpinEdit.SetDataField(const value: string);
Begin
  fDataLink.FieldName:= Value;
End;

Procedure TDBSpinEdit.SetDataSource(Value: TDataSource);
Begin
  fDataLink.DataSource:= Value;
End;

Procedure TDBSpinEdit.DataChange(Sender: TObject);
Begin
  if fDataLink.DataSource.State = dsBrowse then
    if (fDataLink.Field.AsInteger <> null) then
      Value:= fDataLink.Field.AsInteger
    else
      Value:= 0;
End;

Procedure TDBSpinEdit.DataUpdate(Sender: TObject);
Begin
  if fDataLink.CanModify then
    fDataLink.Field.AsInteger:= Value;
End;

Procedure TDBSpinEdit.Change;
Begin
  If Modified then
    begin
      if not(fDataLink.Editing) then
        fDataLink.Edit;
      fDatalink.Modified;
      inherited Change;
    end;
End;

procedure TDBSpinEdit.CMExit(var Message: TWMNoParams);
begin
   try
      fDataLink.UpdateRecord;
   except
      on Exception do SetFocus;
   end;
   inherited;
end;

procedure TDBSpinEdit.UpClick (Sender: TObject);
begin
  Inherited UpClick(Sender);
     if not(fDataLink.Editing) then
        fDataLink.Edit;
      fDatalink.Modified;
end;

procedure TDBSpinEdit.DownClick (Sender: TObject);
begin
  Inherited DownClick(Sender);
  if not(fDataLink.Editing) then
    fDataLink.Edit;
  fDatalink.Modified;
end;

procedure Register;
begin
  RegisterComponents('BPComponents', [TDBSpinEdit]);
end;

end.
