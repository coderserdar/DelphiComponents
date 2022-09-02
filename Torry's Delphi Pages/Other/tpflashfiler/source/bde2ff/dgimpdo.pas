{*********************************************************}
{* Progress meter for import operations                  *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit dgimpdo;

interface

uses
  Windows,
  SysUtils,
  Dialogs,
  Classes,
  DBTables,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  DB,
  Buttons,
  ExtCtrls,
  Gauges,
  dbconsts,
  bde,
  bdeconst,
  ffllbase,
  ffsrbde,
  ffdb,
  ffdbbase;

type
  TdlgImportProgress = class(TForm)
    Bevel1: TBevel;
    lblProgress: TLabel;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    edtImportFilename: TEdit;
    edtTablename: TEdit;
    guaProgress: TGauge;
    procedure btnCancelClick(Sender: TObject);
  private
  public
    Terminated : Boolean;

    procedure ShowProgress(aImportFilename, aTableName : string);
    procedure UpdateProgress(aNumRead, aTotalRecs : Longint);
  end;

procedure ConvertBDEDataType(aDataType : TFieldType;
                             aSize     : LongInt;
                         var aFFType   : TffFieldType;
                         var aFFSize   : LongInt;
                         var aFFDecPl  : Integer);

function DoImport(aSourceTable    : TTable;             { Table to copy from }
                  aSourceFields   : TStringList;        { List of field #'s to copy }
                  aDestTable      : TffTable;           { Table to copy to }
                  aBlockInserts   : SmallInt;           { Transaction batch size }
              var aNumTransferred : LongInt): Boolean;  { Number of records copied }

var
  dlgImportProgress : TdlgImportProgress;

implementation

{$R *.DFM}

uses
  ffclintf,
  fmmain;

procedure ConvertBDEDataType(aDataType : TFieldType;
                             aSize     : LongInt;
                         var aFFType   : TffFieldType;
                         var aFFSize   : LongInt;
                         var aFFDecPl  : Integer);
begin
  aFFSize := aSize;
  aFFDecPl := 0;
  case aDatatype of
    {$IFDEF DCC4OrLater}
    ftFixedChar,
    {$ENDIF}
    ftString :
{Begin !!.01}
      if aSize <= 255 then begin
{Begin !!.11}
        if frmMain.chkUseANSIFields.Checked then begin
          if frmMain.chkUseZeroTerminatedStrings.Checked then
            aFFType := fftNullAnsiStr
          else
            aFFType := fftShortAnsiStr
        end
        else begin
          if frmMain.chkUseZeroTerminatedStrings.Checked then
            aFFType := fftNullString
          else
            aFFType := fftShortString;
        end
{End !!.11}
      end
      else begin
        if frmMain.chkUseANSIFields.Checked then
          aFFType := fftNullAnsiStr
        else
          aFFType := fftNullString;
      end;
{End !!.01}
    ftSmallint:
      aFFType := fftInt16;
    ftInteger:
      aFFType := fftInt32;
    ftWord:
      aFFType := fftWord16;
    ftBoolean:
      aFFType := fftBoolean;
    ftFloat:
      aFFType := fftDouble;
    ftCurrency:
      aFFType := fftCurrency;
    ftBCD:
      aFFType := fftDouble;
    ftDate:
{Begin !!.11}
      if frmMain.chkUseSysToolsDates.Checked then
        aFFType := fftStDate
      else
        aFFType := fftDateTime;
{End !!.11}
    ftTime:
{Begin !!.11}
      if frmMain.chkUseSysToolsTimes.Checked then
        aFFType := fftStTime
      else
        aFFType := fftDateTime;
{End !!.11}
    ftDateTime:
      aFFType := fftDateTime;
    ftBytes,
    ftVarBytes:
      aFFType := fftByteArray;
    ftBlob:
      aFFType := fftBLOB;
    ftMemo:
      aFFType := fftBLOBMemo;
    ftGraphic:
      aFFType := fftBLOBGraphic;
    ftAutoInc:
      aFFType := fftAutoInc;
    ftFmtMemo:
      aFFType := fftBLOBFmtMemo;
    ftParadoxOle,
    ftDBaseOle:
      aFFType := fftBLOBOleObj;
    ftTypedBinary:
      aFFType := fftBLOBTypedBin;
  end;
end;

function DoImport(aSourceTable    : TTable;
                  aSourceFields   : TStringList;
                  aDestTable      : TffTable;
                  aBlockInserts   : SmallInt;
              var aNumTransferred : LongInt) : Boolean;

resourcestring
  SInvalidFieldKind = 'Invalid Field Conversion  %s <- %s';
var
  FieldNo     : Integer;
  DestFieldNo : Integer;
  TotalRecs   : Longint;
  DoThisOne   : Boolean;
  DoExplicitTrans : Boolean;
  InTransaction   : Boolean;
  MaxAutoInc      : Integer;
  TempStr     : string;                                                {!!.01}

  procedure CopyField(aDestField, aSourceField : TField);
  var
    Buffer : Pointer;
    Stream : TMemoryStream;
  begin
{Begin !!.11}
    if aSourceField.IsNull then begin
      if frmMain.chkEmptyStrings.Checked and
         (aSourceField.Datatype = ftString) then
        aDestField.AsString := ''
      else
        aDestField.Clear;
    end
    else
{End !!.11}
    case aSourceField.Datatype of
      ftBoolean:
        case aDestField.Datatype of
          ftBoolean:
            aDestField.AsBoolean := aSourceField.AsBoolean;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftString:
        case aDestField.Datatype of
          ftString:
            begin
{Begin  !!.11}
              if frmMain.chkClearEmptyStrings.Checked then begin
                TempStr := aSourceField.AsString;
                if TempStr = '' then
                  aDestField.Clear
                else
                  aDestField.AsString := TempStr;
              end
              else
                aDestField.AsString := aSourceField.AsString;
{End !!.11}
{Begin !!.01}
              if frmMain.chkOEMAnsi.Checked and
                 (Length(aDestField.AsString) > 0) then begin
                SetLength(TempStr, Length(aDestField.AsString));
                tempStr := aDestField.AsString;
                OEMToCharBuff(PChar(tempStr), PChar(tempStr), Length(aDestField.AsString));
                aDestField.AsString := tempStr;
              end;
{End !!.01}
            end;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftAutoInc,
      ftSmallint,
      ftInteger,
      ftWord:
        case aDestField.Datatype of
          ftSmallInt,
          ftInteger,
          ftAutoInc,
          ftWord:
            begin
              aDestField.AsInteger := aSourceField.AsInteger;
              if (aDestField.Datatype = ftAutoInc) and
                 (aDestField.AsInteger > MaxAutoInc) then
                MaxAutoInc := aDestField.AsInteger;
            end;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftBCD,
      ftFloat,
      ftCurrency:
        case aDestField.Datatype of
          ftFloat,
          ftCurrency:
            aDestField.AsFloat := aSourceField.AsFloat;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftDate:
        case aDestField.Datatype of
          ftDate,
          ftDateTime:
            aDestField.AsDateTime := aSourceField.AsDateTime;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftTime:
        case aDestField.Datatype of
          ftTime,
          ftDateTime:
            aDestField.AsDateTime := aSourceField.AsDateTime;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftDateTime:
        case aDestField.Datatype of
          ftDate,
          ftTime,
          ftDateTime:
            aDestField.AsDateTime := aSourceField.AsDateTime;
          else
            DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
              aSourceField.DisplayName]);
        end;
      ftBytes,
      ftVarBytes:
        begin
          GetMem(Buffer, aDestField.DataSize);
          try
            case aDestField.Datatype of
              ftBytes,
              ftVarBytes:
                if aSourceField.GetData(Buffer) then
                  aDestField.SetData(Buffer)
                else
                  aDestField.SetData(nil);
              ftFmtMemo,
              ftParadoxOle,
              ftDBaseOle,
              ftTypedBinary,
              ftMemo,
              ftGraphic,
              ftBlob:
                if not aSourceField.GetData(Buffer) then
                  aDestField.SetData(nil)
                else begin
                  Stream := TMemoryStream.Create;
                  try
                    Stream.Write(Buffer^, aSourceField.DataSize);
                    TBLOBField(aDestField).LoadFromStream(Stream);
                  finally
                    Stream.Free;
                  end;
                end;
              else
                DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                  aSourceField.DisplayName]);
            end;
          finally
            FreeMem(Buffer, aDestField.DataSize);
          end;
        end;
      ftFmtMemo,
      ftParadoxOle,
      ftDBaseOle,
      ftTypedBinary,
      ftMemo,
      ftGraphic,
      ftBlob:
        begin
          case aDestField.Datatype of
            ftFmtMemo,
            ftParadoxOle,
            ftDBaseOle,
            ftTypedBinary,
            ftMemo,
            ftGraphic,
            ftBlob:
              begin
                Stream := TMemoryStream.Create;
                try
                  TBLOBField(aSourceField).SaveToStream(Stream);
                  TBLOBField(aDestField).LoadFromStream(Stream);
                finally
                  Stream.Free;
                end;
              end;
            else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          end;
        end;
      ftUnknown:
        DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
          aSourceField.DisplayName]);
    end;
  end;

begin
  Result := False;
  with dlgImportProgress do begin
    Terminated := False;
    ShowProgress(aSourceTable.TableName, aDestTable.TableName);
    try

      { If we only have one insert per transaction, then let the server
        do implicit transactions; it'll be faster }
      if aBlockInserts = 0 then aBlockInserts := 1;
      DoExplicitTrans := (aBlockInserts > 1);

      aSourceTable.Open;
      try
        TotalRecs := aSourceTable.RecordCount;
        aNumTransferred := 0;

        aDestTable.Open;
        if (DoExplicitTrans) then                                      {!!.05}
          DoExplicitTrans := (not aDestTable.Dictionary.HasBLOBFields);{!!.05}
        try
          MaxAutoInc := 0;
          InTransaction := False;
          try
            while not aSourceTable.EOF do begin
//              UpdateProgress(aNumTransferred + 1, TotalRecs);        {Deleted !!.01}

              { Blocks inserts within a transaction }
              if DoExplicitTrans and not InTransaction then begin
                frmMain.dbDest.StartTransaction;
                InTransaction := True;
              end;

              aDestTable.Insert;

              { Copy fields one at a time }
              for FieldNo := 0 to aSourceTable.FieldCount - 1 do begin

                { Do only selected fields }
                DoThisOne := not Assigned(aSourceFields);
                if not DoThisOne then begin
                  DoThisOne := aSourceFields.IndexOf(ANSIUppercase(aSourceTable.Fields[FieldNo].FieldName)) <> -1;
                end;

                if DoThisOne then begin

                  { Fields might be in order, avoid expensive FieldByName }
                  if (FieldNo < aDestTable.FieldCount) and
                     (FFCmpShStrUC(aSourceTable.Fields[FieldNo].FieldName,
                                   aDestTable.Fields[FieldNo].FieldName,
                                   255) = 0) then
                    DestFieldNo := FieldNo
                  else begin
                    try
                      DestFieldNo := aDestTable.FieldByName(aSourceTable.Fields[FieldNo].FieldName).FieldNo - 1;
                    except
                      DestFieldNo := -1;
                    end;
                  end;

                  if DestFieldNo <> -1 then
                    try
{Begin !!.11}
//                    aDestTable.Fields[DestFieldNo].Assign(aSourceTable.Fields[FieldNo]);
{Begin !!.01}
//                    if frmMain.chkOEMAnsi.Checked and
//                       (aDestTable.Fields[DestFieldNo].Datatype = ftString) and
//                       (Length(aDestTable.Fields[DestFieldNo].AsString) > 0) then begin
//                      SetLength(TempStr, Length(aDestTable.Fields[DestFieldNo].AsString));
//                      tempStr := aDestTable.Fields[DestFieldNo].AsString;
//                      OEMToCharBuff(PChar(tempStr), PChar(tempStr), Length(aDestTable.Fields[DestFieldNo].AsString));
//                      aDestTable.Fields[DestFieldNo].AsString := tempStr;
//                    end;
{End !!.01}
                    CopyField(aDestTable.Fields[DestFieldNo], aSourceTable.Fields[FieldNo]);
{End !!.11}
                    if (aDestTable.Fields[DestFieldNo].Datatype = ftAutoInc) and
                      (aDestTable.Fields[DestFieldNo].AsInteger > MaxAutoInc) then
                      MaxAutoInc := aDestTable.Fields[DestFieldNo].AsInteger;
                    except
                      on E:EDatabaseError do begin
                        CopyField(aDestTable.Fields[DestFieldNo], aSourceTable.Fields[FieldNo]);
                      end;
                      else
                        raise;
                    end;
                end;
              end;

              aDestTable.Post;
              Inc(aNumTransferred);  { Increment after successfully posting }

              { See if it's time to commit the transaction }
{Begin !!.01}
              if InTransaction then begin
                if ((aNumTransferred mod aBlockInserts) = 0) then begin
                  aDestTable.Database.Commit;
                  UpdateProgress(aNumTransferred, TotalRecs);
                  InTransaction := False;
                end
              end
              else
                UpdateProgress(aNumTransferred + 1, TotalRecs);
{End !!.01}

              { Check for user termination }
              if Terminated then begin
                if InTransaction then
                  aDestTable.Database.Rollback;
                Exit;
              end;

              aSourceTable.Next;
            end;

            {update the maximum autoinc value for the dest table}
            aDestTable.SetTableAutoIncValue(MaxAutoInc);
            { Residual inserts need to be posted? }
            if InTransaction then begin                                {!!.01}
              aDestTable.Database.Commit;
              UpdateProgress(aNumTransferred + 1, TotalRecs);          {!!.01}
            end;                                                       {!!.01}
          except
            if InTransaction then
              aDestTable.Database.Rollback;
            raise;
          end;
        finally
          aDestTable.Close;
        end;
      finally
        aSourceTable.Close;
      end;
    finally
      Hide;
    end;
    Result := not Terminated;
  end;
end;

procedure TdlgImportProgress.ShowProgress(aImportFilename, aTableName : string);
begin
  edtImportFilename.Text := aImportFilename;
  edtTablename.Text := aTableName;
  lblProgress.Hide;
  guaProgress.Progress := 0;
  inherited Show;
  Application.ProcessMessages;
end;

procedure TdlgImportProgress.UpdateProgress(aNumRead, aTotalRecs: LongInt);
var
  Dividend : LongInt;
  Divisor  : LongInt;
resourcestring
  SProgressStatus = 'Processing record %d of %d';
begin
  with lblProgress do begin
    Caption := Format(SProgressStatus, [aNumRead, aTotalRecs]);
    Show;
    Application.ProcessMessages;
  end;

  { Calculate % completed }
  if (aNumRead >= $1000000) then begin
    Dividend := (aNumRead shr 7) * 100;
    Divisor := aTotalRecs shr 7;
  end
  else begin
    Dividend := aNumRead * 100;
    Divisor := aTotalRecs;
  end;

  if Divisor <> 0 then
    guaProgress.Progress := Dividend div Divisor;
end;

procedure TdlgImportProgress.btnCancelClick(Sender: TObject);
resourcestring
  SAbortMsg = 'Abort transferring data?';
begin
  Terminated := MessageDlg(SAbortMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

end.
