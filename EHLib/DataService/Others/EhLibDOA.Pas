{*******************************************************}
{                                                       }
{                       EhLib v3.0                      }
{     Register object that sort data in TOracleDataset  }
{     from Direct Oracle Access (DOA) library by        }
{     Allround Automations.                             }
{     See detail info about DOA at                      }
{     http://www.allroundautomations.nl                 }
{                                                       }
{      Copyright (c) 2003 by Dmitry V. Bolshakov        }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TOracleDataset automatically after sorting markers    }
{ will be changed.                                      }
{ TSQLDatasetFeaturesEh will try to find line in        }
{ TOracleDataset.SQL string that begin from 'ORDER BY'  } 
{ phrase and replace line by 'ORDER BY FieldNo1 [DESC],'}
{ using SortMarkedColumns.                              }
{*******************************************************}

unit EhLibDOA;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, OracleData;

implementation

uses Classes;

initialization
  RegisterDatasetFeaturesEh(TSQLDatasetFeaturesEh, TOracleDataset);
end.
