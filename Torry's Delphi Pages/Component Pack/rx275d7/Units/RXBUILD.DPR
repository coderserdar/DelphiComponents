{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{         RX build project                              }
{                                                       }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

{$D-,L-,Y-,B-,S-,I-}

{$I RX.INC}

{$IFDEF RX_D3}
  ERROR! RX Build program is intended for Delphi 1.0 and 2.0 only
{$ENDIF}

Library RxBuild;

Uses RxCtlReg, RxDBReg, RxTooReg, RxBDEReg, DbExcpt, DateUtil, RxIni, ObjStr,
  Parsing, RxVerInf, SplshWnd, Ole2Auto, QBndDlg, RxHints, Str16;

begin
end.