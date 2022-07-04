#!/bin/bash
FBLDOC=FblibHelp
if [ ! -d $FBLDOC ]; then
  mkdir $FBLDOC
fi
pasdoc FBLDatabase.pas FBLTransaction.pas FBLDsql.pas FBLService.pas FBLExcept.pas FBLEvents.pas FBLMetadata.pas FBLScript.pas FBLSimple.pas FBLParamDsql.pas -N FbLib -T FbLib-docs -E $FBLDOC
