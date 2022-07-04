{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit cconsts;

interface

const
SNotItBatchMUpdateMode = 'Recordset not in batch update mode';
SCannotAddRecords = 'Cannot add records to the current recordset';
SConnectionClosed = 'Cannot perform this operation on a closed connection';
SPreparedNotSupported = 'Provider does not support prepared/compiled queries';
SCannotBeginTrans = 'Cannot begin new transaction';
SCannotCommit = 'Cannot commit transaction';
SCannotRollback = 'Cannot rollback transaction';
SNoBookmarks = 'Recordset doesn''t support bookmarks';
SDSNNotFound = 'Data source name ''%s'' not found';
SBadParamDataType = 'Invalid data type of param ''%s''';
SCannotModifyDSN = 'Cannot add/modify data source name ''%s''';
SCannotChangeType = 'Cannot change source type';
SCannotCompareBookmarks = 'Cannot compare bookmarks';
SCannotMoveLast = 'Cannot move last if dataset does not support fetching backward';
SNoConnection = 'Connection/Database property must be assigned to an active TConnection component';
SInvalidSchemaType = 'Invalid schema type';
SInvalidSchemaResult = 'Invalid schema result';
SInvalidSource = 'Error in executing command. Check table name, procedure name or SQL script. Probably the command does not return any records';
SInvalidDataset = 'Invalid dataset';
SNoMoreRecordsets = 'No more recordsets';
SOnlyTextOrStoredProc = 'Allowed only SQL script or stored procedure name';
SInvalidOptions = 'Invalid options';
SInvalidLockType = 'Invalid lock type';
SNoNamedParams = 'Named parameters not supported';
SFeatureNotSupported = '%s not supported in MDAC version %s or earlier. Please install MDAC %s or later';
SRecordsetLocked = 'Recordset locked';
SOLEBlobStreamError = 'Provider doesn''t s';
SOLEBlobStreamModeError = 'Stream cannot be used for both reading and writing';
SOLEBlobStreamSeekError = 'Cannot seek in sequential blob stream';
SInvalidRowHandle = 'Invalid row handle';
SCannotAddNewProperties = 'Cannot add new properties';
SMasterSourceInterfaceIsNull = 'Master source doesn''t expose necessary interface';
SMasterNotEditing = 'Master source not in edit or insert mode';

implementation

end.
