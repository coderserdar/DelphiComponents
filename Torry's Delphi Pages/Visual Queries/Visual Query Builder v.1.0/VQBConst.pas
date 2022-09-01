unit VQBConst;

interface

const  //Base DB Field Types
   FieldType: array [0..37] of string = ('Unknown', 'String', 'Smallint', 'Integer', 'Word',
    'Boolean', 'Float', 'Currency', 'BCD', 'Date', 'Time', 'DateTime',
    'Bytes', 'VarBytes', 'AutoInc', 'Blob', 'Memo', 'Graphic', 'FmtMemo',
    'ParadoxOle', 'DBaseOle', 'TypedBinary', 'Cursor', 'FixedChar', 'WideString',
    'Largeint', 'ADT', 'Array', 'Reference', 'DataSet', 'OraBlob', 'OraClob',
    'Variant', 'Interface', 'IDispatch', 'Guid', 'TimeStamp', 'FMTBcd');

   crCUR_LINK = 51;  //Do you really think you can handle 50 cursors in your application?
   crCUR_TABLE = 52;

   ResTableID = 0;
   ResFieldID = 1;
   ResShowID = 2;
   ResFunctionID = 3;
   ResGroupID = 4;

   ResSortOrder: array [0..3] of String = ('',
                                           '-',
                                           'ASC',
                                           'DESC');

   ResFuncOrder: array [0..6] of String = ('',
                                           '-',
                                           'AVG',
                                           'COUNT',
                                           'MAX',
                                           'MIN',
                                           'SUM');

implementation

end.
