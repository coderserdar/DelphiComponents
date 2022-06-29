unit RVUndoStr;

interface
uses RVEdit;

const RVUndoTypeNamesEn : array [TRVUndoType] of String =

                 (
                 '', // <- no undo
                 'removing', 'inserting', 'paragraph modification',
                 'editing', 'page break', 'removing page break',
                 'typing', 'tag modification', 'changing text style',
                 'inserting checkpoint', 'removing checkpoint',
                 'editing checkpoint','modifying',
                 '' // <- use custom undo name instead
                 );

implementation

end.
