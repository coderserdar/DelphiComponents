TSelectPanel v1.0
===============================================================================

You are allowed to used this component in any project for free.

Overview.
Component TSelectPanel is intended for selecting several records in the lookup 
dataset and generation of SQL-queries with appropriate conditions.

Main limitations.
1. The lookup table should have a primary key composed from alone integer field.
2. Application is required midas.dll.

Installation.
Use "File\Open..." menu item to open package KBKSP.DPK.
In "Package..." window click "Compile" button to compile the package and then 
click "Install" button to register TSelectPanel component on the component 
palette. If package has been installed correctly you have to see new component 
TSelectPanel on 'Data Control' tab page. Add, (if need) TSelectPanel directory  
in Tools->Environment Options->Library-> Library Path.

Using TSelectPanel.
1. Locate component on the form.
2. Set the ListSource, which Dataset is lookup dataset.
3. Choose a field to use as a lookup key from the drop-down list for the 
   KeyField property.
4. Choose a field whose values to return from the drop-down list for the 
   ListField property. 
5. Optionally choose a field from the drop-down list for the NoteField property
6. Set the SizeText - a size of a displayed field. In case definition its equal 
   to zero - size of a displayed field and width the component will be changed 
   in correspondence with value ListSource.Dataset.FindField(ListField).Size. 

For generation of SQL-queries the procedure will utillize: 
AddWhereClause (const awcItem: TawcCondition; const AFieldName: String; 
  ASQL: TStrings; const ASQLCount: Integer = -1);
awcItem can accept one of four values:
    awcNone - before a condition nothing is added
    awcWhere - before a condition is added WHERE.
    awcAnd - before a condition is added AND, but if is not retrieved earlier
    WHERE (from a beginning of line) - WHERE.
    awcOr - before a condition is added OR, but if is not retrieved earlier
    WHERE (from a beginning of line) - WHERE.
AFieldName - the name of a field (in the operating table), on which one is added
Condition.
ASQL - text SQL-query.
ASQLCount - it is necessary to keep amount of initial lines, which one in ASQL
before addition of a condition. The remaining lines are deleted. Value -1 
means absence of changes in ASQL.

Examples of usage see in a folder DEMO.

DISCLAIMER
By using this component or parts thereof you are accepting the full
responsibility of it's use. You agree to not hold the author responsible
in any way for any problems occurring from the use of this component.

Konstantin Koshelev (koshelevkb@land.ru)
