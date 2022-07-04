This directory contain files that contain objects that allows to SORT and 
FILTER data in various types of datasets. TDBGridEh use this objects 
to sort data after sorting markers will be changed and filter data when
STFitler is visible in the grid.

If you adjust grid and title for sortmatking but don't write 
OnSortMarkingChanged event then grid will try to sort data automatically. 
DBGridEhDataService try to find special object that can sort data in 
specified type of TDatsetSet usning function GetDatasetFeaturesForDataSet. 
As you know TDataSet does not support sorting data, but descented objects 
as TQuery or TClientDataSet allows to do it. Using procedure 
RegisterDatasetFeaturesEh you can register TDatasetFeaturesEhClass class 
that can sort data in specified type of DataSet. EhLib already have
classes that can sort data TQuery, TADOQuery and TClientDataSet objects. 
Simply add one of the units EhLib... (EhLibBDE, EhLibADO, EhLibCDS) to 
'uses' clause of any unit of your project and grid will automatically 
sort data in such DataSet if it connected to one of its. EhLibBDE, 
EhLibADO, EhLibCDS call RegisterDatasetFeaturesEh procedure in 
initialization part of unit for according type of DataSet's. For other 
types of datasets you have to write and register  new object that will 
implement sorting data in DataSet. Writing procedure 
T[YouDataSet]DatasetFeaturesEh.ApplySorting you can access to list of 
columns whose sortmarkers have up/down direction using SortMarkedColumns 
property. See for instance DbUtilsEh unit to understand  how to write
T[YouDataSet]DatasetFeaturesEh class and EhLibBDE unit to understand  
how to register T[YouDataSet]DatasetFeaturesEh class. 


Engine            DataSet           FileName     

BDE               TQuery            EhLibBDE     
ADO               TADOQuery         EhLibADO     
ClientDataSet     TClientDataSet    EhLibCDS     
DBExpress         TSQLQuery         EhLibDBX
InterBase Express TIBQuery          EhLibIBX