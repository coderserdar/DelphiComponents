�
 TFFINVCONN 0:  TPF0�
TffInvConn	ffInvConnLeft� Top�  �
TffSessionffSess
ClientName
ffInvCommsSessionNamefInvSess  �TffDatabaseffDBSessionNamefInvSess  TffTabletblDCDatabaseNameffInvDBSessionNamefInvSess	TableNameDistribCLeftTop@ TAutoIncFieldtblDCID	FieldNameID  TStringField	tblDCName	FieldNameNameRequired	Size2   TffTable
tblProductDatabaseNameffInvDBSessionNamefInvSess	TableNameProductLeftHTop@ TAutoIncFieldtblProductID	FieldNameID  TStringFieldtblProductName	FieldNameNameRequired	Size2  TStringFieldtblProductUOM	FieldNameUOMRequired	Size  TStringFieldtblProductOEM	FieldNameOEMRequired	Size   TffTabletblInvDatabaseNameffInvDBSessionNamefInvSess	TableNameInventOnCalcFieldstblInvCalcFieldsLeftxTop@ TIntegerFieldtblInvDistribCenterID	FieldNameDistribCenterIDRequired	  TIntegerFieldtblInvProductID	FieldName	ProductIDRequired	  TIntegerFieldtblInvQtyOnHand	FieldName	QtyOnHandRequired	  TIntegerFieldtblInvQtyOnOrder	FieldName
QtyOnOrderRequired	  TStringFieldtblInvProductName	FieldNameProductNameLookupDataSet
tblProductLookupKeyFieldsIDLookupResultFieldName	KeyFields	ProductIDSize2Lookup	  TStringFieldtblInvProductOEM	FieldName
ProductOEMLookupDataSet
tblProductLookupKeyFieldsIDLookupResultFieldOEM	KeyFields	ProductIDSizeLookup	  TIntegerFieldtblInvQtyAvail	FieldNameQtyAvail
Calculated	  TStringFieldtblInvDCName	FieldNameDCNameLookupDataSettblDCLookupKeyFieldsIDLookupResultFieldName	KeyFieldsDistribCenterIDSize2Lookup	    