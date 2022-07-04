
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BizIntf;

{$TYPEDADDRESS OFF}

interface

{$I STD.INC}

uses
  Windows, ActiveX; 

const
  BusinessMajorVersion = 1;
  BusinessMinorVersion = 0;

  LIBID_Business: TGUID = '{87445F6A-C4A6-43EF-83E4-903091D502D8}';
  IID_IStorable: TGUID = '{A26C1D53-6544-4BB7-A712-B150B3D8A295}';
  IID_ICollection: TGUID = '{BF758BA0-1379-473D-B204-400A19A2692B}';
  IID_ISearchCollection: TGUID = '{D48C13AF-F445-4BD3-8239-71C4D83E30A2}';

type
  IStorable = interface(IDispatch)
    ['{A26C1D53-6544-4BB7-A712-B150B3D8A295}']
    procedure Execute(Command: OleVariant); safecall;
    function Retrieve(Format: OleVariant): OleVariant; safecall;
    procedure Update(Data: OleVariant); safecall;
  end;

  IStorableDisp = dispinterface
    ['{A26C1D53-6544-4BB7-A712-B150B3D8A295}']
    procedure Execute(Command: OleVariant); dispid 1;
    function  Retrieve(Format: OleVariant): OleVariant; dispid 2;
    procedure Update(Data: OleVariant); dispid 3;
  end;

  ICollection = interface(IStorable)
    ['{BF758BA0-1379-473D-B204-400A19A2692B}']
    function  Get__NewEnum: IUnknown; safecall;
    function  Add(Item: OleVariant): OleVariant; safecall;
    procedure Delete(Item: OleVariant); safecall;
    function  Get_Count: Integer; safecall;
    function  Get_Item(Index: OleVariant): OleVariant; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property Item[Index: OleVariant]: OleVariant read Get_Item;
  end;

  ICollectionDisp = dispinterface
    ['{BF758BA0-1379-473D-B204-400A19A2692B}']
    property _NewEnum: IUnknown readonly dispid -4;
    function  Add(Item: OleVariant): OleVariant; dispid 5;
    procedure Delete(Item: OleVariant); dispid 6;
    property Count: Integer readonly dispid 7;
    property Item[Index: OleVariant]: OleVariant readonly dispid 8;
    procedure Execute(Command: OleVariant); dispid 1;
    function  Retrieve(Format: OleVariant): OleVariant; dispid 2;
    procedure Update(Data: OleVariant); dispid 3;
  end;

  ISearchCollection = interface(ICollection)
    ['{D48C13AF-F445-4BD3-8239-71C4D83E30A2}']
    function  Search(Params: OleVariant): OleVariant; safecall;
  end;

  ISearchCollectionDisp = dispinterface
    ['{D48C13AF-F445-4BD3-8239-71C4D83E30A2}']
    function  Search(Params: OleVariant): OleVariant; dispid 9;
    property _NewEnum: IUnknown readonly dispid -4;
    function  Add(Item: OleVariant): OleVariant; dispid 5;
    procedure Delete(Item: OleVariant); dispid 6;
    property Count: Integer readonly dispid 7;
    property Item[Index: OleVariant]: OleVariant readonly dispid 8;
    procedure Execute(Command: OleVariant); dispid 1;
    function  Retrieve(Format: OleVariant): OleVariant; dispid 2;
    procedure Update(Data: OleVariant); dispid 3;
  end;

implementation

{.$R BIZINTF.TLB}

end.



