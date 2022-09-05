// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17505: DefHuffman.pas 
//
//   Rev 1.0    27-05-2002 16:21:46  mcm

unit DefHuffman;

interface

type
  THuffmanCodeLen = Packed record
                     Len  : word;
                     Code : word;
                   end;

  THuffmanArray = array[0..0] of THuffmanCodeLen;
  PHuffmanArray = ^THuffmanArray;

implementation

end.
