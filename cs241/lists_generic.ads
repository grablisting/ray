GENERIC
   TYPE ElementType IS PRIVATE;
PACKAGE Lists_Generic IS
   TYPE SingleNode;
   TYPE pNode IS ACCESS SingleNode;
   TYPE SingleNode IS RECORD
      Value : ElementType;
      Next : pNode;
   END RECORD;

   TYPE List IS RECORD
      Head : pNode;
      Tail : pNode;
   END RECORD;

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType);
   PROCEDURE AddToEnd (L : IN OUT List; Element : IN ElementType);
   PROCEDURE RemoveFront (L : IN OUT List);
   PROCEDURE PopFront (L : IN OUT List; Element : IN OUT ElementType);
   FUNCTION GetNext (Element : pNode) RETURN pNode;
   FUNCTION RetrieveFront (L : List) RETURN pNode;
   FUNCTION IsEmpty (L : List) RETURN Boolean;
   FUNCTION IsNull (Element : pNode) RETURN Boolean;
END Lists_Generic;
