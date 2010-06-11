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

   FUNCTION CopyNode (E : pNode) RETURN pNode;
   FUNCTION CopyList (L : List) RETURN List;
   PROCEDURE Push (L : IN OUT List; Element : IN ElementType);
   PROCEDURE Add (L : IN OUT List; Element : IN ElementType);
   PROCEDURE Pop (L : IN OUT List; E : OUT pNode);
   FUNCTION Find (L : List; Element : ElementType) RETURN pNode;

   PROCEDURE Extract (L : IN OUT List; E : IN OUT ElementType; Result : IN OUT pNode);
   FUNCTION GetNext (Element : pNode) RETURN pNode;
   FUNCTION RetrieveFront (L : List) RETURN pNode;
   FUNCTION IsEmpty (L : List) RETURN Boolean;
   FUNCTION IsNull (Element : pNode) RETURN Boolean;
END Lists_Generic;
