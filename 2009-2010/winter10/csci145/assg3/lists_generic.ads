GENERIC
   TYPE ElementType IS (<>);
PACKAGE Lists_Generic IS
   TYPE List IS PRIVATE;

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType);
   PROCEDURE AddToEnd (L : IN OUT List; Element : IN ElementType);
   PROCEDURE InsertInOrder (L : IN OUT List; Element : IN ElementType);
   PROCEDURE RemoveFront (L : IN OUT List);
   PROCEDURE ConcatenateList (L : IN OUT List; L2 : IN List);
   FUNCTION RetrieveFront (L : IN List) RETURN ElementType;
   FUNCTION Display (L : IN List) RETURN ElementType;
   FUNCTION IsEmpty (L : IN List) RETURN Boolean;
PRIVATE
   TYPE ListNode;
   TYPE pListNode IS ACCESS ListNode;
   TYPE ListNode IS RECORD
      Value : ElementType;
      Next : pListNode;
   END RECORD;

   TYPE List IS RECORD
      Head : pListNode;
      Tail : pListNode;
   END RECORD;
END Lists_Generic;
