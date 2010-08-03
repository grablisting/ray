GENERIC
   TYPE ElementType IS PRIVATE;
PACKAGE Lists_Generic IS
   TYPE List IS PRIVATE;

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType);
   PROCEDURE RemoveFront (L : IN OUT List);
   FUNCTION Copy (L : IN List) RETURN List;
   FUNCTION Display (L : IN List) RETURN ElementType;
   FUNCTION IsEmpty (L : IN List) RETURN Boolean;
PRIVATE
   TYPE ListNode;
   TYPE List IS ACCESS ListNode;
   TYPE ListNode IS RECORD
      Value : ElementType;
      Next : List;
   END RECORD;
END Lists_Generic;
