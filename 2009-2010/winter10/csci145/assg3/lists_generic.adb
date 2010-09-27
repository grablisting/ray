WITH unchecked_deallocation;

PACKAGE BODY Lists_Generic IS
   PROCEDURE Dispose IS NEW Unchecked_Deallocation (Object => ListNode, Name => pListNode);

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType) IS
      Temp : List;
   BEGIN --AddToFront
      L.Head := NEW ListNode'(Element, L.Head);
      L.Tail := L.Head;
   END AddToFront;

   PROCEDURE AddToEnd (L : IN OUT List; Element : IN ElementType) IS
   BEGIN --AddToEnd
      IF L.Head = NULL THEN
         L.Head := NEW ListNode'(Element, NULL);
         L.Tail := L.Head;
      ELSE
         L.Tail.ALL.Next := NEW ListNode'(Element, NULL);
         L.Tail := L.Tail.ALL.Next;
      END IF;
   END AddToEnd;

   PROCEDURE ConcatenateList (L : IN OUT List; L2 : IN List) IS
   BEGIN --ConcatenateList
      L.Tail.ALL.Next := L2.Head;
   END ConcatenateList;


   PROCEDURE InsertInOrder (L : IN OUT List; Element : IN ElementType) IS
      Temp, Previous, Current : pListNode;
   BEGIN --InsertInOrder
      IF L.Head = NULL THEN
         AddToFront (L, Element);
      ELSIF Element < L.Head.ALL.Value THEN
         AddToFront (L, Element);
      ELSIF Element >= L.Tail.ALL.Value THEN
         AddToEnd(L, Element);
      ELSE
         Temp := NEW ListNode'(Element, NULL);
         Previous := L.Head;
         Current := Previous.ALL.Next;

         WHILE Element >= Current.ALL.Value LOOP
            Previous := Current;
            Current := Current.ALL.Next;
         END LOOP;

         Temp.ALL.Next := Current;
         Previous.ALL.Next := Temp;
      END IF;
   END InsertInOrder;


   PROCEDURE RemoveFront (L : IN OUT List) IS
      Temp : List;
   BEGIN --RemoveFront
      IF L.Head /= NULL THEN
         Temp := L;
         L.Head := L.Head.ALL.Next;
         Dispose(Temp.Head);
      END IF;
   END RemoveFront;

   FUNCTION RetrieveFront (L : IN List) RETURN ElementType IS
   BEGIN --RetrieveFront
      RETURN L.Head.ALL.Value;
   END RetrieveFront;

   FUNCTION Display (L : IN List) RETURN ElementType IS
   BEGIN --Display
      RETURN L.Head.Value;
   END Display;

   FUNCTION IsEmpty (L : IN List) RETURN Boolean IS
   BEGIN --IsEmpty
      IF L.Head = NULL THEN
         RETURN True;
      ELSE
         RETURN False;
      END IF;
   END IsEmpty;
END Lists_Generic;
