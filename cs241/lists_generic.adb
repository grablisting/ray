WITH unchecked_deallocation;

PACKAGE BODY Lists_Generic IS
   PROCEDURE Dispose IS NEW Unchecked_Deallocation (Object => SingleNode, Name => pNode);

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType) IS
      Temp : List;
   BEGIN --AddToFront
      L.Head := NEW SingleNode'(Element, L.Head);
      L.Tail := L.Head;
   END AddToFront;

   PROCEDURE AddToEnd (L : IN OUT List; Element : IN ElementType) IS
   BEGIN --AddToEnd
      IF L.Head = NULL THEN
         L.Head := NEW SingleNode'(Element, NULL);
         L.Tail := L.Head;
      ELSE
         L.Tail.ALL.Next := NEW SingleNode'(Element, NULL);
         L.Tail := L.Tail.ALL.Next;
      END IF;
   END AddToEnd;

   PROCEDURE RemoveFront (L : IN OUT List) IS
      Temp : List;
   BEGIN --RemoveFront
      IF L.Head /= NULL THEN
         Temp := L;
         L.Head := L.Head.ALL.Next;
         Dispose(Temp.Head);
      END IF;
   END RemoveFront;

   PROCEDURE PopFront (L : IN OUT List; Element : IN OUT ElementType) IS
   BEGIN -- PopFront
      Element := RetrieveFront(L).Value;
      RemoveFront(L);
   END PopFront;


   FUNCTION GetNext (Element : pNode) RETURN pNode IS
   BEGIN --GetNext
      IF Element.ALL.Next = NULL THEN
         RETURN NULL;
      ELSE
         RETURN Element.ALL.Next;
      END IF;
   END GetNext;

   FUNCTION RetrieveFront (L : List) RETURN pNode IS
   BEGIN --RetrieveFront
      RETURN L.Head;
   END RetrieveFront;

   FUNCTION IsEmpty (L : List) RETURN Boolean IS
   BEGIN --IsEmpty
      IF L.Head = NULL THEN
         RETURN True;
      ELSE
         RETURN False;
      END IF;
   END IsEmpty;

   FUNCTION IsNull (Element : pNode) RETURN Boolean IS
   BEGIN --IsNull
      IF Element = NULL THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;

   END IsNull;

END Lists_Generic;
