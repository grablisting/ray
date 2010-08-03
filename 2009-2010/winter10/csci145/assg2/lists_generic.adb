WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
WITH unchecked_deallocation;

PACKAGE BODY Lists_Generic IS
   PROCEDURE Dispose IS NEW Unchecked_Deallocation (Object => ListNode, Name => List);

   PROCEDURE AddToFront (L : IN OUT List; Element : IN ElementType) IS
      Temp : List;
   BEGIN --AddToFront
      Temp := NEW ListNode;
      Temp.ALL.Value := Element;
      Temp.ALL.Next := L;
      L := Temp;
   END AddToFront;

   PROCEDURE RemoveFront (L : IN OUT List) IS
      Temp : List;
   BEGIN --RemoveFront
      IF L /= NULL THEN
         Temp := L;
         L := L.Next;
         Dispose(Temp);
      END IF;
   END RemoveFront;

   FUNCTION Copy (L : IN List) RETURN List IS
   BEGIN --Copy
      IF L = NULL THEN
         RETURN NULL;
      ELSE
         RETURN NEW ListNode'(L.ALL.Value, Copy(L.ALL.Next));
      END IF;
   END Copy;

   FUNCTION Display (L : IN List) RETURN ElementType IS
   BEGIN --Display
      RETURN L.ALL.Value;
   END Display;

   FUNCTION IsEmpty (L : IN List) RETURN Boolean IS
   BEGIN --IsEmpty
      IF L = NULL THEN
         RETURN True;
      ELSE
         RETURN False;
      END IF;
   END IsEmpty;
END Lists_Generic;
