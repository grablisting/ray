WITH unchecked_deallocation;

PACKAGE BODY Lists_Generic IS
   PROCEDURE Dispose IS NEW Unchecked_Deallocation (Object => SingleNode, Name => pNode);



   FUNCTION CopyNode (E : pNode) RETURN pNode IS
   BEGIN --CopyNode
      RETURN NEW SingleNode'(E.ALL.Value, E.ALL.Next);
   END CopyNode;

   FUNCTION CopyList (L : List) RETURN List IS
      newList : List;
      temp : pNode := L.Head;
   BEGIN --CopyList
      WHILE temp /= NULL LOOP
         Add (newList, temp.ALL.Value);
         temp := temp.ALL.Next;
      END LOOP;
      RETURN newList;
   END CopyList;




   FUNCTION Find (L : List; Element : ElementType) RETURN pNode IS
      temp : pNode := L.Head;
   BEGIN --Find
      WHILE temp /= NULL AND THEN temp.ALL.Value /= Element LOOP
         temp := temp.ALL.Next;
      END LOOP;

      RETURN temp;
   END Find;

   PROCEDURE Push (L : IN OUT List; Element : IN ElementType) IS
   BEGIN --AddToFront
      L.Head := NEW SingleNode'(Element, L.Head);

      IF L.Head.ALL.Next = NULL THEN
         L.Tail := L.Head;
      END IF;
   END Push;

   PROCEDURE Add (L : IN OUT List; Element : IN ElementType) IS
   BEGIN --AddToEnd
      IF L.Head = NULL THEN
         L.Head := NEW SingleNode'(Element, NULL);
         L.Tail := L.Head;
      ELSE
         L.Tail.ALL.Next := NEW SingleNode'(Element, NULL);
         L.Tail := L.Tail.ALL.Next;
      END IF;
   END Add;

   PROCEDURE Pop (L : IN OUT List; E : OUT pNode) IS
      thisElement : pNode := NULL;
   BEGIN --RemoveFront
      IF NOT IsEmpty (L) THEN
         thisElement := NEW SingleNode'(L.Head.ALL);
         L.Head := L.Head.ALL.Next;
      END IF;
      E := thisElement;
   END Pop;

   PROCEDURE ExtractHelper (L : IN OUT List; E : IN OUT ElementType; Result : IN OUT pNode) IS
      tempLast : pNode;
      tempNode : pNode := L.Head;
   BEGIN --ExtractHelper
      WHILE tempNode /= NULL AND THEN tempNode.ALL.Value /= E LOOP
         tempLast := tempNode;
         tempNode := tempNode.ALL.Next;
      END LOOP;
      IF tempNode /= NULL THEN
         IF tempNode = L.Head THEN
            tempLast := L.Head;
            tempLast.ALL.Next := tempNode.ALL.Next.ALL.Next;
         ELSE
            tempLast.ALL.Next := tempNode.ALL.Next;
         END IF;
      END IF;

      Result := tempLast;
   END ExtractHelper;

   PROCEDURE Extract (L : IN OUT List; E : IN OUT ElementType; Result : IN OUT pNode) IS
   BEGIN --Extract
      ExtractHelper (L, E, Result);
   END Extract;

   FUNCTION GetNext (Element : pNode) RETURN pNode IS
   BEGIN --GetNext
      RETURN Element.ALL.Next;
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
