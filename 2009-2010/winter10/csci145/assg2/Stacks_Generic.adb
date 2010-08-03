PACKAGE BODY Stacks_Generic IS
   PROCEDURE MakeEmpty (S : IN OUT Stack) IS
   BEGIN --MakeEmpty
      LOOP
         Lists.RemoveFront(S.Store);
         EXIT WHEN Lists.IsEmpty(S.Store);
      END LOOP;
   END MakeEmpty;

   PROCEDURE Push(S : IN OUT Stack; E : IN StackElement) IS
   BEGIN --Push
      Lists.AddToFront(S.Store, E);
   END Push;

   FUNCTION Top (S : IN Stack) RETURN StackElement IS
   BEGIN --Top
      RETURN Lists.Display(S.Store);
   END Top;

   PROCEDURE Pop (S : IN OUT Stack; E : OUT StackElement) IS
   BEGIN --Pop
      E := Top(S);
      Lists.RemoveFront(S.Store);
   END Pop;

   FUNCTION IsEmpty (S : IN Stack) RETURN Boolean IS
   BEGIN --IsEmpty
     RETURN Lists.IsEmpty(S.Store);
   END IsEmpty;

END Stacks_Generic;
