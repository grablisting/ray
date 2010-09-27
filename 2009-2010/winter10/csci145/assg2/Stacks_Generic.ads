WITH Lists_Generic;

GENERIC
   TYPE StackElement IS PRIVATE;
PACKAGE Stacks_Generic IS
   TYPE Stack IS LIMITED PRIVATE;
   StackEmpty : EXCEPTION;

   PROCEDURE MakeEmpty (S : IN OUT Stack);
   PROCEDURE Push(S : IN OUT Stack; E : IN StackElement);
   PROCEDURE Pop (S : IN OUT Stack; E : OUT StackElement);
   FUNCTION Top (S : IN Stack) RETURN StackElement;
   FUNCTION IsEmpty (S : IN Stack) RETURN Boolean;
PRIVATE
   PACKAGE Lists IS NEW Lists_Generic(ElementType => StackElement);

   TYPE Stack IS RECORD
      Store : Lists.List;
   END RECORD;
END Stacks_Generic;
