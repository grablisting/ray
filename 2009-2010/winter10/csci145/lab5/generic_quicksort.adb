PROCEDURE Sort (This : IN OUT ArrayType; pLeft, pRight : IN ) IS
   Q : Integer;
BEGIN --Sort
   IF pLeft - pRight <= 0 THEN
      RETURN This;
   ELSE
      Q := Partition(This,pLeft,pRight);
      Sort(This,pLeft,Q-1);
      Sort(This,Q+1,pRight);
   END IF;
END Sort;

FUNCTION Partition(This : IN ArrayType; pLeft, pRight : IN ) RETURN Integer IS
   i, x : Integer;
BEGIN --Partition
   i := pLeft - 1;
   x := This[pRight];

   FOR j in pLeft..pRight - 1 LOOP
      IF This[j] <= x THEN
         i := i + 1;


END Partition;

