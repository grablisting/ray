with Ada.Text_IO;
with Ada.Integer_Text_IO;

PROCEDURE Counting_Sort (Source : IN OUT ArrayType) IS
   Destination : ArrayType := Source;

   TYPE StorageArray IS Array (ElementType RANGE <>) OF Integer;
   TYPE pStorage IS ACCESS StorageArray;

   Min, Max : ElementType := Source(Source'First);
   Storage : pStorage;
   z : Integer := 1;
BEGIN --Counting_Sort
   FOR x IN Source'Range LOOP
      IF ElementType'Pos(Source(x)) > ElementType'Pos(Max) THEN
         Max := Source(x);
      END IF;
      IF ElementType'Pos(Source(x)) < ElementType'Pos(Min) THEN
         Min := Source(x);
      END IF;
   END LOOP;

   Storage := new StorageArray(ElementType'Pred(Min)..Max);
   FOR x IN Storage'Range LOOP
      Storage.ALL(x) := 0;
   END LOOP;

   FOR i IN Source'Range LOOP
      Storage.ALL(Source(i)) := Storage.ALL(Source(i)) + 1;
   END LOOP;

   FOR i IN Min..Max LOOP
      Storage.ALL(i) := Storage.ALL(i) + Storage.ALL(ElementType'Pred(i));
   END LOOP;

   FOR j IN REVERSE Source'Range LOOP
      Destination(IndexType'Val(Storage.ALL(Source(j)))) := Source(j);
      Storage.ALL(Source(j)) := Storage.ALL(Source(j)) - 1;
   END LOOP;

   Source := Destination;
END Counting_Sort;
