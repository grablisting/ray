with Ada.Text_IO;
with Ada.Integer_Text_IO;

PROCEDURE Counting_Sort (Source : IN OUT ArrayType; key : IN keytype) IS
   Destination : ArrayType(Source'Range);

   TYPE StorageArray IS Array (KeyType RANGE <>) OF Integer;
   TYPE pStorage IS ACCESS StorageArray;

   Storage : pStorage;
   akey : KeyType;
   Max : KeyType := KeyType'First;
   Min : KeyType := KeyType'Last;
   z : Integer := 1;
BEGIN --Counting_Sort
   --Calculate the minimum and maximum key value.
   FOR x IN Source'Range LOOP
      akey := GetKey(Source(x), key);
      IF akey > Max THEN
         Max := akey;
      END IF;
      IF akey < Min THEN
         Min := akey;
      END IF;
   END LOOP;

   --Build an array from the min and max values. Set every count to zero.
   Storage := new StorageArray(Min..Max);
   FOR x IN Storage'Range LOOP
      Storage.ALL(x) := 0;
   END LOOP;

   --Use auxillary array record the frequency of each object in the source array.
   FOR i IN Source'Range LOOP
      akey := GetKey(Source(i), key);
      Storage.ALL(akey) := Storage.ALL(akey) + 1;
   END LOOP;

   --Add the number of the elements in the previous position to the current position.
   FOR i IN KeyType'Succ(Min)..Max LOOP
      Storage.ALL(i) := Storage.ALL(i) + Storage.ALL(KeyType'Pred(i));
   END LOOP;

   --Build a new sorted array from the auxillary array.
   FOR j IN REVERSE Source'Range LOOP
      akey := GetKey(Source(j), key);
      Destination(IndexType'Val(Storage.ALL(akey))) := Source(j);
      Storage.ALL(akey) := Storage.ALL(akey) - 1;
   END LOOP;

   --Return the results to the original array.
   Source := Destination;
END Counting_Sort;
