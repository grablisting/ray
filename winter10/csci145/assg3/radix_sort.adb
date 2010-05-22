PROCEDURE radix_sort (Source : IN OUT ArrayType; d : IN Integer) IS
BEGIN --radix_sort
   --Call the stable sorting method on each position.
   --The position passes the correct keyvalue to the stable sorting methods.
   FOR x IN 1..d LOOP
      Sort (Source, x);
   END LOOP;
END radix_sort;
