with lists_generic;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

PROCEDURE Bucket_Sort (Source : IN OUT ArrayType; Key : IN KeyType) IS
   PACKAGE Lists IS NEW Lists_Generic(ElementType => ElementType);
   USE Lists;

   TYPE BucketArray IS Array(KeyType RANGE <>) OF List;
   TYPE pBucketArray IS ACCESS BucketArray;
   MyBuckets : pBucketArray;

   Save : List;
   akey, i : KeyType;
   Min : ElementType := ElementType'Last;
   Max : ElementType := ElementType'First;
   count : Integer := 1;
BEGIN --Bucket_Sort
   --Find the minimum and maximum keyvalues of the sourced array.
   FOR x IN Source'Range LOOP
      akey := GetKey(Source(x), Key);
      IF akey > GetKey(Max, Key) THEN
         Max := Source(x);
      END IF;
      IF akey < GetKey(Min, Key) THEN
         Min := Source(x);
         i := akey;
      END IF;
   END LOOP;

   --Create an array the size of the range of min to max keyvalues.
   MyBuckets := new BucketArray(GetKey(Min, Key)..GetKey(Max, Key));

   --Add each element to it's corresponding keyvalue bucket.
   FOR x IN Source'Range LOOP
      AddToEnd(MyBuckets(GetKey(Source(x), key)), Source(x));
   END LOOP;

   --Connect all the buckets and save the results to the array.
   FOR x IN MyBuckets'Range LOOP
      WHILE NOT IsEmpty(MyBuckets(x)) LOOP
         Source(IndexType'Val(count)) := RetrieveFront(MyBuckets(x));
         RemoveFront(MyBuckets(x));
         count := count + 1;
      END LOOP;
   END LOOP;
END Bucket_Sort;
