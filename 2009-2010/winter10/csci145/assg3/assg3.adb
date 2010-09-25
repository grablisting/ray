with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
with counting_sort;
with bucket_sort;
with Ada.Real_Time;
use Ada.Real_Time;
with radix_sort;

--Assg3 builds an array from a file filled with random values from 1 to 100.
--Assg3 then measures the time required to do the following:
--	1) Build the random array
--	2) Sort the array with Normal Counting Sort
--	3) Sort the array with Normal Bucket Sort
--	4) Sort the array with Radix Sort via Counting Sort
--	5) Sort the array with Radix Sort via Bucket Sort
--Lastly, Assg3 records the results to a user defined output file.
--Built by Ray Peters; March 10, 2010; for Computer Science 145.

PROCEDURE Assg3 IS
   --Easier reference to unbounded strings.
   PACKAGE Str RENAMES Ada.Strings.Unbounded;
   --Scratchpads for files containing the numbers.
   Source, Destination : Str.Unbounded_String;

   --Number of elements for the randomized array.
   NumberOfNumbers : Integer := 500000;

   --Define array and it's potential inhabitants.
   SUBTYPE IntRange IS Integer RANGE 1..100;
   TYPE MyData IS ARRAY (Positive RANGE <>) OF IntRange;
   --Use access type for array to dynamically create a new array.
   TYPE pMyData IS ACCESS MyData;

   --GetKey returns the value at a given digit position.
   --Used by Radix Sort as a parameter to Counting Sort and Bucket Sort.
   FUNCTION GetKey (Element : IntRange; Key : Integer) RETURN Integer IS
   BEGIN --GetKey
      RETURN (Element / (10 ** (key - 1))) REM 10;
   END GetKey;

   --GetNum returns the entire value of the Element.
   --Used by ordinary Counting Sort & Bucket Sort.
   FUNCTION GetNum (Element : IntRange; Key : Integer) RETURN Integer IS
   BEGIN --GetNum
      RETURN Element;
   END GetNum;

   --Normal Counting and Bucket Sort instantiations.
   --Ordinary sorting procedures use GetNum as the sorting key value.
   PROCEDURE CountingSort_withoutkey IS NEW counting_sort(ElementType => IntRange,
                                         	  IndexType => Positive,
                                           	KeyType => Integer,
                                           	ArrayType => MyData,
                                               GetKey => GetNum);
   PROCEDURE BucketSort_withoutkey IS NEW bucket_sort(ElementType => IntRange,
                                         	  IndexType => Positive,
                                           	KeyType => Integer,
                                           	ArrayType => MyData,
                                                   GetKey => GetNum);

   --Radix sorting procedures use GetKey as the sorting key value.
   PROCEDURE CountingSort_withkey IS NEW counting_sort(ElementType => IntRange,
                                               IndexType => Positive,
                                               KeyType => Integer,
                                               ArrayType => MyData,
                                               GetKey => GetKey);
   PROCEDURE BucketSort_withkey IS NEW bucket_sort(ElementType => IntRange,
                                         	  IndexType => Positive,
                                           	KeyType => Integer,
                                           	ArrayType => MyData,
                                               GetKey => GetKey);

   --Radix Sort instantiations.
   PROCEDURE RadixviaCountingSort IS NEW radix_sort(ElementType => IntRange,
                                                    IndexType => Positive,
                                                    ArrayType => MyData,
                                                    Sort => CountingSort_withkey);
   PROCEDURE RadixviaBucketSort IS NEW radix_sort(ElementType => IntRange,
                                                  IndexType => Positive,
                                                  ArrayType => MyData,
                                                  Sort => BucketSort_withkey);

   --SortingMethods provides the available operations to measure duration.
   TYPE SortingMethods IS (NormalCounting, NormalBucket, RadixViaCounting, RadixViaBucket);

   --RecordTime duplicates a Source array and calculates the time required
   --to perform the given sort operation. Returns the duration between the
   --start of the sorting operation and the end of the sorting operation.
   FUNCTION RecordTime (Todo : SortingMethods; pData : pMyData; Key : Integer := 1) RETURN Duration IS
      Tick, Tock : Time;
      pDataDuplicate : pMyData;
   BEGIN --RecordTime
      --DELAY fixes an anamoly when using Ada.Real_Time services in windows.
      --Has no reported effect in Linux.
      DELAY 0.0;

      --Duplicate the source array to provide comparable data.
      pDataDuplicate := new myData (pData.ALL'Range);
      pDataDuplicate.ALL := pData.ALL;

      --Measure time required to sort the array given which sorting method.
      Tick := Clock;
      CASE Todo IS
         WHEN NormalCounting =>
            CountingSort_withoutkey(pDataDuplicate.ALL, Key);
         WHEN NormalBucket =>
            BucketSort_withoutkey(pDataDuplicate.ALL, Key);
         WHEN RadixViaCounting =>
            RadixviaCountingSort(pDataDuplicate.ALL, Key);
         WHEN RadixViaBucket =>
            RadixviaBucketSort(pDataDuplicate.ALL, Key);
      END CASE;
      Tock := Clock;

      --Return the duration.
      RETURN To_Duration(Tock - Tick);
   END RecordTime;

   --Accessor to my data array.
   This : pMyData;

   --File References.
   InFile, OutFile : Ada.Text_IO.File_Type;

   --Ada.Real_Time clock accessors.
   Tick, Tock : Time;
BEGIN -- Assg3
   --User inquiry to gather a source filename for randomized data.
   Ada.Text_IO.Put (Item => "Please enter a file name to save the results: ");
   Ada.Text_IO.Unbounded_IO.Get_Line (Source);

   --Provide a default file if the user doesn't provide a source file.
   IF Str.Length(Source) = Str.Length(Destination) THEN
      Source := Str.To_Unbounded_String("random1millionintegers.txt");
   END IF;

   --Build the randomized array to be sorted.
   Ada.Text_IO.Put ("Building array...                                 ");
   Ada.Text_IO.Open(File => InFile, Mode => Ada.Text_IO.In_File, Name => Str.To_String(Source));
   This := new MyData(1..NumberOfNumbers);
   Tick := Clock;
   FOR x IN This.ALL'Range LOOP
      Ada.Integer_Text_IO.Get (File => InFile, Item => This.ALL(x));
   END LOOP;
   Tock := Clock;
   Ada.Text_IO.Close(File => InFile);
   Ada.Text_IO.Put_Line (Item => "Complete.");

   --User inquiry for filename to save results.
   Ada.Text_IO.Put (Item => "Please enter a file name to save the results: ");
   Ada.Text_IO.Unbounded_IO.Get_Line (Destination);

   --Create the user-named file and provide informative headings.
   Ada.Text_IO.Create (File => OutFile, Mode => Ada.Text_IO.Out_File, Name => Str.To_String(Destination));
   Ada.Text_IO.Put (File => OutFile, Item => "Built an array consisting of ");
   Ada.Integer_Text_IO.Put (File => OutFile, Item => NumberOfNumbers, Width => 1);
   Ada.Text_IO.Put (File => OutFile, Item => " numbers ranging from ");
   Ada.Integer_Text_IO.Put (File => OutFile, Item => IntRange'First, Width => 1);
   Ada.Text_IO.Put (File => OutFile, Item => " .. ");
   Ada.Integer_Text_IO.Put (File => OutFile, Item => IntRange'Last, Width => 1);
   Ada.Text_IO.New_Line (File => OutFile, Spacing => 2);
   Ada.Text_IO.Put_Line (File => OutFile, Item => "-----------------CREATION/SORTING TIMES-----------------");

   --Record the time required to build the array from a file.
   Ada.Text_IO.Put (File => OutFile, Item => "Array Creation:                             ");
   Ada.Text_IO.Put_Line (File => OutFile, Item => Duration'Image(To_Duration(Tock - Tick)));

   --Record the time required to sort the array with Normal Counting Sort.
   Ada.Text_IO.Put (File => OutFile, Item => "Counting Sort Span:                         ");
   Ada.Text_IO.Put ("Sorting file via CountingSort...                  ");
   Ada.Text_IO.Put_Line (File => OutFile, Item => Duration'Image(RecordTime(NormalCounting, This)));
   Ada.Text_IO.Put_Line (Item => "Complete.");

   --Record the time required to sort the array with Normal Bucket Sort.
   Ada.Text_IO.Put (File => OutFile, Item => "Bucket Sort Span:                           ");
   Ada.Text_IO.Put ("Sorting file via BucketSort...                    ");
   Ada.Text_IO.Put_Line (File => OutFile, Item => Duration'Image(RecordTime(NormalBucket, This)));
   Ada.Text_IO.Put_Line (Item => "Complete.");

   --Record the time required to sort the array with Radix Sort via Counting Sort.
   Ada.Text_IO.Put (File => OutFile, Item => "Radix Sort via CountingSort Span:           ");
   Ada.Text_IO.Put ("Sorting file via Radix Sort with Counting Sort... ");
   Ada.Text_IO.Put_Line (File => OutFile, Item => Duration'Image(RecordTime(RadixViaCounting, This, 3)));
   Ada.Text_IO.Put_Line (Item => "Complete.");

   --Record the time required to sort the array with Radix Sort via Bucket Sort.
   Ada.Text_IO.Put (File => OutFile, Item => "Radix Sort via BucketSort Span:             ");
   Ada.Text_IO.Put ("Sorting file via Radix Sort with Bucket Sort...   ");
   Ada.Text_IO.Put_Line (File => OutFile, Item => Duration'Image(RecordTime(RadixViaBucket, This, 3)));
   Ada.Text_IO.Put_Line (Item => "Complete.");
   Ada.Text_IO.Close(File => OutFile);

   --Farewell.
   Ada.Text_IO.Put_Line (Item => "Farewell!");
END Assg3;
