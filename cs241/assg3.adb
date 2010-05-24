WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
WITH BeautifulIO;
WITH lists_generic;

PROCEDURE assg3 IS

   TYPE DataArray IS Array (Integer RANGE <>, Integer RANGE <>) OF Integer;
   TYPE pDataArray IS ACCESS DataArray;

   PACKAGE Beautiful_IO IS NEW BeautifulIO (LineLength => 20,
                                           MaxLineLength => 50);
   USE Beautiful_IO;

   PACKAGE NodeSet IS NEW lists_generic (ElementType => Integer);
   USE NodeSet;

   TYPE ArrayRecord IS RECORD
      Name : BigString;
      Set  : List;
      Data : pDataArray;
   END RECORD;


   PROCEDURE NumLabel (Leading : Natural := 0; Num : Natural; Trailing : Natural := 0) IS
   BEGIN
      Spaces(Leading);
      Ada.Text_IO.Put (Character'Val (Character'Pos ('A') + Num - 1));
      Spaces(Trailing);
   END NumLabel;

   FUNCTION BuildTwoDimensionalArrayRecordFromFileName (Source : String) RETURN ArrayRecord IS
      pNewArray : pDataArray;
      NewArrayRecord : ArrayRecord;
      InFile    : File;
      size      : Integer := 0;
      temp      : Character;
   BEGIN
      Ada.Text_IO.Open (File => InFile, Mode => Ada.Text_IO.In_File, Name => Source);

      WHILE NOT Ada.Text_IO.End_Of_File (File => InFile) LOOP
         Ada.Text_IO.Get (File => InFile, Item => temp);
         IF Ada.Text_IO.End_Of_Line (File => InFile) THEN
            size := size + 1;
         END IF;
      END LOOP;

      Ada.Text_IO.Close (File => InFile);

      pNewArray := NEW DataArray (1 .. size, 1 .. size);
      pNewArray.ALL := (OTHERS => (OTHERS => -1));
      Ada.Text_IO.Open (File => InFile, Mode => Ada.Text_IO.In_File, Name => Source);

      FOR x IN 1 .. size LOOP
         FOR y IN x .. size LOOP
            Ada.Integer_Text_IO.Get (File => InFile, Item => pNewArray.ALL (x, y));
            IF pNewArray.ALL (x, y) = 0 THEN
               pNewArray.ALL(x,y) := Integer'Last;
            END IF;

            --pNewArray.ALL (y, x) := pNewArray.ALL (x, y);
         END LOOP;
         Add(NewArrayRecord.Set, x);
      END LOOP;

      Ada.Text_IO.Close (File => InFile);
      NewArrayRecord.Data := pNewArray;
      NewArrayRecord.Name := BigStringify(Source);

      RETURN NewArrayRecord;
   END BuildTwoDimensionalArrayRecordFromFileName;

   PROCEDURE PrintTwoDimensionalArray (Source : pDataArray) IS
      val : Integer;
   BEGIN
      Spaces(2);
      FOR x IN Source.ALL'Range LOOP
         NumLabel (Leading => 6, Num => x);
      END LOOP;
      Clear(2);

      FOR x IN Source.ALL'Range LOOP
         NumLabel (Leading => 2, Num => x);
         FOR y IN Source.ALL'Range LOOP
            val := Source.ALL (x, y);
            IF val >= 0  AND val < Integer'Last THEN
               Ada.Integer_Text_IO.Put (val, Width => 7);
            ELSIF val < 0 THEN
               Spaces(7);
            ELSE
               Ada.Text_IO.Put("    Inf");
            END IF;
         END LOOP;
         Clear;
      END LOOP;
      Clear(2);
   END PrintTwoDimensionalArray;

   PROCEDURE PrintTwoDimensionalArrayRecord (Datum : ArrayRecord) IS
   BEGIN
      PageBreak (Stringify (Datum.Name));
      PrintTwoDimensionalArray(Datum.Data);
   END PrintTwoDimensionalArrayRecord;

   PROCEDURE PrintSet (Source : List) IS
      temp : pNode := Source.Head;
   BEGIN --PrintSet
      WHILE temp /= NULL LOOP
         NumLabel (Leading => 2, Num => temp.ALL.Value, Trailing => 2);
         temp := temp.ALL.Next;
      END LOOP;
      Clear;
   END PrintSet;

   PROCEDURE PrintData (thing1, thing2a, thing2b: Integer) IS
   BEGIN --PrintData
      NumLabel (2, thing1, 2);
      Ada.Text_IO.Put (" is comparing ");
      NumLabel (2, thing2a, 2);
      Ada.Text_IO.Put (" (weight ");
      Ada.Integer_Text_IO.Put (thing2b, Width => 3);
      Ada.Text_IO.Put_Line (")");
   END PrintData;


   PROCEDURE DijskrasShortestPath (TwoDimArray : IN OUT ArrayRecord; StartNode : IN Integer; Output : BigString) IS
      SUBTYPE ArraySize IS Integer RANGE TwoDimArray.Data.ALL'Range;
      datum : pDataArray := TwoDimArray.Data;
      thisArray  : pDataArray := NEW DataArray(ArraySize, ArraySize);
      TYPE tempArray IS ARRAY (Integer RANGE <>) OF Integer;
      distArray  : tempArray (ArraySize'Range);
      Unused : List := TwoDimArray.Set;
      Used, Weights : List;
      dist, shortestDist : Integer := 1000;
      vertex		 : Integer;
      closestVertex      : Integer;
      lastVertex	 : Integer;
      adjacentDist       : Integer;
      tempNode   : pNode;
      tempSet : List;

   BEGIN --DijskrasShortestPath
      FOR x IN ArraySize LOOP
         distArray (x) := datum.ALL (StartNode, x);
      END LOOP;
      distArray (StartNode) := 0;
      vertex := StartNode;
      Add (Used, vertex);
      tempNode := Find(Unused, StartNode);
      Pop (Unused, tempNode);

      PrintTwoDimensionalArrayRecord(TwoDimArray);
      FOR x IN ArraySize LOOP

         shortestDist := 10000;
         FOR x IN ArraySize LOOP
            IF Find(Used, x) = NULL THEN
               dist := distArray (x);
               IF dist >= 0 AND dist <= shortestDist THEN
                  shortestDist := dist;
                  closestVertex := x;
               END IF;
            END IF;
         END LOOP;


         IF shortestDist = -1 OR shortestDist > 100000 THEN
            Clear(2);
            PageBreak("Done!");
            EXIT;
         END IF;
         Add (Used, closestVertex);


         --Clear(2);
         FOR x IN ArraySize LOOP
            adjacentDist := datum.ALL (x, closestVertex);
            IF adjacentDist > 0 AND Find (Used, x) = NULL THEN
               IF adjacentDist < distArray(x) + shortestDist THEN
                  distArray (x) := adjacentDist;
               END IF;
            END IF;
         END LOOP;
         --Clear(2);
      END LOOP;


      PageBreak("Results of Shortest Path");
      FOR x IN ArraySize LOOP
         --Ada.Integer_Text_IO.Put (distArray (x), Width => 5);
         Pop (Used, tempNode);
         Add (tempSet, tempNode.ALL.Value);
         PrintSet(tempSet);
      END LOOP;

   END DijskrasShortestPath;

   --OutputFile : BigString := Request ("Output File");
   --InputFile : BigString := Include ("Input File", "testdata.dat");
   InputFile : BigString := Include ("Input File", "blackboardtest2.dat");
   Collection : ArrayRecord := BuildTwoDimensionalArrayRecordFromFileName (Stringify (InputFile));
BEGIN --assg3
      --PrintTwoDimensionalArrayRecord (Collection);

   Clear(2);
   DijskrasShortestPath (Collection, 1, BigStringify (" hi "));
   Clear(2);


--
--     DECLARE
--        temp : pNode;
--        TestList : List;
--     BEGIN
--        PageBreak("List Testing Center");
--        PrintSet (Collection.Set);
--        TestList := CopyList (Collection.Set);
--        temp := Pop (Collection.Set);
--        temp := Pop (Collection.Set);
--        PrintSet (Collection.Set);
--        PrintSet (TestList);
--     END;

END assg3;
