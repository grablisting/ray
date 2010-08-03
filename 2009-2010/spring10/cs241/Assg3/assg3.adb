WITH Ada.Text_IO;
USE Ada.TExt_IO;
WITH Ada.Integer_Text_IO;
WITH BeautifulIO;
WITH lists_generic;

PROCEDURE assg3 IS

   GLOBAL_BREAKPOINTS : Boolean;
   TYPE DataArray IS Array (Integer RANGE <>, Integer RANGE <>) OF Integer;
   TYPE pDataArray IS ACCESS DataArray;
   TYPE OneDimArray IS ARRAY (Integer RANGE <>) OF Integer;

   --Beautiful IO is my custom IO package!!
   PACKAGE Beautiful_IO IS NEW BeautifulIO (LineLength => 20,
                                            MaxLineLength => 50);
   USE Beautiful_IO;

   -- Used to display sets of nodes for the paths.
   PACKAGE NodeSet IS NEW lists_generic (ElementType => Integer);
   USE NodeSet;

   --Record is used to pass arrays around
   TYPE ArrayRecord IS RECORD
      Name : BigString;
      Set  : List;
      Data : pDataArray;
   END RECORD;


   --Provides unique labels to unlabeled data axes.
   PROCEDURE NumLabel (Leading  : Natural := 0;
                       Num      : Natural;
                       Trailing : Natural := 0;
                       Here       : File := Standard_Output) IS
   BEGIN
      Spaces(Leading, Here);
      Ada.Text_IO.Put (File => Here, Item => Character'Val (Character'Pos ('A') + Num - 1));
      Spaces(Trailing, Here);
   END NumLabel;

   --Straightforward
   FUNCTION BuildTwoDimensionalArrayRecordFromFileName (Source : String) RETURN ArrayRecord IS
      InFile    : File;
      size      : Integer := 0;
      temp      : Character;
      pNewArray : pDataArray;
      NewArrayRecord : ArrayRecord;
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

            pNewArray.ALL(y,x) := pNewArray.ALL(x,y);
         END LOOP;
         Add(NewArrayRecord.Set, x);
      END LOOP;

      Ada.Text_IO.Close (File => InFile);
      NewArrayRecord.Data := pNewArray;
      NewArrayRecord.Name := BigStringify(Source);

      RETURN NewArrayRecord;
   END BuildTwoDimensionalArrayRecordFromFileName;

   --Prints the monster from above!
   PROCEDURE PrintTwoDimensionalArray (Source      : IN pDataArray;
                                       Highlighter : Integer := -1;
                                       Here        : File := Standard_Output) IS
      val : Integer;
      y : Integer;
   BEGIN
      myRepeat ("-", 90, Here);
      Clear(Here);
      Spaces(2, Here);
      FOR x IN Source.ALL'Range LOOP
         NumLabel (6, x, 0, Here);
      END LOOP;
      Clear(2, Here);

      FOR x IN Source.ALL'Range LOOP
         NumLabel (2, x, 0, Here);
         y := x;

         FOR x IN Source.ALL'First + 1 .. y LOOP
            Spaces(7, Here);
         END LOOP;

         WHILE y < Source.ALL'Last + 1 LOOP
            val := Source.ALL (x, y);
            IF val >= 0  AND val < Integer'Last THEN
               Ada.Integer_Text_IO.Put (File => Here,
                                        Item  => val,
                                        Width => 7);
            ELSE
               Ada.Text_IO.Put(Here, "  (inf)");
            END IF;
            y := y + 1;
         END LOOP;

         IF Highlighter = x THEN
            Ada.Text_IO.Put (Here, " <<<");
         END IF;
         Clear (Here);

      END LOOP;
      myRepeat ("-", 90, Here);
      Clear (2, Here);
   END PrintTwoDimensionalArray;

   --Prints more data abotu Arrays... could have overloaded this i guess.
   PROCEDURE PrintTwoDimensionalArrayRecord (Datum       : IN ArrayRecord;
                                             Highlighter : Integer := -1;
                                             Here        : File := Standard_Output) IS
   BEGIN
      --PageBreak (Stringify (Datum.Name));
      PrintTwoDimensionalArray(Datum.Data, Highlighter, Here);
   END PrintTwoDimensionalArrayRecord;

   -- Prints a set of custom labels from a list.
   PROCEDURE PrintSet (Source : List; Here : File := Standard_Output) IS
      temp : pNode := Source.Head;
   BEGIN --PrintSet
      WHILE temp /= NULL LOOP
         NumLabel (2, temp.ALL.Value, 2, Here);
         temp := temp.ALL.Next;
      END LOOP;
      Clear (Here);
   END PrintSet;


   --HW Assignment
   PROCEDURE DijskrasShortestPath (TwoDimArray : IN OUT ArrayRecord;
                                   StartNode   : IN Integer;
                                   OSTREAM : IN File := Standard_Output) IS

      distance : pDataArray := TwoDimArray.Data;
      SUBTYPE ArrayRange IS Integer RANGE distance.ALL'Range;
      v, lastv              : Integer := StartNode;
      thisWeight, weightV   : Integer := Integer'Last;
      altCost, previousCost : Integer;
      Used, tempList        : List;
      node	            : pNode;
      NoPath		    : EXCEPTION;

   BEGIN --DijskrasShortestPath

      PageBreak ("Djikstra's Algorithm by Ray Peters, May 24 2010", OSTREAM);
      PrintTwoDimensionalArrayRecord (TwoDimArray, -1, OSTREAM);

      --For every element in the array
      FOR x IN ArrayRange LOOP
         distance (v, v) := 0;

         --Compare the cost of the current path to a new alternative path.
         FOR y IN ArrayRange LOOP
            previousCost := distance (StartNode, y);
            IF Find (Used, y) = NULL THEN
               altCost := distance(StartNode, v) + distance(v, y);
               IF altCost > 0 AND THEN altCost < previousCost THEN
                  distance (StartNode, y) := altCost;
               END IF;
            END IF;
         END LOOP;
         --Touch v
         Add (Used, v);

         --Initialize weightV to guarantee replacement.
         weightV := Integer'Last;
         FOR z IN ArrayRange LOOP
            IF Find (Used, z) = NULL THEN
               thisWeight := distance(StartNode, z);
               IF thisWeight > 0 AND THEN thisWeight <= weightV THEN
                  v := z;
                  weightV := thisWeight;
               END IF;
            END IF;
         END LOOP;

         --weightV either couldn't be replaced or had no where to go
         --EXIT CONDITION!
         IF weightV < 0 OR weightV > 1000000 THEN
            --If this happens before the end of the array,
            --it means there wasn't a path to all elements
            IF x /= ArrayRange'Last THEN
               raise nopath;
            END IF;
         END IF;

         --General optioneering
         IF GLOBAL_BREAKPOINTS THEN
            PrintTwoDimensionalArrayRecord (TwoDimArray, lastv, OSTREAM);
            Ada.Text_IO.Put (OSTREAM, "     Current Path: ");
            PrintSet (Used, OSTREAM);
            --Ada.Text_IO.Skip_Line (OSTREAM);
            Ada.Text_IO.New_Line (OSTREAM);
         END IF;

         lastv := v;
      END LOOP;

      v := 0;
      --Display results and leave
      PageBreak ("Fin:  Weights by Path Step", OSTREAM);
      FOR x IN ArrayRange LOOP
         Pop (Used, node);
         v := v + distance (StartNode, node.ALL.Value);
         Ada.Integer_Text_IO.Put (OSTREAM, v, Width => 5);
         Spaces (3, OSTREAM);
         Add (tempList, node.ALL.Value);
         PrintSet (tempList, OSTREAM);
      END LOOP;

   EXCEPTION
      WHEN NoPath =>
         PageBreak ("ERR: Catastrophic Failure!!!");
         Ada.Text_IO.Put_Line ("Unused vertices:");
         FOR x IN ArrayRange LOOP
            IF Find (Used, x) = NULL THEN
               NumLabel(2, x, 2);
            END IF;
         END LOOP;
      WHEN OTHERS =>
         NULL;
   END DijskrasShortestPath;

   --Renames a monster LOL
   FUNCTION make2DArray (FileName : BigString) RETURN ArrayRecord IS
   BEGIN --make2DArray
      RETURN BuildTwoDimensionalArrayRecordFromFileName(Stringify(FileName));
   END make2DArray;



   OutputFile : BigString;
   InputFile : BigString;
   --InputFile : BigString := Include ("Input File", "blackboardtest2.dat");
   InputRecord : ArrayRecord;
   Init : Boolean := True;
   OutFile : File;
BEGIN --assg3
   LOOP
      GLOBAL_BREAKPOINTS := Ask ("Display breakpoints?");
      Clear;

      InputFile := Include ("Input File", "testdata.dat");
      InputRecord := make2DArray (InputFile);


      IF Ask ("Display results to a file?") THEN
         OutputFile := Request ("Output File");
         Open (OutFile, Out_File, Stringify(OutputFile));
         DijskrasShortestPath (InputRecord, 1, OutFile);
         Close (OutFile);
      ELSE
         DijskrasShortestPath (InputRecord, 1);
      END IF;
      Clear(2);

      EXIT WHEN NOT Ask("Repeat?");
      Clear (3);
   END LOOP;
   PageBreak("Farewell! -Ray");
END assg3;
