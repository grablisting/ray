WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
WITH BeautifulIO;

PROCEDURE assg3 IS

   TYPE DataArray IS Array (Integer RANGE <>, Integer RANGE <>) OF Integer;
   TYPE pDataArray IS ACCESS DataArray;

   PACKAGE Beautiful_IO IS NEW BeautifulIO (LineLength => 20);
   USE Beautiful_IO;

   PROCEDURE myRepeat (Obj : String; Times : Natural) IS
   BEGIN
      FOR x IN 1 .. Times LOOP
         Ada.Text_IO.Put (Obj);
      END LOOP;
   END myRepeat;


   PROCEDURE NumLabel (Leading : Natural := 0; Num : Natural; Trailing : Natural := 0; NewLines : Natural := 0) IS
   BEGIN
      myRepeat(" ", Leading);
      Ada.Text_IO.Put (Character'Val (Character'Pos ('A') + Num - 1));
      myRepeat (" ", Trailing);

      FOR x IN 1 .. NewLines LOOP
         Ada.Text_IO.New_Line;
      END LOOP;
   END NumLabel;

   FUNCTION BuildTwoDimensionalArrayFromFileName (Source : String) RETURN pDataArray IS
      pNewArray : pDataArray;
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
         END LOOP;
      END LOOP;

      Ada.Text_IO.Close (File => InFile);

      RETURN pNewArray;
   END BuildTwoDimensionalArrayFromFileName;

   PROCEDURE PrintTwoDimensionalArray (Source : pDataArray) IS
      val : Integer;
   BEGIN
      Ada.Text_IO.Put("   ");
      FOR x IN Source.ALL'Range LOOP
         NumLabel (Leading => 4, Num => x);
      END LOOP;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;

      FOR x IN Source.ALL'Range LOOP
         NumLabel (Leading => 2, Num => x);
         FOR y IN Source.ALL'Range LOOP
            val := Source.ALL (x, y);
            IF val >= 0 THEN
               Ada.Integer_Text_IO.Put (val, Width => 5);
            ELSE
               Ada.Text_IO.Put ("     ");
            END IF;
         END LOOP;
         Ada.Text_IO.New_Line;
      END LOOP;
      Ada.Text_IO.New_Line;
   END PrintTwoDimensionalArray;


   InputFile : BigString := Include ("Input File", "testdata.dat");
   Collection : pDataArray := BuildTwoDimensionalArrayFromFileName (Stringify (InputFile));
   min, val : Integer := -1;
BEGIN --assg3
   PrintTwoDimensionalArray (Collection);



   FOR x IN Collection.ALL'Range LOOP
      val := 0;
      min := Collection.ALL(x, 1);
      WHILE min <= 0 AND val < Collection.ALL'Last LOOP
         val := val + 1;
         min := Collection.ALL (x, val);
      END LOOP;
      min := val;

      FOR y IN x..Collection.ALL'Last LOOP
         val := Collection.ALL (x, y);
         IF val > 0 AND val < Collection.ALL(x, min) THEN
            min := y;
         END IF;
      END LOOP;
      NumLabel (Leading => 2, Num => x, Trailing => 2, NewLines => 0);
      NumLabel (Leading => 2, Num => min, Trailing => 2, NewLines => 1);
   END LOOP;
   Ada.Text_IO.New_Line;

END assg3;
