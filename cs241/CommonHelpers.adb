WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
WITH Ada.Float_Text_IO;

PACKAGE BODY CommonHelpers IS

   FUNCTION BuildEmptyArray (Min, Max : IndexType) RETURN pDataArray IS
      temp : pDataArray := NEW DataArray(Min..Max);
   BEGIN --BuildEmptyArray
      RETURN temp;
   END BuildEmptyArray;

   FUNCTION BuildInitializedArray (Min, Max : IndexType; InitialValue : ElementType) RETURN pDataArray IS
      temp : pDataArray := BuildEmptyArray(Min, Max);
   BEGIN --BuildInitializedArray
      temp.ALL := (OTHERS => InitialValue);
      RETURN temp;
   END BuildInitializedArray;

   FUNCTION BuildArrayFromFileName (Source : String) RETURN pDataArray IS
      pNewArray : pDataArray;
      InFile : Ada.Text_IO.File_Type;
      count : Integer := 0;
      temp : Integer;
      x : IndexType := IndexType'Val(0);
   BEGIN
      Ada.Text_IO.Open (File => InFile, Mode => Ada.Text_IO.In_File, Name => Source);
      WHILE NOT Ada.Text_IO.End_Of_File (File => InFile) LOOP
         Ada.Integer_Text_IO.Get(File => InFile, Item => temp);
         count := count + 1;
      END LOOP;
      Ada.Text_IO.Close (File => InFile);

      pNewArray := BuildEmptyArray(x, IndexType'Val(count-1));
      Ada.Text_IO.Open (File => InFile, Mode => Ada.Text_IO.In_File, Name => Source);
      WHILE NOT Ada.Text_IO.End_Of_File (File=> InFile) LOOP
         Ada.Integer_Text_IO.Get (File => InFile, Item => temp);
         pNewArray.ALL(x) := ElementType'Val(temp);
         x := IndexType'Succ(x);
      END LOOP;
      Ada.Text_IO.Close (File => InFile);

      RETURN pNewArray;
   END BuildArrayFromFileName;

   FUNCTION Insert (HashTable : pDataArray;
                    Value : ElementType;
                    Policy : ACCESS FUNCTION (thing1, thing2 : Integer) RETURN Integer) RETURN Integer IS

      n : Integer := HashTable.ALL'Length;
      i : Integer := 0;
      HashIndex : IndexType := IndexType'Val(ElementType'Pos(Value) MOD n);
      TableSize : Positive := HashTable.ALL'Length-1;
   BEGIN --Insert
      WHILE HashTable.ALL(HashIndex) /= EmptyFlag LOOP
         i := i + 1;
         HashIndex := IndexType'Val(Policy(ElementType'Pos(Value), i) MOD n);
         IF i >= TableSize THEN
            RETURN -1;
         END IF;
      END LOOP;
      HashTable.ALL(HashIndex) := Value;
      RETURN i;
   END Insert;
END CommonHelpers;
