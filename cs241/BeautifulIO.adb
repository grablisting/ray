
PACKAGE BODY BeautifulIO IS
   FUNCTION Stringify (Or_Not_To_String : BigString) RETURN String IS
   BEGIN --Stringify
      RETURN Str.To_String(Or_Not_To_String);
   END Stringify;

   FUNCTION BigStringify (That_Is_The_Question : String) RETURN BigString IS
   BEGIN --BigStringify
      RETURN Str.To_Unbounded_String(That_Is_The_Question);
   END BigStringify;

   PROCEDURE Clear (To : File_Type := Standard_Output) IS
   BEGIN
      Ada.Text_IO.New_Line (File => To);
   END Clear;

   PROCEDURE Clear (num : Natural; To : File_Type := Standard_Output) IS
   BEGIN
      FOR x IN 1 .. num LOOP
         Clear (To);
      END LOOP;
   END Clear;

   PROCEDURE Spaces(num : Natural; To : File_Type := Standard_Output) IS
   BEGIN
      myRepeat (" ", num, To);
   END Spaces;

   PROCEDURE myRepeat (Obj : String; Times : Natural; To : File_Type := Standard_Output) IS
   BEGIN
      FOR x IN 1 .. Times LOOP
         Ada.Text_IO.Put (File => To, Item => Obj);
      END LOOP;
   END myRepeat;

   PROCEDURE PageBreak (Message : String; Here : File_Type := Standard_Output) IS
      breaks : Natural := 2;
      TextWrapper : Integer := MaxLineLength - Message'Length;
   BEGIN --PageBreak
      Clear (Here);
      FOR x IN 0..breaks LOOP
         Spaces (x, Here);
         myRepeat ("-", 5, Here);
         IF x = breaks THEN
            Spaces (MaxLineLength, Here);
         ELSE
            myRepeat ("-", MaxLineLength + x - 1, Here);
         END IF;
         myRepeat("-", 5, Here);
         Clear (Here);
      END LOOP;

      Spaces(breaks + 1, Here);
      myRepeat("-", 5, Here);
      Spaces (TextWrapper / 2, Here);
      Ada.Text_IO.Put (Here, Message);
      Spaces (TextWrapper - TextWrapper / 2, Here);
      myRepeat("-", 5, Here);
      Clear(Here);

      FOR x IN REVERSE 0 .. breaks LOOP
         Spaces (x, Here);
         myRepeat ("-", 5, Here);
         IF x = breaks THEN
            Spaces (MaxLineLength, Here);
         ELSE
            myRepeat ("-", MaxLineLength + x - 1, Here);
         END IF;
         myRepeat("-", 5, Here);
         Clear(Here);
      END LOOP;
      Clear(Here);
   END PageBreak;



   PROCEDURE PrintInt (ExtraLong : Boolean; Name : String; Value : Integer; ExtraLineBreaks : Natural) IS
   BEGIN --PrintInt
      Ada.Text_IO.Put (Name & " ");
      IF ExtraLong THEN
         FOR x IN Name'Length..LineLength LOOP
            Ada.Text_IO.Put (" ");
         END LOOP;
      END IF;
      Ada.Integer_Text_IO.Put (Value, Width => 10);

      FOR x IN 1..ExtraLineBreaks LOOP
         Ada.Text_IO.New_Line;
      END LOOP;

   END PrintInt;

   PROCEDURE PrintLongInt (ExtraLong : Boolean; Name : String; Value : Long_Long_Integer; ExtraLineBreaks : Natural) IS
   BEGIN --PrintLongInt
      Ada.Text_IO.Put (Name & " ");
      IF ExtraLong THEN
         FOR x IN Name'Length..LineLength LOOP
            Ada.Text_IO.Put (" ");
         END LOOP;
      END IF;

      Ada.Long_Long_Integer_Text_IO.Put (Value, Width => 10);

      FOR x IN 1..ExtraLineBreaks LOOP
         Ada.Text_IO.New_Line;
      END LOOP;

   END PrintLongInt;

   PROCEDURE PrintFloat (ExtraLong : Boolean; Name : String; Value : Float; AfterVal : String; ExtraLineBreaks : Natural) IS
   BEGIN --PrintFloat
      Ada.Text_IO.Put (Name & " ");
      IF ExtraLong THEN
         FOR x IN Name'Length..LineLength LOOP
            Ada.Text_IO.Put (" ");
         END LOOP;
      END IF;
      Ada.Float_Text_IO.Put (Value, Fore => 8, Aft => 1, Exp => 0);
      Ada.Text_IO.Put (AfterVal);

      FOR x IN 1..ExtraLineBreaks LOOP
         Ada.Text_IO.New_Line;
      END LOOP;
   END PrintFloat;

   PROCEDURE PrintString (ExtraLong : Boolean; Name : String; Value : String; ExtraLineBreaks : Natural) IS
   BEGIN --PrintString
      Ada.Text_IO.Put (Name & " ");
      IF ExtraLong THEN
         FOR x IN Name'Length..LineLength LOOP
            Ada.Text_IO.Put (" ");
         END LOOP;
      END IF;
      Ada.Text_IO.Put (Value);

      FOR x IN 1..ExtraLineBreaks LOOP
         Ada.Text_IO.New_Line;
      END LOOP;
   END PrintString;

   PROCEDURE PrintBigString (ExtraLong : Boolean; Name : String; Value : BigString; ExtraLineBreaks : Natural) IS
   BEGIN --PrintBigString
      Ada.Text_IO.Put (Name & " ");
      IF ExtraLong THEN
         FOR x IN Name'Length..LineLength LOOP
            Ada.Text_IO.Put (" ");
         END LOOP;
      END IF;
      Ada.Text_IO.Put (Stringify(Value));

      FOR x IN 1..ExtraLineBreaks LOOP
         Ada.Text_IO.New_Line;
      END LOOP;

   END PrintBigString;

   FUNCTION Percentage (Thing1, Thing2 : Integer) RETURN Float IS
   BEGIN --Percentage
      RETURN Float(Thing1)/Float(Thing2);
   END Percentage;

   FUNCTION BigPercentage (Thing1 : Long_Long_Integer; Thing2 : Integer) RETURN Float IS
   BEGIN --Percentage
      RETURN Float(Long_Long_Float(Thing1)/Long_Long_Float(Thing2));
   END BigPercentage;


   FUNCTION AskForValidFileName (Title : String; OutputBool : Boolean) RETURN BigString IS
      temp : File;
      FileName : BigString;
   BEGIN --AskForValidInputFileName
      LOOP
         BEGIN
            Ada.Text_IO.Put ("Please enter the name of your " & Title & ": ");
            Ada.Text_IO.Unbounded_IO.Get_Line (Item => FileName);

            IF OutputBool THEN
               Ada.Text_IO.Create (File => temp, Mode => Ada.TexT_IO.In_File, Name => Stringify(FileName));
               PrintString (true, "Beautiful success!", "Created " & Title & " (" & Stringify(FileName) & ")!", 1);
            ELSE
               Ada.Text_IO.Open (File => temp, Mode => Ada.Text_IO.In_File, Name => Stringify(FileName));
               PrintString (true, "Beautiful success!", "Loaded " & Stringify(FileName) & "!", 1);
            END IF;
            Ada.Text_IO.Close (File => temp);

            RETURN FileName;
         EXCEPTION
            WHEN OTHERS =>
               Ada.Text_IO.New_Line;
               IF OutputBool THEN
                  PrintString (true, "ERROR...", "I couldn't create " & Stringify (FileName) & "!! :(", 2);
               ELSE
                  PrintString (true, "ERROR...", "I couldn't find " & Stringify (FileName) & "!! :(", 2);
               END IF;
         END;
      END LOOP;
   END AskForValidFileName;

   FUNCTION Ask (Question : String) RETURN Boolean IS
      Reply : Character;
   BEGIN --Ask
      Clear;
      PrintString (true, Question & " (y/n)?", "", 0);
      Ada.Text_IO.Get (Reply);
      Ada.Text_IO.Skip_Line;

      CASE Reply IS
         WHEN 'y' =>
            RETURN true;
         WHEN 'Y' =>
            RETURN true;
         WHEN OTHERS =>
            RETURN false;
      END CASE;
   END Ask;


   FUNCTION Include (Title, Location : String) RETURN BigString IS
      temp : File;
   BEGIN --Include
      IF Ask ("Autoload " & Title & " from " & Location) THEN
         Ada.Text_IO.Open (File => temp, Mode => Ada.Text_IO.In_File, Name => Location);
         Ada.Text_IO.Close (File => temp);
         PrintString (true, "Beautiful success!", "Loaded " & Location & "!", 1);
         RETURN BigStringify(Location);
      ELSE
         RETURN AskForValidFileName(Title, false);
      END IF;
   EXCEPTION
      WHEN Ada.Text_IO.Name_Error =>
         Ada.Text_IO.New_Line;
         PrintString(true, "ERROR...", "I couldn't find " & Location & "!! :(", 2);
         RETURN AskForValidFileName(Title, false);
   END Include;

   FUNCTION Request (Title : String) RETURN BigString IS
      temp : File;
   BEGIN --Request
      RETURN AskForValidFileName(Title, true);
   END Request;

END BeautifulIO;
