
PACKAGE BODY BeautifulIO IS
   FUNCTION Stringify (Or_Not_To_String : BigString) RETURN String IS
   BEGIN --Stringify
      RETURN Str.To_String(Or_Not_To_String);
   END Stringify;

   FUNCTION BigStringify (That_Is_The_Question : String) RETURN BigString IS
   BEGIN --BigStringify
      RETURN Str.To_Unbounded_String(That_Is_The_Question);
   END BigStringify;


   PROCEDURE PageBreak (Message : String; Lines : Natural) IS
      breaks : Integer := Lines / 2 + 1;
      MaxBreakLength : Integer := LineLength * 4;
      TextWrapper : Integer := MaxBreakLength - Message'Length - 12;
   BEGIN --PageBreak
      Ada.Text_IO.New_Line;
      FOR x IN 1..breaks LOOP
         FOR x IN 1..MaxBreakLength LOOP
            Ada.Text_IO.Put ("-");
         END LOOP;
         Ada.Text_IO.New_Line;
      END LOOP;

      Ada.Text_IO.Put ("----- ");
      FOR x IN 1..TextWrapper/2 LOOP
         Ada.Text_IO.Put (" ");
         TextWrapper := TextWrapper - 1;
      END LOOP;

      Ada.Text_IO.Put (Message);

      FOR x IN 1..TextWrapper LOOP
         Ada.Text_IO.Put (" ");
      END LOOP;
      Ada.Text_IO.Put (" -----");
      Ada.Text_IO.New_Line;

      FOR x IN 1..breaks LOOP
         FOR x IN 1..MaxBreakLength LOOP
            Ada.Text_IO.Put ("-");
         END LOOP;
         Ada.Text_IO.New_Line;
      END LOOP;
      Ada.Text_IO.New_Line;
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
            ELSE
               Ada.Text_IO.Open (File => temp, Mode => Ada.Text_IO.In_File, Name => Stringify(FileName));
            END IF;

            Ada.Text_IO.Close (File => temp);

            PrintString(true, "Beautiful success!", "", 2);
            RETURN FileName;
         EXCEPTION
            WHEN Ada.Text_IO.Name_Error =>
               Ada.Text_IO.New_Line;
         PrintString(false, "ERROR...", "I couldn't find " & Stringify(FileName) & "!! :(", 0);
               Ada.Text_IO.New_Line;
               Ada.Text_IO.New_Line;
         END;
      END LOOP;
   END AskForValidFileName;

   FUNCTION Include (Title, Location : String) RETURN BigString IS
      temp : File;
   BEGIN --Include
      Ada.Text_IO.Open (File => temp, Mode => Ada.Text_IO.In_File, Name => Location);
      Ada.Text_IO.Close (File => temp);
      Ada.Text_IO.New_Line;
      PrintString(true, "Beautiful success!", "Loaded " & Title & " (" & Location & ")!", 2);
      Ada.Text_IO.New_Line;
      RETURN BigStringify(Location);
   EXCEPTION
      WHEN Ada.Text_IO.Name_Error =>
         Ada.Text_IO.New_Line;
         PrintString(false, "ERROR...", "I couldn't find " & Location & "!! :(", 0);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.New_Line;
         RETURN AskForValidFileName(Title, false);
   END Include;

END BeautifulIO;
