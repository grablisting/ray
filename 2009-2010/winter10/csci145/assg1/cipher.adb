with Ada.Text_IO;
with Ada.Characters.Handling;

-- Cipher reads a file by character, and decodes letters via vigenere cipher
-- and a supplied keyword. Written by Ray Peters on January 15, 2009.

PROCEDURE Cipher IS
   SUBTYPE Lowercase IS Character RANGE 'a'..'z';
   SUBTYPE Uppercase IS Character RANGE 'A'..'Z';

   InData, OutData : Ada.Text_IO.File_Type;
   Key, File, SaveFile : String(1..50) := (OTHERS => '#');
   KeyLength, FileLength, SaveFileLength, row, offset, Mark : Integer := 0;
   NextCh : Character;
BEGIN --Cipher

   Ada.Text_IO.Put (Item => "Enter name of file to decode (InData): ");
   Ada.Text_IO.Get_Line (Item => File, Last => FileLength);

   Ada.Text_IO.Put (Item => "Enter name of file to save decoded message (OutData): ");
   Ada.Text_IO.Get_Line (Item => SaveFile, Last => SaveFileLength);

   Ada.Text_IO.Put (Item => "Please enter the cipher key: ");
   Ada.Text_IO.Get_Line (Item => Key, Last => KeyLength);

   Ada.Text_IO.Put (Item => "Opening ");
   Ada.Text_IO.Put (Item => File(1..FileLength));
   Ada.Text_IO.Put (Item => " and deciphering with the keyword ");
   Ada.Text_IO.Put (Item => Key(1..KeyLength));
   Ada.Text_IO.Put (Item => ":");
   Ada.Text_IO.New_Line(2);

   Ada.Text_IO.Open (File => InData, Mode => Ada.Text_IO.In_File, Name => File(1..FileLength));
   Ada.Text_IO.Create(File => OutData, Mode => Ada.Text_IO.Out_File, Name => SaveFile(1..SaveFileLength));

   WHILE NOT Ada.Text_IO.End_of_File (File => InData) LOOP
      WHILE NOT Ada.Text_IO.End_of_Line (File => InData) LOOP
         Ada.Text_IO.Get (File => InData, Item => NextCh);
         CASE NextCh IS
            WHEN Lowercase =>
               offset := 26 - (Character'Pos(Ada.Characters.Handling.To_Lower(Key(Mark REM KeyLength + 1))) - Character'Pos('a'));
               row := Character'Pos(NextCh) - Character'Pos('a');
               Ada.Text_IO.Put (Item => Character'Val(((row + offset) REM 26) + Character'Pos('a')));
               Ada.Text_IO.Put (File => OutData, Item => Character'Val(((row + offset) REM 26) + Character'Pos('a')));
               Mark := Mark + 1;
            WHEN Uppercase =>
               offset := 26 - (Character'Pos(Key(Mark REM KeyLength + 1)) - Character'Pos('A'));
               row := Character'Pos(NextCh) - Character'Pos('A');
               Ada.Text_IO.Put (Item => Character'Val(((row + offset) REM 26) + Character'Pos('A')));
               Ada.Text_IO.Put (File => OutData, Item => Character'Val(((row + offset) REM 26) + Character'Pos('A')));
               Mark := Mark + 1;
            WHEN OTHERS =>
               Ada.Text_IO.Put (Item => NextCh);
               Ada.Text_IO.Put (File => OutData, Item => NextCh);
         END CASE;
      END LOOP;
      Ada.Text_IO.Skip_Line(File => InData);
      Ada.Text_IO.New_Line(File => OutData);
      Ada.Text_IO.New_Line;
   END LOOP;
   Ada.Text_IO.Close (File => OutData);
   Ada.Text_IO.Close (File => InData);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "--------------------------------------");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Saved decoded message to file ");
   Ada.Text_IO.Put (Item => SaveFile(1..SaveFileLength));
   Ada.Text_IO.Put (Item => "! Farewell.");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "--------------------------------------");
END Cipher;
