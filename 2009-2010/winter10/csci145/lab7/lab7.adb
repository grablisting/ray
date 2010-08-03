with Ada.Text_IO;
with Ada.Integer_Text_IO;
with counting_sort;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;

PROCEDURE Lab7 IS
   PACKAGE Str RENAMES Ada.Strings.Unbounded;

   TYPE MyData IS ARRAY (Positive RANGE <>) OF Integer;
   TYPE pMyData IS ACCESS MyData;
   PROCEDURE Sort IS NEW counting_sort(ElementType => Integer, IndexType => Positive, ArrayType => MyData);
   PROCEDURE Create (HowMany : IN Integer; Between : IN Integer := 100; To : IN String) IS SEPARATE;

   InFile, OutFile : Ada.Text_IO.File_Type;
   This : pMyData;
   Source : Str.Unbounded_String;
   NextCh : Character;
   ThisMany : Positive;
BEGIN --Lab7_Sort
   Ada.Text_IO.Put ("Hey, I'm gonna create a random file, enter a name for it: ");
   Ada.Text_IO.Unbounded_IO.Get_Line (Source);

   Ada.Text_IO.Put ("How many numbers you want in it? ");
   Ada.Integer_Text_IO.Get (ThisMany);
   Create(HowMany => ThisMany, To => Str.To_String(Source));

   Ada.Text_IO.Open(File => InFile, Mode => Ada.Text_IO.In_File, Name => Str.To_String(Source));
   This := new MyData(1..ThisMany);
   FOR x IN This.ALL'Range LOOP
      Ada.Integer_Text_IO.Get (File => InFile, Item => This.ALL(x));
   END LOOP;
   Ada.Text_IO.Close(File => InFile);

   Ada.Text_IO.Put ("Sorting file... ");

   Sort(This.ALL);

   Ada.Text_IO.Create (File => OutFile, Mode => Ada.Text_IO.Out_File, Name => "sorted_" & Str.To_String(Source));
   FOR x IN This.ALL'Range LOOP
      Ada.Integer_Text_IO.Put (File => OutFile, Item => This.ALL(x));
   END LOOP;
   Ada.Text_IO.Close(File => OutFile);

   Ada.Text_IO.Put_Line (Item => "Complete.");

   Ada.Text_IO.Put ("Wanna see it(y/n)? ");
   Ada.Text_IO.Get (NextCh);

   IF NextCh /= 'N' AND NextCh /= 'n' THEN
      FOR x IN This.ALL'Range LOOP
         Ada.Integer_Text_IO.Put (This.ALL(x));
         IF x MOD 10 = 0 THEN
            Ada.Text_IO.New_Line;
         END IF;
      END LOOP;
   END IF;
END Lab7;
