with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

SEPARATE (Lab7)
PROCEDURE Create (HowMany : IN Integer; Between : IN Integer := 100; To : IN String) IS
   SUBTYPE EntryIndex IS Integer RANGE 1..HowMany;
   SUBTYPE RandomRange IS Integer RANGE 1..Between;
   PACKAGE Rando IS NEW Ada.Numerics.Discrete_Random (Result_Subtype => RandomRange);
   G : Rando.Generator;

   PROCEDURE Fill (This : IN OUT Ada.Text_IO.File_Type) IS
   BEGIN --Fill
      Rando.Reset (Gen => G);
      Ada.Text_IO.Put ("Filling file...");
      FOR x IN EntryIndex LOOP
         Ada.Integer_Text_IO.Put (File => This, Item => Rando.Random(Gen => G));
      END LOOP;
      Ada.Text_IO.Put_Line (" Complete.");
   END Fill;

   OutFile : Ada.Text_IO.File_Type;
BEGIN --Lab7
   Ada.Text_IO.Create (File => OutFile, Mode => Ada.Text_IO.Out_File, Name => To);
   Fill(OutFile);
   Ada.Text_IO.Close (OutFile);
END Create;
