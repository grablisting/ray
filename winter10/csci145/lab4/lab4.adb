with Ada.Integer_Text_IO;
with Ada.Text_IO;

PROCEDURE Lab4 IS
   FUNCTION PairCheck (Item : Integer) RETURN Integer IS
      Total : Integer := 0;
   BEGIN --PairCheck
      FOR x IN 1..((Item / 2) + 1) LOOP
         IF Item REM x = 0 THEN
            Total := Total + x;
         END IF;
      END LOOP;
      RETURN Total;
   END PairCheck;
   Num, Num2 : Integer;
BEGIN --Lab4
   LOOP
      Ada.Text_IO.Put (Item => "Please enter a number: ");
      Ada.Integer_Text_IO.Get (Item => Num);

      Num2 := PairCheck(Num);

      IF PairCheck(Num2) = Num THEN
         Ada.Text_IO.Put(Item => "This number is part of an amicable pair: ");
         Ada.Integer_Text_IO.Put (Item => Num, Width => 1);
         Ada.Text_IO.Put (Item => " ");
         Ada.Integer_Text_IO.Put (Item => Num2, Width => 1);
      ELSE
         Ada.Text_IO.Put (Item => "This number is not part of an amicable pair.");
      END IF;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "-----------------------------");
      Ada.Text_IO.New_Line;
   END LOOP;
END Lab4;
