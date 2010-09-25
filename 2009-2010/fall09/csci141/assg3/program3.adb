-- Program will sum two nonnegative integers with 30 digits or less,
-- and silently ignores non-digit characters.
-- Written by Ray Peters, November 14, 2009.

with Ada.Text_IO;
with Ada.Integer_Text_IO;
WITH Ada.Characters.Handling;

PROCEDURE Program3 IS

   max : CONSTANT Natural := 31;
   SUBTYPE Long_Integer IS String(1..max);
   SUBTYPE Index IS Natural RANGE 0..(max - 1); -- For backwards building

   arg1, arg2, sum : Long_Integer;

   -- Procedure to clean user input
   PROCEDURE Input (Item : OUT String) IS
      i : Natural := 0;
      IntCopy : Long_Integer;
      InChar : Character;
   BEGIN
      LOOP
         BEGIN
            WHILE NOT Ada.Text_IO.End_Of_Line LOOP -- Runs until EOL marker
               Ada.Text_IO.Get (Item => InChar);
               IF Ada.Characters.Handling.Is_Digit(InChar) THEN -- Saves value if it's a digit
                  i := i + 1;
                  IntCopy(i) := InChar; -- Second String to format the first
               END IF;
               EXIT WHEN i > (max - 2);
            END LOOP;
            Ada.Text_IO.Skip_Line; -- clean buffer
            EXIT;
         EXCEPTION
            WHEN Ada.Text_IO.Name_Error =>
               Ada.Text_IO.Skip_Line; -- clean buffer on all exceptions
            WHEN Ada.Text_IO.End_Error =>
               Ada.Text_IO.Skip_Line;
            WHEN Ada.Text_IO.Data_Error =>
               Ada.Text_IO.Skip_Line;
         END;
      END LOOP;

      FOR l in 0..(i - 1) LOOP -- Range based on the number of valid digits
         Item(max - l) := IntCopy(i - l); -- Last slot in Item is equal to last valid digit
         FOR z in 1..(max - i) LOOP
           Item(z) := ' '; -- Formats the number with leading spaces
         END LOOP;
      END LOOP;
   END Input;

   Arg1Val, Arg2Val : Integer;
   IVal : Integer := 0;
   ThisRound : Integer;

BEGIN -- Program3
-- Greeting
   Ada.Text_IO.Put (Item => "WELCOME TO RAY'S LONG INTEGER CALCULATOR!");
   Ada.Text_IO.New_Line;

   -- Ruler
   Ada.Text_IO.Put (Item => "--------------------A Ruler----------------------0        1         2         3");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "--------------------For Your Trouble-------------123456789012345678901234567890");
   Ada.Text_IO.New_Line;

   -- User Input # 1
   Ada.Text_IO.Put (Item => "#1 Please enter a 30 digit, nonnegative integer: ");
   Input (Item => arg1);

   -- User Input # 2
   Ada.Text_IO.Put (Item => "#2 Please enter a 30 digit, nonnegative integer: ");
   Input (Item => arg2);

   -- Display Resultant arguments
   Ada.Text_IO.Put (Item => arg1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => arg2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "sum----------------------------");
   Ada.Text_IO.New_Line;

-- Failed attempt to build the sum string backwards
--     FOR n IN Index LOOP
--        ThisRound := (max - n);
--
--        IF Character'Pos(arg1(ThisRound)) = Character'Pos(' ') THEN
--           Arg1Val := 0;
--        ELSE
--           Arg1Val := (Character'Pos(arg1(ThisRound)) - Character'Pos('0'));
--        END IF;
--
--        IF Character'Pos(arg2(ThisRound)) = Character'Pos(' ') THEN
--           Arg2Val := 0;
--        ELSE
--           Arg2Val := (Character'Pos(arg2(ThisRound)) - Character'Pos('0'));
--        END IF;
--
--        SumVal := IVal + Arg1Val + Arg2Val;
--
--
--        IF SumVal > 9 THEN
--           IVal := 1;
--           sum(ThisRound) := Character'Val(SumVal REM 10);
--        ELSE
--           IVal := 0;
--           sum(ThisRound) := Character'Val(SumVal);
--        END IF;
--
--     END LOOP;

   FOR n IN Index LOOP
      -- Work backwards
      ThisRound := max - n;

      -- Make sure the digit exists, problem because of the early formating
      IF Ada.Characters.Handling.Is_Digit(arg1(ThisRound)) THEN
         Arg1Val := Character'Pos(arg1(ThisRound)) - Character'Pos('0');
      ELSE
         Arg1Val := 0;
      END IF;

      -- Make sure a digit exists, problem because of the early formating
      IF Ada.Characters.Handling.Is_Digit(arg2(ThisRound)) THEN
         Arg2Val := Character'Pos(arg2(ThisRound)) - Character'Pos('0');
      ELSE
         Arg2Val := 0;
      END IF;

      --Calculate the sum
      sum(ThisRound) := Character'Val((IVal + Arg1Val + Arg2Val) REM 10);

      --Carry the 1 boolean
      IF Arg1Val + Arg2Val > 9 THEN
         IVal := 1;
      ELSE
         IVal := 0;
      END IF;

   END LOOP;

   --Display the Sum
   FOR d IN 1..max LOOP
      Ada.Text_IO.Put (Item => sum(d));
   END LOOP;

   Ada.Text_IO.New_Line;
   -- Wish my program worked, farewell...

END Program3;
