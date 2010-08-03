-- Reads, displays and tries to solve sudoku puzzles
-- Written by Ray Peters, November 30, 2009
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

PROCEDURE SolveSudoku IS

   SUBTYPE Numbers IS Natural RANGE 0..9;
   TYPE PotentialsArray IS ARRAY (0..9) OF Numbers;
   TYPE BoardArray IS ARRAY (0..8, 0..8) OF PotentialsArray;

   PuzzleSource : CONSTANT STRING := "puzzles.txt";
   PuzzleType : Ada.Text_IO.File_Type;
   ThisRound : BoardArray;

   FUNCTION Get_Game(Game: OUT BoardArray; Source: IN Ada.Text_IO.File_Type) IS
      NextCh : Character;
      i, x : Natural := 0;

      CellValue : Numbers;
      CellAnswers : PotentialsArray;
      CurrentGame : BoardArray;
   BEGIN
      WHILE x < 9 LOOP
         WHILE NOT Ada.Text_IO.End_Of_Line LOOP
            Ada.Text_IO.Get(Item => NextCh);
            IF NextCh = '#' THEN
               Ada.Text_IO.Skip_Line;
            ELSIF NextCh = '.' THEN
               CellAnswers := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
               CurrentGame(i, x) := CellAnswers;
               i := i + 1;
            ELSIF Ada.Characters.Handling.Is_Digit(NextCh) THEN
               CellValue := Character'Pos(NextCh) - Character'Pos('0');
               CellAnswers(CellValue) := CellValue;
               CellAnswers := (OTHERS => 0);
               CurrentGame(i, x) := CellAnswers;
               i := i + 1;
            END IF;
            EXIT WHEN i > 8;
         END LOOP;
         x := x + 1;
      END LOOP;
      Game := CurrentGame;
   END Get_Game;

BEGIN -- SolveSudoku
   Ada.Text_IO.Open(File => PuzzleType, Mode => Ada.Text_IO.In_File, Name => PuzzleSource);
   Get_Game(Game => ThisRound, Source => data_source);

   Ada.Text_IO.Put (Item => ThisRound);

--   FUNCTION Display_Game(st: IN Sudoku_Game; fin: IN Sudoku_Series);
--   FUNCTION Apply_Rules(st: IN Sudoku_Game; fin: OUT Sudoku_Game; rule_limit: IN Positive);

END SolveSudoku;
