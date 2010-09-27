WITH Ada.Text_IO;
WITH Sudoku_Def; USE Sudoku_Def;
WITH Sudoku_Rules; USE Sudoku_Rules;

-- Solve simple Sudoku puzzles
-- James L. Johnson
-- November 16, 2009
-------------------------------
-------------------------------
-- Modified by Ray Peters
-- November 29, 2009
-------------------------------

PROCEDURE Sudoku IS
   data_source: CONSTANT STRING := "puzzles.txt";
   data_file: Ada.Text_IO.File_Type;
   initial_game: Sudoku_Game;
   final_game: Sudoku_Series;

BEGIN -- Sudoku
   Ada.Text_IO.Open(File => data_file, Mode => Ada.Text_IO.In_File, Name => data_source);
   WHILE NOT Ada.Text_IO.End_Of_File(File => data_file) LOOP
      Get_Game(Game => initial_game, source => data_file);
      FOR i IN 1 .. 3 LOOP
         Apply_Rules(st => initial_game, fin => final_game(i), rule_limit => i);
      END LOOP;
      Display_Game(st => initial_game, fin => final_game);
      Ada.Text_IO.Skip_Line;
   END LOOP;
   Ada.Text_IO.Close(File => data_file);
EXCEPTION
   WHEN malformed_input =>
      Ada.Text_IO.New_Line(3);
      Ada.Text_IO.Put_Line(Item => "Malformed input -- program terminated");
      Ada.Text_IO.Close(File => data_file);
END Sudoku;
