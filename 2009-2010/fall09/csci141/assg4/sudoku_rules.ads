WITH Ada.Text_IO;
WITH Sudoku_Def; USE Sudoku_Def;

PACKAGE Sudoku_Rules IS
-------------------------------
-------------------------------
-- Modified by Ray Peters
-- November 29, 2009
-------------------------------

   malformed_input: EXCEPTION;

   PROCEDURE Get_Game(Game: OUT Sudoku_Game; source: IN Ada.Text_IO.File_Type);
   PROCEDURE Display_Game(st: IN Sudoku_Game; fin: IN Sudoku_Series);
   PROCEDURE Apply_Rules(st: IN Sudoku_Game; fin: OUT Sudoku_Game; rule_limit: IN Positive);

END Sudoku_Rules;
