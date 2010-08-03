-------------------------------------------------------------------------------
--  Package Sudoku_Def
--
--  This package defines the data structures for maintaining the Sudoku board.
--
--  James L. Johnson
-- November 16, 2009
-------------------------------------------------------------------------------
-------------------------------
-------------------------------
-- Modified by Ray Peters
-- November 29, 2009
-------------------------------

PACKAGE Sudoku_Def IS

   --  Size of the Board
   Board_Size : constant Positive := 9; -- 9x9 board
   Cell_Size  : constant Positive := 3; -- with 3x3 cells

   SUBTYPE Board_Size_Range IS Positive RANGE 1 .. Board_Size;

   --  All_Cell_Value gives the values that can appear in a cell. Zero is used to
   --  represent empty, not present, not known, or not specified.
   SUBTYPE All_Cell_Value IS Natural RANGE 0..Board_Size;

   TYPE Sudoku_Game IS ARRAY(Board_Size_Range, Board_Size_Range) OF All_Cell_Value;
   TYPE Sudoku_Series IS ARRAY(1 .. 3) OF Sudoku_Game;

   TYPE Dir_Type IS (row_dir, col_dir); -- row or column direction

   -- Row and column bounds define a subsquare; the dir and exp fields are used
   -- in analyzing a subsquare with an intersecting row or column as required in rule 3.
   TYPE Square IS RECORD
      row_lo, row_hi, col_lo, col_hi: Board_Size_Range;
      dir: Dir_Type;
      exp: Board_Size_Range; -- exceptional row or column
   END RECORD;

   --  Cell_Value is a sub-range of All_Cell_Value giving valid values
   SUBTYPE Cell_Value IS All_Cell_Value RANGE 1..Board_Size;

   --  Possible_Values is a boolean array. True indicates that
   --  the given Cell_Value is a possibility.
   TYPE Possible_Values IS ARRAY(Cell_Value) of Boolean;

   TYPE Possible_Value_Array IS ARRAY(Board_Size_Range, Board_Size_Range) OF Possible_Values;

   --  Data describing rules

   --  Rules is an enumeration type for which rule is being applied
   TYPE Rules IS (No_Rule           , -- No Rule is applicable
                  One_Value_For_Cell, -- Rule 1: One possible value for cell.
                  One_Cell_For_Value, -- Rule 2: One possible cell for value in a row, column, or 3x3
                  Row_Col_Square_Overlap -- Rule 3: Value restricted to overlap of a row/column and a 3x3 subsquare
                 );

END Sudoku_Def;
