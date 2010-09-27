-- Reads, displays and tries to solve sudoku puzzles
-- Written by Ray Peters, November 30, 2009
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

PROCEDURE SolveSudoku IS

   TYPE PotentialsArray IS ARRAY (1..9) OF Boolean; -- Possible values for use to solve
   TYPE BoardArray IS ARRAY (1..9, 1..9) OF PotentialsArray; -- Math Sudoku Board
   TYPE PotentialsQuadrant IS ARRAY (0..2, 0..2) OF PotentialsArray; -- Displayed Sudoku Board
   SUBTYPE CellDigit IS Natural RANGE 0..9; -- Possible digits to display to user
   TYPE DisplayBoard IS ARRAY (1..9, 1..9) OF CellDigit; -- Displayed Sudoku Board

   -- Scratch pads #1
   Puzzle : Ada.Text_IO.File_Type;
   ThisGame, ThisGame_Solved : DisplayBoard;

   -- This procedure is given a file, and outputs both a replica of the board inside the sudoku file
   PROCEDURE Get_Game (Display : OUT DisplayBoard; Source : IN Ada.Text_IO.File_Type) IS
      NextCh : Character; -- Grab single character
      Checker : Boolean := False; -- Denotes valuable information
   BEGIN
      FOR row IN 1..9 LOOP -- recurse rows
         FOR col IN 1..9 LOOP -- recurse columns
            WHILE NOT Checker LOOP -- loop until finds something valuable
               Ada.Text_IO.Get (File => Source, Item => NextCh); -- gather a single character

               -- Check to see if the character is what we are looking for; skips lines starting with '#'
               IF NextCh = '#' THEN -- Skip marker
                  Ada.Text_IO.Skip_Line(File => Source);
               ELSIF NextCh = '.' THEN -- count '.' as valid input
                  Display(row, col) := 0; -- denote with a zero
                  Checker := True; -- found a valuable piece of info
               ELSIF Ada.Characters.Handling.Is_Digit(NextCh) THEN -- count digits as valid input
                  Display(row, col) := Character'Pos(NextCh) - Character'Pos('0'); -- update the display sudoku board
                  Checker := True; -- found a valuable piece of info
               END IF;
            END LOOP;
            Checker := False; -- Reset for next pass
         END LOOP;
      END LOOP;
   END Get_Game;

   PROCEDURE Solver (Item : IN DisplayBoard; Display : OUT DisplayBoard) IS
      TempDisplay : DisplayBoard;
      ThisCell, CellPossibles : PotentialsArray;
      QuadrantPossibles : PotentialsArray := (OTHERS => True);
      Quadrant, QuadrantChecker : PotentialsQuadrant := (OTHERS => (OTHERS => QuadrantPossibles)) ;
      Trigger, Mark : Natural;
      Whiteboard, WhiteboardChecker : BoardArray;
   BEGIN -- Solver
      TempDisplay := Item; -- Grab the board created from the file

      -- Build a mathmatical model of the same board
      FOR row IN 1..9 LOOP
         FOR col IN 1..9 LOOP
            Trigger := TempDisplay(row, col); -- Grab the value at this cell
            ThisCell := (OTHERS => True); -- Set default values to true
            IF NOT (TempDisplay(row, col) = 0) THEN -- Look for digits other than zero (zero's are seen as valueless)
               FOR x in 1..9 LOOP -- Loop through PotentialsArray
                  IF NOT (Trigger = x) THEN -- sets everything but the value of the cell to cell
                     ThisCell(x) := False;
                  END IF;
               END LOOP;
            END IF;
            Whiteboard(row, col) := ThisCell; -- save work
         END LOOP;
      END LOOP;

      -- The following loop uses cells in the Display Board to manipulate the Math board
      LOOP -- general loop runs until it can't make more changes
         Display := TempDisplay; -- Comparison for EXIT WHEN conditional (before and after)
         WhiteboardChecker := Whiteboard; -- Comparison for EXIT WHEN conditional (before and after)

         LOOP
            QuadrantChecker := Quadrant; -- Comparison Quadrant

            -- quadrant section divides the sudoku board into 9, 3x3 squares
            -- build quadrant possibles by looking for non-zero digits
            FOR QRow IN 0..2 LOOP
               FOR QCol IN 0..2 LOOP
                  QuadrantPossibles := (OTHERS => True); -- Assume everything is valid

                  FOR Here IN (3 * QRow) + 1..(3 * QRow) + 3 LOOP -- cell selection loop based on outer quadrant loop
                     FOR This IN (3 * QCol) + 1..(3 * QCol) + 3 LOOP -- cell selection
                        IF NOT (TempDisplay(Here, This) = 0) THEN -- Look for non-zero digits
                           Trigger := TempDisplay(Here, This); -- get value
                           QuadrantPossibles(Trigger) := False; -- set that value to false in possibles
                        END IF;
                     END LOOP;
                  END LOOP;
                  Quadrant(QRow, QCol) := QuadrantPossibles; -- save quadrant
               END LOOP;
            END LOOP;

            -- use built quadrants to manipulate zero digits
            FOR QRow IN 0..2 LOOP
               FOR QCol IN 0..2 LOOP
                  QuadrantPossibles := Quadrant(QRow, QCol); -- load possibles by quadrant

                  FOR Here IN (3 * QRow) + 1..(3 * QRow) + 3 LOOP -- cell selection
                     FOR This IN (3 * QCol) + 1..(3 * QCol) + 3 LOOP -- dynamic cell selection based from Quadrant loop
                        IF TempDisplay(Here, This) = 0 THEN -- look for zeros
                           ThisCell := Whiteboard(Here, This); -- grab existing possibles at this cell
                           FOR Update IN 1..9 LOOP -- update existing cells with quadrant possibles
                              IF NOT QuadrantPossibles(Update) THEN -- get false values
                                 ThisCell(Update) := False; -- update active cell
                              END IF;
                           END LOOP;
                           Whiteboard(Here, This) := ThisCell; -- save work
                        END IF;
                     END LOOP;
                  END LOOP;
               END LOOP;
            END LOOP;
            EXIT WHEN QuadrantChecker = Quadrant; -- exit general loop if no changes were made
         END LOOP;

         FOR row IN 1..9 LOOP
            FOR col IN 1..9 LOOP

               IF TempDisplay(row, col) > 0 THEN -- look for non-zero digits in display board
                  Trigger := TempDisplay(row, col); -- save digit
                  FOR RobustCheck IN 1..9 LOOP -- loop used to skim through appropriate rows / columns
                     -- Rows
                     IF NOT (row = RobustCheck) THEN -- skip it's own row
                        IF TempDisplay(RobustCheck, col) = 0 THEN -- look for zero digits
                           CellPossibles := Whiteboard(RobustCheck, col); -- load it's possible values
                           CellPossibles(Trigger) := False; -- set saved digit as false
                           Whiteboard(RobustCheck, col) := CellPossibles; -- save work
                        END IF;
                     END IF;

                     --Same thing for columns
                     IF NOT (col = RobustCheck) THEN
                        IF TempDisplay(row, RobustCheck) = 0 THEN
                           CellPossibles := Whiteboard(row, RobustCheck);
                           CellPossibles(Trigger) := False;
                           Whiteboard(row, RobustCheck) := CellPossibles;
                        END IF;
                     END IF;
                  END LOOP;
                  END IF;

               ThisCell := Whiteboard(row, col); -- Address current cell address
               Mark := 0; -- reset Mark
               FOR Here IN 1..9 LOOP -- count number of true values at current cell
                  IF ThisCell(Here) THEN
                     Mark := Mark + 1;
                  END IF;
               END LOOP;

               IF Mark = 1 THEN -- if only one true value exists, edit the DisplayBoard
                  FOR Here IN 1..9 LOOP
                     IF ThisCell(Here) THEN
                        TempDisplay(row, col) := Here; -- Save to Display Board
                     END IF;
                  END LOOP;
                  -- manipulate converging cells that contain zeros upon finalizing a value on the Display Board
                  FOR RobustUpdate IN 1..9 LOOP
                     IF NOT (col = RobustUpdate) THEN -- skip it's own column
                        IF TempDisplay(row, RobustUpdate) = 0 THEN
                           CellPossibles := Whiteboard(row, RobustUpdate);
                           CellPossibles(TempDisplay(row, col)) := False;
                           Whiteboard(row, RobustUpdate) := CellPossibles;
                        END IF;
                     END IF;
                     IF NOT (row = RobustUpdate) THEN
                        IF TempDisplay(RobustUpdate, col) = 0 THEN
                           CellPossibles := Whiteboard(RobustUpdate, col);
                           CellPossibles(TempDisplay(row, col)) := False;
                           Whiteboard(RobustUpdate, col) := CellPossibles;
                        END IF;
                     END IF;
                  END LOOP;
               END IF;
            END LOOP;
         END LOOP;
      EXIT WHEN TempDisplay = Display AND WhiteboardChecker = Whiteboard; -- Exit loop when no changes have been made
      END LOOP;
   END Solver;

   count : Integer := 1; -- aesthetic counter
BEGIN -- SolveSudoku
   Ada.Text_IO.Open (File => Puzzle, Name => "puzzles.txt", Mode => Ada.Text_IO.In_File); -- open file

   WHILE NOT (Ada.Text_IO.End_Of_File(File => Puzzle)) LOOP -- Continue until reaching EOF marker
      -- Grab the next game
      Get_Game (Display => ThisGame, Source => Puzzle);
      -- Apply rules to it
      Solver (Item => ThisGame, Display => ThisGame_Solved);

      -- Aesthetic breaks
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "------------------------------------------------");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "--- Input for Game #");
      Ada.Integer_Text_IO.Put (Item => count, Width => 1);
      Ada.Text_IO.Put (Item => " -------- Solved ---------");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "------------------------------------------------");
      Ada.Text_IO.New_Line;

      -- Summon values from Input Game & Solved Game by row
      FOR row IN 1..9 LOOP
         Ada.Text_IO.Put (Item => "|| "); -- aesthetics
         FOR col IN 1..9 LOOP
            IF ThisGame(row, col) = 0 THEN
               Ada.Text_IO.Put (Item => " ."); -- aesthetics
            ELSE
               Ada.Integer_Text_IO.Put (Item => ThisGame(row, col), Width => 2);
            END IF;
         END LOOP;

         Ada.Text_IO.Put (Item => "  || "); -- aesthetics

         FOR col IN 1..9 LOOP
            IF ThisGame_Solved(row, col) = 0 THEN
               Ada.Text_IO.Put (Item => " ."); -- aesthetics
            ELSE
               Ada.Integer_Text_IO.Put (Item => ThisGame_Solved(row, col), Width => 2);
            END IF;
         END LOOP;

         Ada.Text_IO.Put (Item => "  ||"); -- aesthetics
         Ada.Text_IO.New_Line;
      END LOOP;
      -- Aesthetic break
      Ada.Text_IO.Put (Item => "------------------------------------------------");
      Ada.Text_IO.New_Line;
      count := count + 1; -- counter
      Ada.Text_IO.Skip_Line; -- Pause for user paging
   END LOOP;

   Ada.Text_IO.Close(File => Puzzle); -- Close file
END SolveSudoku;
