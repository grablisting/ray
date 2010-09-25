WITH Ada.Text_IO, Ada.Integer_Text_IO;
WITH Ada.Numerics.Discrete_Random;

PROCEDURE Lab8 IS
  -- types, subtypes, and records

  -- The numbers used in our game are 1..15, and 16 is a special number
  SUBTYPE GameNumber     IS Natural    RANGE 1..16;
   SUBTYPE UserGameNumber IS GameNumber RANGE 1..15;

  -- The GameBoard is a 3x3 board with 8 numbers and 1 empty space
  SUBTYPE BoardDimension IS Integer RANGE 0..3;
  TYPE GameBoard IS ARRAY (BoardDimension, BoardDimension) OF GameNumber;

  -- BoardPoint describes a single point on the GameBoard
  TYPE BoardPoint IS RECORD
    r : BoardDimension;
    c : BoardDimension;
  END RECORD;

  -- constants

   EMPTY_SPACE : CONSTANT GameNumber := GameNumber'Last;

  PACKAGE RandomNumber IS NEW Ada.Numerics.Discrete_Random(GameNumber);

  -- scratchpads

  TheBoard : GameBoard;
  UserInput : UserGameNumber;
  UserPoint, SpacePoint : BoardPoint;
  Shuffle : Integer := 10; -- Number of times to shuffle
  G : RandomNumber.Generator; -- Randomize

  -- subprograms
  -- subprogram signatures

  PROCEDURE InitBoard;
  PROCEDURE DisplayBoard;
  FUNCTION  FindNumber   (toFind : GameNumber) RETURN BoardPoint;
  FUNCTION  IsSolved                           RETURN Boolean;
  FUNCTION  AreAdjacent  (P1, P2 : BoardPoint) RETURN Boolean;
  PROCEDURE SwapPoints   (P1, P2 : BoardPoint);

  -- Initializes TheBoard so that each cell of the array has a unique value.
  PROCEDURE InitBoard IS
      count : Integer := GameNumber'First;
      Temp, Temp2 : BoardPoint;
      D : GameNumber;
   BEGIN
      FOR row IN BoardDimension LOOP
        FOR col IN BoardDimension LOOP
           TheBoard(row, col) := count;
           count := count + 1;
        END LOOP;
      END LOOP;

      FOR x IN 1..Shuffle LOOP
         D := RandomNumber.Random(G);
         Temp := FindNumber(D);
         D:= RandomNumber.Random(G);
         Temp2 := FindNumber(D);

         SwapPoints(Temp, Temp2);
      END LOOP;
  END InitBoard;


  -- Displays TheBoard.
  PROCEDURE DisplayBoard IS
  BEGIN
      Ada.Text_IO.New_Line;
      FOR row IN BoardDimension LOOP
         FOR col IN BoardDimension LOOP
            IF NOT (TheBoard(row, col) = EMPTY_SPACE) THEN
               Ada.Integer_Text_IO.Put (Item => TheBoard(row, col), Width => 3);
            ELSE
               Ada.Text_IO.Put (Item => "   ");
            END IF;
         END LOOP;
         Ada.Text_IO.New_Line;
      END LOOP;
  END DisplayBoard;


  -- Returns true if TheBoard is arranged with GameNumber'First in the top
  -- left corner, GameNumber'Last in the bottom right corner, and every number
  -- is in ascending order by reading left-to-right, top-to-bottom.
   FUNCTION IsSolved RETURN Boolean IS
      check : Integer := GameNumber'First;
   BEGIN
      checker:
      FOR row IN BoardDimension LOOP
         FOR col IN BoardDimension LOOP
            IF TheBoard(row, col) = check THEN
               check := check + 1;
            ELSE
               RETURN False;
            END IF;
         END LOOP;
      END LOOP checker;
      RETURN True;
  END IsSolved;


  -- Given a GameNumber, returns the location of that number on TheBoard.
   FUNCTION FindNumber (toFind : GameNumber) RETURN BoardPoint IS
      Temp : BoardPoint;
  BEGIN
     FOR row IN BoardDimension LOOP
       FOR col IN BoardDimension LOOP
          IF TheBoard(row, col) = toFind THEN
               Temp.r := row;
               Temp.c := col;
          END IF;
       END LOOP;
      END LOOP;
      RETURN Temp;
  END FindNumber;


  -- If P1 and P2 are adjacent horizontally or vertically then returns true,
  -- otherwise returns false.
  FUNCTION AreAdjacent (P1, P2 : BoardPoint) RETURN Boolean IS

  BEGIN
      IF P1.r = P2.r THEN
         IF (P1.c = P2.c - 1) OR (P1.c = P2.c + 1) THEN
            RETURN True;
         ELSE
            RETURN False;
         END IF;
      ELSIF P1.c = P2.c THEN
         IF (P1.r = P2.r - 1) OR (P1.r = P2.r + 1) THEN
            RETURN True;
         ELSE
            RETURN False;
         END IF;
      ELSE
         RETURN False;
      END IF;
  END AreAdjacent;


  -- Swaps the values of TheBoard at P1 and P2.
   PROCEDURE SwapPoints (P1, P2 : BoardPoint) IS
      Temp : GameNumber;
  BEGIN
      Temp := TheBoard(P1.r, P1.c);
      TheBoard(P1.r, P1.c) := TheBoard(P2.r, P2.c);
      TheBoard(P2.r, P2.c) := Temp;
  END SwapPoints;

BEGIN
  RandomNumber.Reset(G);
  -- Start of the main program. You do NOT need to edit anything below this
  -- line unless you want to add additional functionality to the game.
  InitBoard;
  DisplayBoard;

  WHILE NOT IsSolved LOOP
    Ada.Text_IO.Put("Swap > ");
    LOOP
      BEGIN
        Ada.Integer_Text_IO.Get(UserInput);
        EXIT;
      EXCEPTION WHEN OTHERS =>
        Ada.Text_IO.Put("That number is not valid. Try again. > ");

        IF NOT Ada.Text_IO.End_Of_Line THEN
          Ada.Text_IO.Skip_Line;
        END IF;
      END;
    END LOOP;

    UserPoint  := FindNumber(UserInput);
    SpacePoint := FindNumber(EMPTY_SPACE);

    IF AreAdjacent(UserPoint, SpacePoint) THEN
      SwapPoints(UserPoint, SpacePoint);
      DisplayBoard;
    ELSE
      Ada.Text_IO.Put("You can't move that tile. Try again. ");
    END IF;
  END LOOP;
END Lab8;
