with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Characters.Handling;

PROCEDURE Lab1 IS
   InputFile : String := "DataFile.txt";

   SUBTYPE IDRange IS Positive RANGE 1111..9999;
   SUBTYPE NameRange IS Positive RANGE 1..20;
   SUBTYPE NameType IS String(NameRange);
   SUBTYPE NonNegFloat IS Float RANGE 0.0..Float'Last;

   TYPE PayCategories IS (Unknown, Professional, Sales, Clerical);
   TYPE Employee (PayStatus : PayCategories := Unknown) IS RECORD
      ID : IDRange := 1111;
      NameLength : NameRange := 20;
      Name : NameType := "                    ";

      CASE PayStatus IS
         WHEN Professional =>
            MonthlySalary : NonNegFloat;
         WHEN Sales =>
            WeeksSalary : NonNegFloat;
            CommRate : NonNegFloat;
            SalesAmount : NonNegFloat;
         WHEN Clerical =>
            HourlyWage : NonNegFloat;
            Hours : NonNegFloat;
         WHEN Unknown =>
            NULL;
      END CASE;
   END RECORD;

   ProfessionalEmployee : Employee(Professional);
   SalesEmployee : Employee(Sales);
   ClericalEmployee : Employee(Clerical);
   CurrentEmployee, UnknownEmployee : Employee;

   SUBTYPE ArraySize IS Natural RANGE 1..5;
   TYPE EmployeeArray IS ARRAY (ArraySize, ArraySize) OF Employee;
   Employed : EmployeeArray := (OTHERS => (OTHERS => CurrentEmployee));

   FUNCTION GetNumber (From: String) RETURN Integer IS
      Num0, Num1 : Integer := 0;
   BEGIN
      FOR x IN 1..From'Last LOOP
         Num1 := Character'Pos(From(x)) - Character'Pos('0');
         Num0 := (Num0 * 10) + Num1;
      END LOOP;
      RETURN Num0;
   END GetNumber;

   PROCEDURE Input (DataFrom: IN String; Matrix: OUT EmployeeArray) IS
      NextLine : String(1..30);
      DataFeed, DataCheck, LineLength : Integer := 0;
      Num0, Marker, row, col : Integer := 0;
      InData : Ada.Text_IO.File_Type;
      Jump : EXCEPTION;
   BEGIN
      Ada.Text_IO.Open (File => InData, Mode => Ada.Text_IO.In_File, Name => DataFrom);
      LOOP
         BEGIN
            WHILE NOT Ada.Text_IO.End_of_File (File => InData) LOOP
               IF DataFeed = 0 THEN
                  Ada.Text_IO.Get_Line (File => InData, Item => NextLine, Last => LineLength);

                  CASE NextLine(1) IS
                     WHEN 'P' =>
                        CurrentEmployee := ProfessionalEmployee;
                        DataFeed := 4;
                     WHEN 'S' =>
                        CurrentEmployee := SalesEmployee;
                        DataFeed := 6;
                     WHEN 'C' =>
                        CurrentEmployee := ClericalEmployee;
                        DataFeed := 5;
                     WHEN 'U' =>
                        CurrentEmployee := UnknownEmployee;
                        DataFeed := 3;
                     WHEN OTHERS =>
                       raise Jump;
                  END CASE;
               END IF;

               DataCheck := DataFeed;

               WHILE DataFeed > 0 LOOP
                  Ada.Text_IO.Get_Line (File => InData, Item => NextLine, Last => LineLength);

                  IF Ada.Characters.Handling.is_digit(NextLine(1)) THEN
                     IF DataFeed <= (DataCheck - 2) THEN
                        CASE CurrentEmployee.PayStatus IS
                           WHEN Professional =>
                              CurrentEmployee.MonthlySalary := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                           WHEN Sales =>
                              CurrentEmployee.WeeksSalary := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                              CurrentEmployee.CommRate := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                              CurrentEmployee.SalesAmount := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                           WHEN Clerical =>
                              CurrentEmployee.HourlyWage := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                              CurrentEmployee.Hours := Float(GetNumber(From => NextLine));
                              DataFeed := DataFeed - 1;
                           WHEN OTHERS =>
                              NULL;
                        END CASE;
                     END IF;

                     IF LineLength = 4 THEN
                        Ada.Text_IO.Put (Item => NextLine(1..NextLine'Last));
                        Ada.Text_IO.Put (Item => NextLine(1..LineLength));
                        CurrentEmployee.ID := GetNumber(From => NextLine);
                        DataFeed := DataFeed - 1;
                     ELSIF LineLength = 2 THEN
                        CurrentEmployee.NameLength := GetNumber(From => NextLine);
                        DataFeed := DataFeed - 1;
                     END IF;
                  ELSIF Ada.Characters.Handling.is_letter(NextLine(1)) THEN
                     IF LineLength = CurrentEmployee.NameLength THEN
                        FOR Here IN (LineLength + 1)..NameRange'Last LOOP
                           NextLine(Here) := ' ';
                        END LOOP;
                        CurrentEmployee.Name := NextLine(1..NameRange'Last);
                        DataFeed := DataFeed - 1;
                     ELSE
                        raise Jump;
                     END IF;
                  ELSE
                     raise Jump;
                  END IF;
                  Marker := Marker + 1;
                  row := ((Marker - 1) / ArraySize'Last) + 1;
                  col := ArraySize'Last - ((Marker - 1) REM ArraySize'Last);
                  Matrix(row, col) := CurrentEmployee;
               END LOOP;
            END LOOP;

            EXIT;
         EXCEPTION
            WHEN Jump =>
               Ada.Text_IO.Skip_Line(File => InData);
         END;
      END LOOP;

      Ada.Text_IO.Close (File => InData);
   END Input;

   PROCEDURE Output (From: IN EmployeeArray) IS

   BEGIN --Output
      FOR col IN ArraySize LOOP
         FOR row IN ArraySize LOOP
            CurrentEmployee := From(row, (ArraySize'Last - col) + 1);

            Ada.Text_IO.Put (Item => "Employee Number: ");
            Ada.Integer_Text_IO.Put (Item => CurrentEmployee.ID, Width => 1);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put (Item => "Name Length: ");
            Ada.Integer_Text_IO.Put (Item => CurrentEmployee.NameLength, Width => 1);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put (Item => "Employee: ");
            Ada.Text_IO.Put (Item => CurrentEmployee.Name);
            Ada.Text_IO.New_Line;

            CASE CurrentEmployee.PayStatus IS
               WHEN Professional =>
                  Ada.Text_IO.Put (Item => "Monthly Salary: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.MonthlySalary, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN Sales =>
                  Ada.Text_IO.Put (Item => "Weekly Salary: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.WeeksSalary, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Commission Rate: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.CommRate, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Sales Amount: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.SalesAmount, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN Clerical =>
                  Ada.Text_IO.Put (Item => "Hourly Wage: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.HourlyWage, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put (Item => "Hours Worked: ");
                  Ada.Float_Text_IO.Put (Item => CurrentEmployee.Hours, Fore => 1, Aft => 2, Exp => 0);
                  Ada.Text_IO.New_Line;
               WHEN OTHERS =>
                  NULL;
            END CASE;
            Ada.Text_IO.Put (Item => "-------------------");
            Ada.Text_IO.New_Line;
         END LOOP;
      END LOOP;
   END Output;

   Farewell : String := "That's it, That's all!";
BEGIN -- Lab1
   Input(DataFrom => InputFile, Matrix => Employed);
   Output(From => Employed);

   FOR x IN 1..Farewell'Last LOOP
      DELAY 0.2;
      Ada.Text_IO.Put (Item => Farewell(x));
   END LOOP;
   Ada.Text_IO.New_Line;
END Lab1;
