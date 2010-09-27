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
   UnknownEmployee : Employee(Unknown);
   CurrentEmployee : Employee;

   SUBTYPE ArraySize IS Natural RANGE 1..5;
   TYPE EmployeeArray IS ARRAY (ArraySize, ArraySize) OF Employee;
   Employed : EmployeeArray := (OTHERS => (OTHERS => CurrentEmployee));

   FUNCTION GetNumber (From: Ada.Text_IO.File_Type) RETURN Integer IS
      Num0, Num1 : Integer := 0;
      NextCh : Character;
   BEGIN
      WHILE NOT Ada.Text_IO.End_of_Line (File => From) LOOP
         Ada.Text_IO.Get (File => From, Item => NextCh);
         Num1 := Character'Pos(NextCh) - Character'Pos('0');
         Num0 := (Num0 * 10) + Num1;
      END LOOP;
      Ada.Text_IO.Skip_Line (File => From);
      RETURN Num0;
   END GetNumber;

   PROCEDURE Input (DataFrom: IN String; Matrix: OUT EmployeeArray) IS
      NextCh : Character;
      Num0, Marker, row, col : Integer := 0;
      InData : Ada.Text_IO.File_Type;
      TempName : NameType;
   BEGIN
      Ada.Text_IO.Open (File => InData, Mode => Ada.Text_IO.In_File, Name => DataFrom);
      WHILE NOT Ada.Text_IO.End_of_File (File => InData) LOOP
         Ada.Text_IO.Get (File => InData, Item => NextCh);
         Ada.Text_IO.Skip_Line (File => InData);

         CASE NextCh IS
            WHEN 'P' =>
               CurrentEmployee := ProfessionalEmployee;
            WHEN 'S' =>
               CurrentEmployee := SalesEmployee;
            WHEN 'C' =>
               CurrentEmployee := ClericalEmployee;
            WHEN 'U' =>
               CurrentEmployee := UnknownEmployee;
         END CASE;

         CurrentEmployee.ID := GetNumber(From => InData);
         CurrentEmployee.NameLength := GetNumber(From => InData);

         Ada.Text_IO.Get_Line (File => InData, Item => TempName, Last => Num0);
         FOR Here IN (Nom0)..NameRange'Last LOOP
            NextLine(Here) := ' ';
         END LOOP;
         CurrentEmployee.Name := NextLine;
         Ada.Text_IO.Skip_Line (File => InData);

         CASE CurrentEmployee.PayStatus IS
            WHEN Professional =>
               CurrentEmployee.MonthlySalary := Float(GetNumber(From => InData));
            WHEN Sales =>
               CurrentEmployee.WeeksSalary := Float(GetNumber(From => InData));
               CurrentEmployee.CommRate := Float(GetNumber(From => InData));
               CurrentEmployee.SalesAmount := Float(GetNumber(From => InData));
            WHEN Clerical =>
               CurrentEmployee.HourlyWage := Float(GetNumber(From => InData));
               CurrentEmployee.Hours := Float(GetNumber(From => InData));
            WHEN OTHERS =>
               NULL;
         END CASE;

         Marker := Marker + 1;
         row := ((Marker - 1) / ArraySize'Last) + 1;
         col := ArraySize'Last - ((Marker - 1) REM ArraySize'Last);
         Matrix(row, col) := CurrentEmployee;
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
