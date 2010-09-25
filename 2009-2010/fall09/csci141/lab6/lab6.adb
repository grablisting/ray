with Ada.Text_IO;
with Ada.Integer_Text_IO;

PROCEDURE Lab6 IS
   -- Lab6 creates a quiz from questions and answers
   -- acquired from an external text file.
   -- Written by Ray Peters on November 4, 2009.

   -- Master assignment of the number of questions
   NumberOfQuestions : CONSTANT Integer := 10;

   -- Set the ranges for the Arrays, dependent on the number of questions
   SUBTYPE QuestionIndex IS Integer RANGE 0..(NumberOfQuestions * 6);
   SUBTYPE AnswerIndex IS Integer RANGE 0..NumberOfQuestions;

   -- Define what MyQuizArray should expect to find in the QA.txt file
   SUBTYPE QAStringType IS String(1..40);
   QAString : QAStringType;

   -- Acceptable range for MyAnswerArray and User Input
   SUBTYPE AnswerType IS Character RANGE 'a'..'d';
   Answers, Guess : AnswerType;

   -- Identify both Array types, one for Questions and one for Answers
   TYPE MyQuizArrayType IS ARRAY (QuestionIndex) OF QAStringType;
   MyQuizArray : MyQuizArrayType;

   TYPE AnswerArrayType IS ARRAY (AnswerIndex) OF AnswerType;
   MyAnswerArray : AnswerArrayType;

   -- Identify Variables and starting positions
   QA : Ada.Text_IO.File_Type;
   TotalCorrect, StartAt : Integer := 0;
   EndAt : Integer := 4;

BEGIN -- Lab6
   -- Open Questions and Answers file
   Ada.Text_IO.Open(File => QA,
                    Name => "qa.txt",
                    Mode => Ada.Text_IO.In_File);

   -- User Greeting
   Ada.Text_IO.Put (Item => "Welcome to the MARIO KART & LINK'S AWAKING QUIZ!!");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "GOOD LUCK!!!");
   Ada.Text_IO.New_Line;

   -- Loops through QA File, acquiring Questions and Answers
   FOR x IN 0..Integer'Last LOOP
      EXIT WHEN Ada.Text_IO.End_Of_File(QA); -- exit at EOF
      -- Loops 5 times to get the Questions for MyQuizArray
      FOR y IN StartAt..EndAt LOOP
         Ada.Text_IO.Get (Item => QAString, File => QA);
         MyQuizArray(y) := QAString;
      END LOOP;
      -- Then grabs the single character Answer for MyAnswerArray
      Ada.Text_IO.Get (Item => Answers, File => QA);
      MyAnswerArray(x) := Answers;

      -- Moves Marker for starting and ending positions
      StartAt := StartAt + 6;
      EndAt := EndAt + 6;
   END LOOP;

   -- Loop until there are no more questions. Also accounts for
   -- starting at 0 by subtracting 1 from the NumberOfQuestions.
   FOR i IN 0..(NumberOfQuestions - 1) LOOP
      -- Aesthetic Break
      Ada.Text_IO.Put (Item => "----------------------------------------");
      Ada.Text_IO.New_Line;

      -- Tell the user which question they are at
      Ada.Text_IO.Put (Item => "QUESTION #");
      Ada.Integer_Text_IO.Put (Item => (i + 1), Width => 1);
      Ada.Text_IO.Put (Item => ":");
      Ada.Text_IO.New_Line;

      -- Loop statement incorporates the relevant QAString range
      -- for MyQuizArray. It is based from the value of i when
      -- starting the loop.
      FOR NextQuestion IN (i * 6)..(i * 6 + 4) LOOP
         Ada.Text_IO.Put (Item => MyQuizArray(NextQuestion));
         Ada.Text_IO.New_Line;
      END LOOP;

      LOOP
         BEGIN -- Repeat question until user inputs a value within acceptable range
            Ada.Text_IO.Put (Item => "--- Please enter your answer: ");
            Ada.Text_IO.Get (Item => Guess);
            EXIT; -- Exit upon successful input
         EXCEPTION -- Checking for bad user input
            WHEN Constraint_Error =>
               -- Tell the user they have been bad
               Ada.Text_IO.Put (Item => "Typo! Please pick an available answer.");
               Ada.Text_IO.New_Line;
         END;
      END LOOP;

      -- Compare User's Guess to the actual Answer in MyAnswerArray
      IF Guess = MyAnswerArray(i) THEN
         -- Relay a happy message upon correct answer and
         -- account for it in TotalCorrect variable
         Ada.Text_IO.Put (Item => "--- CORRECT! NICE!");
         Ada.Text_IO.New_Line;
         TotalCorrect := TotalCorrect + 1;
      ELSE
         -- Relay a sad message upon a wrong answer
         Ada.Text_IO.Put (Item => "--- SORRY, WRONG ANSWER :(");
         Ada.Text_IO.New_Line;
      END IF;
   END LOOP;

   -- Aesthetic break
   Ada.Text_IO.Put (Item => "----------------------------------------");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "++++++++++++++++++++++++++++++++++++++++");
   Ada.Text_IO.New_Line;

   -- Farewell
   Ada.Text_IO.Put (Item => "Thanks for taking the MARIO KART AND LINK'S AWAKING!! quiz.");
   Ada.Text_IO.New_Line;
   -- Report User's Score
   Ada.Text_IO.Put (Item => "You scored: ");
   Ada.Integer_Text_IO.Put (Item => TotalCorrect, Width => 1);
   Ada.Text_IO.Put (Item => " out of ");
   Ada.Integer_Text_IO.Put (Item => NumberOfQuestions, Width => 1);
   Ada.Text_IO.Put (Item => ". (");
   -- Calculate score as a Percentage
   Ada.Integer_Text_IO.Put (Item => (TotalCorrect * 100) / (NumberOfQuestions), Width => 1);
   Ada.Text_IO.Put (Item => "%)");

   -- Aesthetic Break
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "++++++++++++++++++++++++++++++++++++++++");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "----------------------------------------");
   Ada.Text_IO.New_Line;

   -- Close input and output files
   Ada.Text_IO.Close(File => QA);

END Lab6;
