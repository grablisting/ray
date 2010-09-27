with Ada.Real_Time;
use Ada.Real_Time;

PACKAGE BODY RecordTime IS
   FUNCTION RecordTime (Parameter1 : pMyData; Parameter2 : Integer) RETURN Duration IS
      Tick, Tock : Time;
   BEGIN --RecordTime
      Tick := Clock;
      SortProcedure(Parameter1, Parameter2);
      Tock := Clock;
      RETURN To_Duration(Tock - Tick);
   END RecordTime;
END RecordTime;

