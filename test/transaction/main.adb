with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Command_Line;			use Ada.Command_Line;
with ODB.Persistent;			use ODB.Persistent;
with ODB.Storage.File;			use ODB.Storage.File;
use  ODB.Storage;

with ODB.Transaction;			use ODB.Transaction;
use  ODB;

with Person;

procedure Main is 
   P    : Reference := Null;
   FS   : File.Object;
   T    : Transaction.Object;

   -- add a person to the department
   function New_Person(
      Name   : in String;
      Street : in String ) return Reference is
      R      : Reference := Lookup_Object( Name );
   begin
      if R = null then
         R := Person.Create( Name, Street );
      end if;

      return R;
   end New_Person;
      
begin
   -- load persistent objects
   File.Pool_Path( FS, "./data/");
   File.Index_Path( FS, "./data.idx" );
   File.Load(FS);
   -- initialize the transaction manager to handle 200 transactions
   Transaction.Initialize( 200 );

   declare
      P : Reference := New_Person( "Michael Erdmann", "Torellstrasse 2" );
   begin
      Start( T, P );

      Put_Line("===================== Modified data");
      Person.Street( P, "Nowhere" );
      Person.Display( P );

      Rollback(T);

      Put_Line("===================== Data after rollback");
      Person.Display( P );

      Cancel(T);
   exception
      when Others =>
	 Cancel(T);
   end ;

   Transaction.Finalize;
   File.Save(FS);
end Main;

