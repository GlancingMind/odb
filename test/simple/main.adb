with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Command_Line;			use Ada.Command_Line;
with ODB.Persistent;			use ODB.Persistent;
with ODB.Storage.File;			use ODB.Storage.File;
with ODB.Collection;			use ODB.Collection;
use  ODB.Storage;
use  ODB;

with Person;


procedure Main is 
   T    : Reference := Null;
   Dep  : Reference := null;
   FS   : File.Object;

   -- fetch the named department
   function Department(
      Name  : in String ) return Reference is 
      QName : constant String := "Department " & Name;
      R     : Reference := Lookup_Object( QName );
   begin
      if R = null then
         R := Collection.Create( QName );
      end if;

      return R;
   end Department;

   -- display the department 
   procedure Display_Department(
      Dep    : in Reference ) is
      Member : Element_Array_Access := Collection.Contents(Dep); 
   begin
      for i in Member'Range loop
         Person.Display( Member(i) );
      end loop;
   end Display_Department;

   -- add a person to the department
   procedure Add_Person(
      Dep   : in Reference;
      Name : in String;
      Street : in String ) is
      R      : Reference := Lookup_Object( Name );
   begin
      if R = null then
         R := Person.Create( Name, Street );
      end if;

      Add( Dep, R );
      Person.Increment_Used( R );
   end Add_Person;
      
begin
   if Argument_Count < 1 then
      Put_Line("usage:" );
      Put_Line("   main departementname ");
      Put_Line("");
      return;
   end if;

   File.Pool_Path( FS, "./data/");
   File.Index_Path( FS, "./data.idx" );

   File.Load(FS);

   Dep := Department( Argument(1) );

   Add_Person( Dep, "Michael Erdmann" , "Torellstrasse 2" );
   Add_Person( Dep, "Marina Massalski" , "Torellstrasse 2" );
   Add_Person( Dep, "Margerete Massalski" , "Wotanstrasse 2");
    
   Put_Line("Nbr of Objects at write time :" & 
        Natural'Image( Persistent.Nbr_of_Objects ) );

   Display_Department( Dep );

   File.Save(FS);
end Main;

