with Ada.Text_IO;			use Ada.Text_IO;
with ODB.Persistent;			use ODB.Persistent;
with ODB.Storage.File;			use ODB.Storage.File;
use  ODB.Storage;
use  ODB;

with Customer;				use Customer;

procedure Main is 
   T : Reference := Null;
   File_Store : File.Object;
begin
   File.Pool_Path( File_Store, "./data/");
   File.Index_Path( File_Store, "./data.idx" );

   File.Load(File_Store);

   T := Customer.Create( "Michael_Erdmann", "Torellstrasse 2", 1002 );

   Display(T);

   Put_Line("Saving context");
   File.Save(File_Store);
end Main;

