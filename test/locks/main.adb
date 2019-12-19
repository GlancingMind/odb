with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Command_Line;			use Ada.Command_Line;


with Util.Lock_Table;			use Util.Lock_Table;
use  Util;

procedure Main is 
   LT : Lock_Table.Object(200);

   task type Consumer_Task( Id : Natural ) ;

   task body Consumer_Task is 
      H  : Lock_Handle_Type := Seize( LT, 10 );
   begin
      Put_Line(Natural'Image(id) & " using resource" );

      delay 5.0;
      Release( LT, H );
   end Consumer_Task;

   type Consumer_Access is access Consumer_Task;

begin

   for i in 1..4 loop
      declare
        P : Consumer_Access ;
      begin
        P := new Consumer_Task(i);
      end ;
   end loop;


end Main;

