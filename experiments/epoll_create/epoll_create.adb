
with Ada.Text_IO;
with Interfaces.C;
with Epoll;

use Ada.Text_IO;
use Interfaces;

procedure Epoll_Create is
    Max_Size : C.int := 10;
begin
    Put_Line (">> Start");

    declare
        use Interfaces.C;
        RC : C.int := Epoll.Epoll_Create (Max_Size);
    begin
        Put_Line ("Epoll_Create returned: " & C.int'Image (RC));
        if RC = -1 then
            Put_Line (".. it appears that Epoll_Create encountered an error");
        end if;
    end;
end Epoll_Create;
