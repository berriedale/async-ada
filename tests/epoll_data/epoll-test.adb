
with Ada.Text_IO;
with Interfaces.C;
with Epoll;

use Ada.Text_IO;

procedure Epoll.Test is
    Test_Failed : exception;

    procedure Start (Name : in String) is
    begin
        Put (">> Starting test: " & Name);
    end Start;

    procedure Failed is
    begin
        raise Test_Failed;
    end Failed;

    procedure Passed is
    begin
        Put_Line (" ..Passed");
    end Passed;

    function Fd_Of_One return Epoll_Data;
    pragma Import (C, Fd_Of_One, "fd_of_one");
begin
    Put_Line(">>> Starting tests of epoll_data");
    New_Line;

    Start ("Fd_Test");
    Fd_Test:
        declare
            Data : Epoll_Data := Fd_Of_One;
        begin
            if Data.Fd /= 1 then
                Failed;
            end if;
        end Fd_Test;
    Passed;

end Epoll.Test;
