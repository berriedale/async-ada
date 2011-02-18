
with System;
with Ada.Text_IO;
with Interfaces.C;
with Async.Epoll;

use Ada.Text_IO;
use System;

procedure Async.Epoll.Test is
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
    function U32_Of_Two return Epoll_Data;
    function U64_Of_Lots return Epoll_Data;
    function Bad_Pointer return Epoll_Data;

    pragma Import (C, Fd_Of_One, "fd_of_one");
    pragma Import (C, U32_Of_Two, "u32_of_two");
    pragma Import (C, U64_Of_Lots, "u64_of_lots");
    pragma Import (C, Bad_Pointer, "badpointer");
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

    Start ("U32_Test");
    U32_Test:
        declare
            Data : Epoll_Data := U32_Of_Two;
        begin
            if Data.u32 /= 2 then
                Failed;
            end if;
        end U32_Test;
    Passed;

    Start ("U64_Test");
    U64_Test:
        declare
            Data : Epoll_Data := U64_Of_Lots;
        begin
            if Data.u64 /= 23 then
                Failed;
            end if;
        end U64_Test;
    Passed;
end Async.Epoll.Test;
