--
--  Main interface for dealing with epoll(7) from Ada
--

with Interfaces.C;
with System;

use Interfaces;

package Epoll is
    type Epoll_Data_Type is (Pointer,
                            File_Descriptor,
                            Int_32,
                            Int_64);

    type Epoll_Data (D : Epoll_Data_Type := File_Descriptor) is record
        case D is
            when Pointer =>
                Ptr : System.Address;
            when File_Descriptor =>
                Fd : C.int;
            when Int_32 =>
                u32 : C.int;
            when Int_64 =>
                u64 : C.int;
        end case;
    end record;

    pragma Unchecked_Union (Epoll_Data);

    type Epoll_Event is record
        Events : Integer;
        Data : Epoll_Data;
    end record;


    function Epoll_Create (Size : C.int) return C.int;

    function Epoll_Wait (Epoll_Descriptor : C.int;
                            Events : System.Address;
                            Max_Events : C.int;
                            Timeout : C.int) return C.int;
    private
        pragma Import (C, epoll_create);
        pragma Import (C, epoll_wait);
end Epoll;
