
with Interfaces.C,
     System;

package Async.Raw.Epoll is

    subtype Epoll_Fd_Type is Integer;

    subtype Epoll_Events_Type is Interfaces.Unsigned_32;
    EPOLLIN : constant Epoll_Events_Type := 1;
    EPOLLPRI : constant Epoll_Events_Type := 2;
    EPOLLOUT : constant Epoll_Events_Type := 4;
    EPOLLRDNORM : constant Epoll_Events_Type := 64;
    EPOLLRDBAND : constant Epoll_Events_Type := 128;
    EPOLLWRNORM : constant Epoll_Events_Type := 256;
    EPOLLWRBAND : constant Epoll_Events_Type := 512;
    EPOLLMSG : constant Epoll_Events_Type := 1024;
    EPOLLERR : constant Epoll_Events_Type := 8;
    EPOLLHUP : constant Epoll_Events_Type := 16;
    EPOLLRDHUP : constant Epoll_Events_Type := 8192;
    EPOLLEXCLUSIVE : constant Epoll_Events_Type := 268435456;
    EPOLLWAKEUP : constant Epoll_Events_Type := 536870912;
    EPOLLONESHOT : constant Epoll_Events_Type := 1073741824;
    EPOLLET : constant Epoll_Events_Type := 2147483648;

    type Epoll_Ctl_Type is (Epoll_Ctl_Add,
                            Epoll_Ctl_Del,
                            Epoll_Ctl_Mod);
    for Epoll_Ctl_Type use (Epoll_Ctl_Add => 1,
                             Epoll_Ctl_Del => 2,
                             Epoll_Ctl_Mod => 3);

    type Data_Type (Discriminant : Interfaces.C.unsigned := 0) is record
        case Discriminant is
            when 0 =>
                Ptr : System.Address;
            when 1 =>
                FD  : Epoll_Fd_Type;
            when 2 =>
                U32 : Interfaces.Unsigned_32;
            when others =>
                U64 : Interfaces.Unsigned_64;
        end case;
   end record
     with Unchecked_Union,
     Convention => C;


   type Event_Type is record
      Events : Interfaces.Unsigned_32;
      Data   : Data_Type;
   end record
     with Convention => C,
       Pack;

   type Event_Array_Type is array (Integer range <>) of aliased Event_Type
     with Convention => C;

   function Create (Size : Integer) return Epoll_Fd_Type
     with Import,
     Link_Name => "epoll_create",
     Convention => C;

    function Control (Epfd : Epoll_Fd_Type;
                      Op : Epoll_Ctl_Type;
                      Fd : Epoll_Fd_Type;
                      Events : access Event_Type) return Integer
     with Import,
     Link_Name => "epoll_ctl",
     Convention => C;

    function Wait (Epfd : Epoll_Fd_Type;
                   Events : Event_Array_Type;
                   Max_Events : Integer;
                   Timeout    : Integer) return Integer
     with Import,
     Link_Name => "epoll_wait",
       Convention => C;

end Async.Raw.Epoll;
