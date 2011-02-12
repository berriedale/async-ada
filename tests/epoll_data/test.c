

#include <sys/epoll.h>

epoll_data_t fd_of_one() {
    epoll_data_t rc;
    rc.fd = 1;
    return rc;
}

epoll_data_t u32_of_two() {
    epoll_data_t rc;
    rc.u32 = 2;
    return rc;
}

epoll_data_t u64_of_lots() {
    epoll_data_t rc;
    rc.u64 = 23; /* LOTS! */
    return rc;
}

