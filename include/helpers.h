
#ifndef SCOTT_HELPERS__
#define SCOTT_HELPERS__

#ifdef __APPLE__

// Apple/Darwin specific code

#include <stdlib.h>

// A subset of if_data, as defined in <net/if_var.h>
struct scott_if_data {
  u_int32_t   scott_ifi_ipackets;       /* packets received on interface */
  u_int32_t   scott_ifi_opackets;       /* packets sent on interface */
  u_int32_t   scott_ifi_ierrors;        /* input errors on interface */
  u_int32_t   scott_ifi_oerrors;        /* output errors on interface */
  u_int32_t   scott_ifi_ibytes;         /* total number of octets received */
  u_int32_t   scott_ifi_obytes;         /* total number of octets sent */
  u_int32_t   scott_ifi_imcasts;        /* packets received via multicast */
  u_int32_t   scott_ifi_omcasts;        /* packets sent via multicast */
  u_int32_t   scott_ifi_iqdrops;        /* dropped on input, this interface */
  char*       scott_ifi_name;           /* The interface name */
  int         scott_ifi_error;          /* If there was an error */
};

typedef struct scott_if_data scott_if_data_t;

void free_scott_if_data(scott_if_data_t*, int);

// End of Apple/Darwin specific code

#endif

#endif
