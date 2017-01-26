#include "helpers.h"

#ifdef __APPLE__

// Free our scott_if_data data structure
void free_scott_if_data(scott_if_data_t* interfaces, int interfacesNum) {
  for (int i = 0; i < interfacesNum; i++) {
    free(interfaces[i].scott_ifi_name);
  }

  // Finally, deallocate the outermost array
  free(interfaces);
}

#endif
